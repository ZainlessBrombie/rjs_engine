/*pub trait JsValueTrait {
    fn to_native_string(&self) -> String;

    fn call(&self) -> dyn JsValue;

    fn lookup(&self, key: &dyn JsValue) -> dyn JsValue;

    fn type_of(&self) -> dyn JsValue;
}*/

use crate::js::data::gc_util::GcDestr;
use crate::js::data::js_execution::{FnOpRepr, JsVar, NativeFunction, VarAlloc};
use crate::js::data::util::{s_pool, u_undefined};
use gc::{BorrowMutError, Finalize, Gc, GcCell, GcCellRefMut, Trace};
use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use std::pin::Pin;
use std::rc::Rc;

#[derive(Trace, Finalize)]
pub struct JsProperty {
    pub enumerable: bool,
    pub configurable: bool,
    pub writable: bool,
    pub value: JsValue,
}

pub enum ExecResult {
    Done,
    Errored,
    Stepped(Box<dyn JsNext>),
}

pub trait JsNext {
    fn step(self, n: u64) -> ExecResult;
}

pub fn next_done() -> Box<dyn JsNext> {
    struct DoneNext {}
    impl JsNext for DoneNext {
        fn step(self, _n: u64) -> ExecResult {
            ExecResult::Done
        }
    }
    Box::new(DoneNext {})
}

pub fn next_err() -> Box<dyn JsNext> {
    struct ErrNext {}
    impl JsNext for ErrNext {
        fn step(self, _n: u64) -> ExecResult {
            ExecResult::Errored
        }
    }
    Box::new(ErrNext {})
}

#[derive(Trace, Finalize, Clone)]
pub struct JsFn {
    pub(crate) ops: Rc<FnOpRepr>,
    pub(crate) captures: Rc<Vec<JsVar>>,
}

impl JsFn {}

#[derive(Trace, Finalize, Clone)]
pub enum JSCallable {
    NotCallable,
    Js {
        content: Rc<String>,
        creator: Gc<JsFn>,
    },
    Native {
        op: Rc<dyn NativeFunction>,
    },
}

#[derive(Trace, Finalize, Clone)]
pub struct Identity(Rc<u64>); // Note: Name ist stored in object

impl Identity {
    pub fn new() -> Identity {
        return Identity(Rc::new(rand::random()));
    }
}

impl Hash for Identity {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(*Rc::deref(&self.0))
    }
}

impl PartialEq for Identity {
    fn eq(&self, other: &Self) -> bool {
        let us_loc: &u64 = Rc::deref(&self.0);
        let other_loc: &u64 = Rc::as_ref(&other.0);

        return (us_loc as *const u64) == (other_loc as *const u64);
    }
}

impl Eq for Identity {}

#[derive(Trace, Finalize, Clone)]
pub enum JsValue {
    Undefined,
    Null,
    Number(f64),
    Boolean(bool),
    String(Rc<String>),
    Object(Gc<GcCell<JsObj>>),
}

impl JsValue {
    pub fn is_symbol(&self) -> bool {
        match self {
            JsValue::Object(o) => o.borrow().is_symbol,
            _ => false,
        }
    }
}

#[derive(Trace, Finalize)]
pub struct InsertOrderMap<K: Hash + Eq, V> {
    counter: u64,
    map: HashMap<K, (u64, V)>,
}

impl<K: Hash + Eq, V> Default for InsertOrderMap<K, V> {
    fn default() -> Self {
        InsertOrderMap {
            counter: 0,
            map: Default::default(),
        }
    }
}

impl<K: Hash + Eq, V> InsertOrderMap<K, V> {
    pub fn new() -> InsertOrderMap<K, V> {
        InsertOrderMap {
            counter: 0,
            map: Default::default(),
        }
    }

    pub fn insert(&mut self, k: K, v: V) {
        self.map.insert(k, (self.counter, v));
        self.counter = self.counter + 1;
    }

    pub fn get(&self, k: &K) -> Option<&V> {
        self.map.get(k).map(|v| &v.1)
    }

    pub fn get_mut(&mut self, k: &K) -> Option<&mut V> {
        self.map.get_mut(k).map(|v| &mut v.1)
    }

    pub fn get_values_ordered(&self) -> Vec<(&K, &V)> {
        let mut ret: Vec<(u64, &K, &V)> = self.map.iter().map(|(k, v)| (v.0, k, &v.1)).collect();
        ret.sort_by_key(|tup| tup.0);
        return ret.iter().map(|tup| (tup.1, tup.2)).collect();
    }
}

#[derive(Trace, Finalize)]
pub struct JsObj {
    pub(crate) is_array: bool,
    pub(crate) content: InsertOrderMap<Rc<String>, JsProperty>,
    pub(crate) symbol_keys: InsertOrderMap<JsValue, JsProperty>,
    pub(crate) call: JSCallable,
    pub(crate) identity: Identity,
    pub(crate) is_symbol: bool,
}

impl JsObj {
    pub fn get_prop(&self, k: &Rc<String>) -> JsValue {
        self.content
            .get(k)
            .map(|prop| &prop.value)
            .unwrap_or(&JsValue::Undefined)
            .clone()
    }
}

impl Hash for JsValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            JsValue::Undefined => state.write_u8(0),
            JsValue::Null => state.write_u8(0),
            JsValue::Number(n) => unsafe { state.write_u64(std::mem::transmute(*n)) },
            JsValue::Boolean(b) => b.hash(state),
            JsValue::String(str) => str.hash(state),
            JsValue::Object(jsobj) => jsobj.borrow().identity.hash(state),
        }
    }
}

impl PartialEq for JsValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (JsValue::Undefined, JsValue::Undefined) => true,
            (JsValue::Null, JsValue::Null) => true,
            (JsValue::Number(n1), JsValue::Number(n2)) => n1 == n2,
            (JsValue::Boolean(b1), JsValue::Boolean(b2)) => b1 == b2,
            (JsValue::String(str1), JsValue::String(str2)) => str1 == str2,
            (JsValue::Object(obj1), JsValue::Object(obj2)) => {
                obj1.borrow().identity == obj2.borrow().identity
            }
            (_, _) => false,
        }
    }
}

impl Eq for JsValue {}

impl Default for JsValue {
    fn default() -> Self {
        JsValue::Undefined
    }
}

impl JsValue {
    pub fn from_string(val: &str) -> JsValue {
        return JsValue::String(Rc::new(val.into()));
    }

    /// Returns the safe, system internal representation of the string
    /// like used in templating. Safe means no execution. Strings are returned literally.
    /// Very long arrays with a lot of empties may produce a lot of commas
    pub fn to_system_string(&self) -> Rc<String> {
        match self {
            JsValue::Undefined => "undefined".into(),
            JsValue::Null => "null".into(),
            JsValue::Number(n) => n.to_string(),
            JsValue::Boolean(b) => b.to_string(),
            JsValue::String(s) => s.to_string(),
            JsValue::Object(obj) => {
                let ref_mut = GcCell::try_borrow_mut(&obj);
                let ref_mut = match ref_mut {
                    Ok(ok) => ok,
                    Err(err) => return Rc::new("".into()), // circular
                };
                if ref_mut.is_array {
                    let mut ret = Vec::new();
                    let len = ref_mut
                        .content
                        .get(&s_pool("length"))
                        .map(|prop| prop.value.clone())
                        .unwrap_or(u_undefined());
                    let len = match len {
                        JsValue::Number(n) => n,
                        _ => 0.0,
                    };
                    // TODO this may block the server
                    for i in 0..(len.floor() as usize/* TODO this can panic */) {
                        ret.push(
                            ref_mut
                                .content
                                .get(&Rc::new(i.to_string()))
                                .map(|prop| prop.value.to_system_string())
                                .unwrap_or(Rc::new("".into()))
                                .as_str()
                                .to_string(),
                        )
                    }
                    return Rc::new(ret.join(","));
                }
                "[object Object]".into()
            }
        }
        .into()
    }

    pub fn truthy(&self) -> bool {
        return match self {
            JsValue::Undefined => false,
            JsValue::Null => false,
            JsValue::Number(n) => *n != 0.0 && !n.is_nan(),
            JsValue::Boolean(b) => *b,
            JsValue::String(s) => !s.is_empty(),
            JsValue::Object { .. } => true,
        };
    }
}
