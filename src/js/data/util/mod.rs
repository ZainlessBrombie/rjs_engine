pub mod col_line_map;

use crate::js::data::engine_constants::ConstantStrings;
use crate::js::data::js_execution::{native_from, EngineQueuer, FnOpRepr, JsVar, VarAlloc};
use crate::js::data::js_types::{Identity, JSCallable, JsFn, JsObj, JsProperty, JsValue};
use safe_gc::{Gc, GcCell};
use std::borrow::BorrowMut;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::os::raw::c_void;
use std::rc::Rc;

pub struct JsObjectBuilder<'a> {
    obj: JsObj,
    strings: Option<&'a ConstantStrings>,
}

impl<'a> JsObjectBuilder<'a> {
    pub fn new(constants: Option<&ConstantStrings>) -> JsObjectBuilder {
        return JsObjectBuilder {
            obj: JsObj {
                is_array: false,
                content: Default::default(),
                symbol_keys: Default::default(),
                call: JSCallable::NotCallable,
                identity: Identity::new(),
                is_symbol: false,
            },
            strings: constants,
        };
    }

    pub fn with_prop(self, key: Rc<String>, value: JsValue) -> Self {
        return self.with_prop_full(key, value, true, true, true);
    }

    pub fn with_prop_full(
        mut self,
        key: Rc<String>,
        value: JsValue,
        enumerable: bool,
        configurable: bool,
        writable: bool,
    ) -> Self {
        self.obj.content.insert(
            key,
            JsProperty {
                enumerable,
                configurable,
                writable,
                value,
            },
        );
        self
    }

    pub fn with_proto(self, proto: JsValue) -> Self {
        let proto_str = self
            .strings
            .map(|s| s.proto.clone())
            .unwrap_or_else(|| Rc::new("__proto__".into()));
        return self.with_prop_full(proto_str, proto, false, false, false);
    }

    pub fn with_being_symbol(mut self) -> Self {
        self.obj.is_symbol = true;
        self
    }

    pub fn with_callable(mut self, callable: JSCallable) -> Self {
        self.obj.call = callable;
        return self;
    }

    pub fn with_being_array(mut self) -> Self {
        self.obj.is_array = true;
        return self;
    }

    pub fn with_symbol_full(
        mut self,
        key: JsValue,
        value: JsValue,
        enumerable: bool,
        configurable: bool,
        writable: bool,
    ) -> Self {
        self.obj.symbol_keys.insert(
            key,
            JsProperty {
                enumerable,
                configurable,
                writable,
                value,
            },
        );
        return self;
    }

    pub fn build(self) -> JsValue {
        return JsValue::Object(Gc::new(GcCell::new(self.obj)));
    }
}

pub fn u_null() -> JsValue {
    JsValue::Null
}

pub fn u_undefined() -> JsValue {
    JsValue::Undefined
}

pub fn u_number(n: f64) -> JsValue {
    return JsValue::Number(n);
}

pub fn u_bool(b: bool) -> JsValue {
    return JsValue::Boolean(b);
}

pub fn u_false() -> JsValue {
    u_bool(false)
}

pub fn u_true() -> JsValue {
    u_bool(true)
}

pub fn u_string(s: &str) -> JsValue {
    return JsValue::String(Rc::new(s.into()));
}

thread_local! {
    static STR_POOL: RefCell<HashMap<usize, Rc<String>>> = RefCell::new(Default::default());
}

pub fn s_pool(s: &'static str) -> Rc<String> {
    STR_POOL.with(|map| {
        match map
            .borrow_mut()
            .entry(s as *const str as *const c_void as usize)
        {
            Entry::Occupied(o) => o.get().clone(),
            Entry::Vacant(o) => {
                let ret = Rc::new(s.to_string());
                o.insert(ret.clone());
                ret
            }
        }
    })
}

pub enum VType {
    Const,
    Let,
    Var,
}
