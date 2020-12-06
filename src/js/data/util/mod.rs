use crate::js::data::gc_util::GcDestr;
use crate::js::data::js_execution::{FnOpRepr, FnOpResult, JsVar, StackFrame, VarAlloc};
use crate::js::data::js_types::{Identity, JSCallable, JsFn, JsObj, JsProperty, JsValue};
use crate::js::data::EngineConstants::{ConstantStrings, EngineConstants};
use gc::{Gc, GcCell};
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::os::raw::c_void;
use std::rc::Rc;
use crate::js::data::js_code::ScopeLookup;

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

    pub fn with_prop(mut self, key: Rc<String>, value: JsValue) -> Self {
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

    pub fn with_proto(mut self, proto: JsValue) -> Self {
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

pub fn u_load_global(name: &str) -> FnOpRepr {
    return FnOpRepr::LoadGlobal {
        name: Rc::new(name.to_string()),
    };
}

pub fn u_read_var(var: VarAlloc) -> FnOpRepr {
    return FnOpRepr::ReadVar { which: var };
}

pub fn u_return(ret: FnOpRepr) -> FnOpRepr {
    FnOpRepr::Return {
        what: Rc::from(ret),
    }
}

pub fn u_write_var(var: VarAlloc, what: FnOpRepr) -> FnOpRepr {
    return FnOpRepr::Assign {
        target: var,
        what: Rc::from(what),
    };
}

pub fn u_literal(value: JsValue) -> FnOpRepr {
    return FnOpRepr::LoadStatic { value };
}

pub fn u_obj() -> JsValue {
    JsObjectBuilder::new(None).build()
}

pub fn u_this() -> FnOpRepr {
    return FnOpRepr::LoadThis {};
}

pub fn u_plus_num(left: FnOpRepr, right: FnOpRepr) -> FnOpRepr {
    return FnOpRepr::NumeralPlus {
        left: Rc::new(left),
        right: Rc::new(right),
    };
}

pub fn u_plus(left: FnOpRepr, right: FnOpRepr) -> FnOpRepr {
    return FnOpRepr::Plus {
        left: Rc::new(left),
        right: Rc::new(right),
    };
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

pub fn u_assign(
    to: FnOpRepr,
    key: FnOpRepr,
    what: FnOpRepr,
) -> FnOpRepr {
    return FnOpRepr::AssignRef {
        to: Rc::new(to),
        key: Rc::new(key),
        what: Rc::new(what),
    };
}

pub fn u_if(cond: FnOpRepr, if_b: FnOpRepr) -> FnOpRepr {
    u_if_else(cond, if_b, FnOpRepr::Nop {})
}

pub fn u_and(left: FnOpRepr, right: FnOpRepr) -> FnOpRepr {
    return FnOpRepr::BoolAnd {
        left: Rc::from(left),
        right: Rc::from(right),
    };
}

pub fn u_or(left: FnOpRepr, right: FnOpRepr) -> FnOpRepr {
    return FnOpRepr::BoolOr {
        left: Rc::from(left),
        right: Rc::from(right),
    };
}

pub fn u_strict_comp(left: FnOpRepr, right: FnOpRepr) -> FnOpRepr {
    return FnOpRepr::StrictCompare {
        left: Rc::new(left),
        right: Rc::new(right),
    };
}

pub fn u_fuzzy_comp(left: FnOpRepr, right: FnOpRepr) -> FnOpRepr {
    return FnOpRepr::FuzzyCompare {
        left: Rc::new(left),
        right: Rc::new(right),
    };
}

pub fn u_typeof(of: FnOpRepr) -> FnOpRepr {
    return FnOpRepr::TypeOf {
        of: Rc::new(of),
    };
}

pub fn u_if_else(
    cond: FnOpRepr,
    if_b: FnOpRepr,
    else_b: FnOpRepr,
) -> FnOpRepr {
    return FnOpRepr::IfElse {
        condition: Rc::from(cond),
        if_block: Rc::from(if_b),
        else_block: Rc::from(else_b),
    };
}

pub fn u_throw(what: FnOpRepr) -> FnOpRepr {
    return FnOpRepr::Throw {
        what: Rc::from(what),
    };
}

pub fn u_not(of: FnOpRepr) -> FnOpRepr {
    return FnOpRepr::BoolNot { of: Rc::new(of) };
}

pub fn u_while(condition: FnOpRepr, block: FnOpRepr) -> FnOpRepr {
    return FnOpRepr::While {
        condition: Rc::new(condition),
        block: Rc::new(block),
    };
}

pub fn u_array(mut values: Vec<FnOpRepr>) -> FnOpRepr {
    let arr = JsObjectBuilder::new(None).with_being_array().build();
    let mut ops = Vec::new();
    for (i, v) in values.iter_mut().enumerate() {
        ops.push(u_assign(
            u_literal(arr.clone()),
            u_literal(JsValue::Number(i as f64)),
            v.clone(),
        ))
    }
    ops.push(u_literal(arr.clone()));
    u_block(ops)
}

pub fn u_array_e() -> FnOpRepr {
    FnOpRepr::NewObject { is_array: true }}

/// Returns (hasNext, value)
pub fn u_it_next(scopes: &mut ScopeLookup, on: VarAlloc, it: FnOpRepr) -> (FnOpRepr, FnOpRepr) {
    
    let returned = u_cached(scopes, u_call_simple(on, it));
    return (
        u_deref(returned.1.clone(), u_literal(u_string("done"))),
        u_deref(returned.1, u_literal(u_string("value"))),
    );
}

pub fn u_cached(scopes: &mut ScopeLookup, what: FnOpRepr) -> (VarAlloc, FnOpRepr) {
    let var = GcCell::borrow_mut(scopes)
        .insert_here(Rc::new("".into()));
    let temp = GcCell::borrow_mut(scopes)
        .insert_here(Rc::new("".into()));
    (temp, u_block(vec![
        u_if(u_read_var(var.clone()), u_block(vec![
            u_write_var(temp.clone(), what),
            u_write_var(var, u_literal(u_bool(true)))
        ])),
        u_read_var(temp)
    ]))
}

pub fn u_deref(from: FnOpRepr, key: FnOpRepr) -> FnOpRepr {
    return FnOpRepr::Deref {
        from: Rc::new(from),
        key: Rc::new(key),
    };
}

pub fn u_string(s: &str) -> JsValue {
    return JsValue::String(Rc::new(s.into()));
}

pub fn u_block(b: Vec<FnOpRepr>) -> FnOpRepr {
    return FnOpRepr::Multi { block: b };
}

pub fn u_call_simple(this: VarAlloc, on: FnOpRepr) -> FnOpRepr {
    u_call(
        this,
        on,
        u_literal(JsObjectBuilder::new(None).with_being_array().build()),
    )
}

pub fn u_call(this: VarAlloc, on: FnOpRepr, args: FnOpRepr) -> FnOpRepr {
    return FnOpRepr::CallFunction {
        this,
        on: Rc::new(on),
        arg_array: Rc::from(args)
    };
}

thread_local! {
    static str_pool: RefCell<HashMap<usize, Rc<String>>> = RefCell::new(Default::default());
}

pub fn s_pool(s: &'static str) -> Rc<String> {
    str_pool.with(|map| {
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
