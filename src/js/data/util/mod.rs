use crate::js::data::gc_util::GcDestr;
use crate::js::data::js_execution::{FnOpRepr, FnOpResult, JsVar, StackFrame};
use crate::js::data::js_types::{Identity, JSCallable, JsFn, JsObj, JsProperty, JsValue};
use crate::js::data::EngineConstants::{ConstantStrings, EngineConstants};
use gc::{Gc, GcCell};
use std::cell::RefCell;
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

pub fn u_load_global(name: &str) -> GcDestr<FnOpRepr> {
    return GcDestr::new(FnOpRepr::LoadGlobal {
        name: Rc::new(name.to_string()),
    });
}

pub fn u_read_var(var: JsVar) -> GcDestr<FnOpRepr> {
    return GcDestr::new(FnOpRepr::ReadVar { which: var });
}

pub fn u_return(ret: GcDestr<FnOpRepr>) -> GcDestr<FnOpRepr> {
    GcDestr::new(FnOpRepr::Return {
        what: Box::from(ret),
    })
}

pub fn u_write_var(var: JsVar, what: GcDestr<FnOpRepr>) -> GcDestr<FnOpRepr> {
    return GcDestr::new(FnOpRepr::Assign {
        target: var,
        what: Box::from(what),
    });
}

pub fn u_literal(value: JsValue) -> GcDestr<FnOpRepr> {
    return GcDestr::new(FnOpRepr::LoadStatic { value });
}

pub fn u_obj() -> JsValue {
    JsObjectBuilder::new(None).build()
}

pub fn u_this() -> GcDestr<FnOpRepr> {
    return GcDestr::new(FnOpRepr::LoadThis {});
}

pub fn u_plus_num(left: GcDestr<FnOpRepr>, right: GcDestr<FnOpRepr>) -> GcDestr<FnOpRepr> {
    return GcDestr::new(FnOpRepr::NumeralPlus {
        left: Box::new(left),
        right: Box::new(right),
    });
}

pub fn u_plus(left: GcDestr<FnOpRepr>, right: GcDestr<FnOpRepr>) -> GcDestr<FnOpRepr> {
    return GcDestr::new(FnOpRepr::Plus {
        left: Box::new(left),
        right: Box::new(right),
    });
}
/*
pub fn u_capture_name(mut of: GcDestr<FnOp>) -> (GcDestr<FnOp>, GcDestr<FnOp>) {
    let done = JsVar::new_t();
    let name = JsVar::new_t();
    let ret_val = JsVar::new_t();
    let init = u_cached(u_if(
        u_not(u_read_var(done.clone())),
        u_block(vec![u_write_var(
            ret_val.clone(),
            GcDestr::new(FnOp::CaptureName {
                into: name.clone(),
                what: Box::from(of.destroy_move()),
            }),
        )]),
    ));
    return (
        u_block(vec![init(), u_read_var(name)]),
        u_block(vec![init(), u_read_var(ret_val)]),
    );
}*/

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
    to: GcDestr<FnOpRepr>,
    key: GcDestr<FnOpRepr>,
    what: GcDestr<FnOpRepr>,
) -> GcDestr<FnOpRepr> {
    return GcDestr::new(FnOpRepr::AssignRef {
        to: Box::new(to),
        key: Box::new(key),
        what: Box::new(what),
    });
}

pub fn u_if(cond: GcDestr<FnOpRepr>, if_b: GcDestr<FnOpRepr>) -> GcDestr<FnOpRepr> {
    u_if_else(cond, if_b, GcDestr::new(FnOpRepr::Nop {}))
}

pub fn u_and(left: GcDestr<FnOpRepr>, right: GcDestr<FnOpRepr>) -> GcDestr<FnOpRepr> {
    return GcDestr::new(FnOpRepr::BoolAnd {
        left: Box::from(left),
        right: Box::from(right),
    });
}

pub fn u_or(left: GcDestr<FnOpRepr>, right: GcDestr<FnOpRepr>) -> GcDestr<FnOpRepr> {
    return GcDestr::new(FnOpRepr::BoolOr {
        left: Box::from(left),
        right: Box::from(right),
    });
}

pub fn u_strict_comp(left: GcDestr<FnOpRepr>, right: GcDestr<FnOpRepr>) -> GcDestr<FnOpRepr> {
    return GcDestr::new(FnOpRepr::StrictCompare {
        left: Box::new(left),
        right: Box::new(right),
        l_store: JsVar::new(Rc::new("#temp#".into())),
        r_store: JsVar::new(Rc::new("#temp#".into())),
        done: JsVar::new(Rc::new("#temp#".into())),
    });
}

pub fn u_fuzzy_comp(left: GcDestr<FnOpRepr>, right: GcDestr<FnOpRepr>) -> GcDestr<FnOpRepr> {
    return GcDestr::new(FnOpRepr::FuzzyCompare {
        left: Box::new(left),
        right: Box::new(right),
        l_store: JsVar::new(Rc::new("#temp#".into())),
        r_store: JsVar::new(Rc::new("#temp#".into())),
        done: JsVar::new(Rc::new("#temp#".into())),
    });
}

pub fn u_typeof(of: GcDestr<FnOpRepr>) -> GcDestr<FnOpRepr> {
    return GcDestr::new(FnOpRepr::TypeOf {
        of: Box::new(of),
        result: JsVar::new(Rc::new("#temp#".into())),
        done: JsVar::new(Rc::new("#temp#".into())),
    });
}

pub fn u_if_else(
    cond: GcDestr<FnOpRepr>,
    if_b: GcDestr<FnOpRepr>,
    else_b: GcDestr<FnOpRepr>,
) -> GcDestr<FnOpRepr> {
    return GcDestr::new(FnOpRepr::IfElse {
        condition: Box::from(cond),
        if_block: Box::from(if_b),
        else_block: Box::from(else_b),
    });
}

pub fn u_throw(what: GcDestr<FnOpRepr>) -> GcDestr<FnOpRepr> {
    return GcDestr::new(FnOpRepr::Throw {
        what: Box::from(what),
    });
}

pub fn u_not(of: GcDestr<FnOpRepr>) -> GcDestr<FnOpRepr> {
    return GcDestr::new(FnOpRepr::BoolNot { of: Box::new(of) });
}

pub fn u_while(condition: GcDestr<FnOpRepr>, block: GcDestr<FnOpRepr>) -> GcDestr<FnOpRepr> {
    return GcDestr::new(FnOpRepr::While {
        condition: Box::new(condition),
        block: Box::new(block),
    });
}

pub fn u_cached(mut what: GcDestr<FnOpRepr>) -> impl FnMut() -> GcDestr<FnOpRepr> {
    let temp = JsVar::new_t();
    let done = JsVar::new_t();
    return move || {
        u_block(vec![
            u_if(
                u_not(u_read_var(done.clone())),
                u_block(vec![
                    u_write_var(temp.clone(), what.destroy_move()),
                    u_write_var(done.clone(), u_literal(JsValue::Boolean(true))),
                ]),
            ),
            u_read_var(temp.clone()),
        ])
    };
}

pub fn u_array(mut values: Vec<GcDestr<FnOpRepr>>) -> GcDestr<FnOpRepr> {
    let arr = JsObjectBuilder::new(None).with_being_array().build();
    let mut ops = Vec::new();
    for (i, v) in values.iter_mut().enumerate() {
        ops.push(u_assign(
            u_literal(arr.clone()),
            u_literal(JsValue::Number(i as f64)),
            v.destroy_move(),
        ))
    }
    ops.push(u_literal(arr.clone()));
    u_block(ops)
}

pub fn u_array_e() -> JsValue {
    JsObjectBuilder::new(None).with_being_array().build()
}

/// Returns (hasNext, value)
pub fn u_it_next(it: GcDestr<FnOpRepr>) -> (GcDestr<FnOpRepr>, GcDestr<FnOpRepr>) {
    let returned = u_cached(u_call_simple(it));
    return (
        u_deref(returned(), u_literal(u_string("done"))),
        u_deref(returned(), u_literal(u_string("value"))),
    );
}

pub fn u_deref(from: GcDestr<FnOpRepr>, key: GcDestr<FnOpRepr>) -> GcDestr<FnOpRepr> {
    return GcDestr::new(FnOpRepr::Deref {
        from: Box::new(from),
        key: Box::new(key),
        from_store: JsVar::new(Rc::new("#temp#".into())),
        key_store: JsVar::new(Rc::new("#temp#".into())),
        done: JsVar::new(Rc::new("#temp#".into())),
        this_override: None,
    });
}

pub fn u_string(s: &str) -> JsValue {
    return JsValue::String(Rc::new(s.into()));
}

pub fn u_block(b: Vec<GcDestr<FnOpRepr>>) -> GcDestr<FnOpRepr> {
    return GcDestr::new(FnOpRepr::Multi { block: b });
}

pub fn u_function(b: GcDestr<FnOpRepr>) -> JsValue {
    return JsObjectBuilder::new(None)
        .with_callable(JSCallable::Js {
            content: Rc::new("#manual#".to_string()),
            creator: Gc::from(JsFn::new(b, move |f, ret, args, this| StackFrame {
                vars: vec![],
                remaining_ops: vec![f.destroy_move()],
                this,
                ret_store: ret,
            })),
        })
        .build();
}

pub fn u_call_simple(on: GcDestr<FnOpRepr>) -> GcDestr<FnOpRepr> {
    u_call(
        on,
        u_literal(JsObjectBuilder::new(None).with_being_array().build()),
    )
}

pub fn u_call(on: GcDestr<FnOpRepr>, args: GcDestr<FnOpRepr>) -> GcDestr<FnOpRepr> {
    return GcDestr::new(FnOpRepr::CallFunction {
        on: Box::new(on),
        arg_array: Box::from(args),
        on_var: JsVar::new(Rc::new("#temp#".into())),
        args_var: JsVar::new(Rc::new("#temp#".into())),
        this_var: JsVar::new(Rc::new("#temp#".into())),
        ready: JsVar::new(Rc::new("#temp#".into())),
    });
}

// TODO multi needs to return last. does it? <- what do I mean by that? ._.
pub fn u_capture_deref(of: GcDestr<FnOpRepr>) -> (GcDestr<FnOpRepr>, GcDestr<FnOpRepr>) {
    let v = JsVar::new(s_pool("#temp#"));
    let val = GcDestr::new(FnOpRepr::CaptureDeref {
        into: v.clone(),
        what: Box::new(of),
    });
    let from = u_read_var(v);
    return (val, from);
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
