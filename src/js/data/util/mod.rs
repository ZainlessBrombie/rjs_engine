use crate::js::data::gc_util::GcDestr;
use crate::js::data::js_execution::{FnOp, JsVar, StackFrame};
use crate::js::data::js_types::{Identity, JSCallable, JsFn, JsObj, JsProperty, JsValue};
use crate::js::data::EngineConstants::{ConstantStrings, EngineConstants};
use gc::{Gc, GcCell};
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

pub fn u_standard_load_global(name: &str) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::LoadGlobal {
        name: Rc::new(name.to_string()),
    });
}

pub fn u_read_var(var: JsVar) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::ReadVar { which: var });
}

pub fn u_write_var(var: JsVar, what: GcDestr<FnOp>) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::Assign {
        target: var,
        what: Box::from(what),
    });
}

pub fn u_literal(value: JsValue) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::LoadStatic { value });
}

pub fn u_if(cond: GcDestr<FnOp>, if_b: GcDestr<FnOp>) -> GcDestr<FnOp> {
    u_if_else(cond, if_b, GcDestr::new(FnOp::Nop {}))
}

pub fn u_and(left: GcDestr<FnOp>, right: GcDestr<FnOp>) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::BoolAnd {
        left: Box::from(left),
        right: Box::from(right),
    });
}

pub fn u_or(left: GcDestr<FnOp>, right: GcDestr<FnOp>) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::BoolOr {
        left: Box::from(left),
        right: Box::from(right),
    });
}

pub fn u_strict_comp(left: GcDestr<FnOp>, right: GcDestr<FnOp>) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::StrictCompare {
        left: Box::new(left),
        right: Box::new(right),
        l_store: JsVar::new(Rc::new("#temp#".into())),
        r_store: JsVar::new(Rc::new("#temp#".into())),
        done: JsVar::new(Rc::new("#temp#".into())),
    });
}

pub fn u_fuzzy_comp(left: GcDestr<FnOp>, right: GcDestr<FnOp>) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::FuzzyCompare {
        left: Box::new(left),
        right: Box::new(right),
        l_store: JsVar::new(Rc::new("#temp#".into())),
        r_store: JsVar::new(Rc::new("#temp#".into())),
        done: JsVar::new(Rc::new("#temp#".into())),
    });
}

pub fn u_typeof(of: GcDestr<FnOp>) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::TypeOf {
        of: Box::new(of),
        result: JsVar::new(Rc::new("#temp#".into())),
        done: JsVar::new(Rc::new("#temp#".into())),
    });
}

pub fn u_if_else(cond: GcDestr<FnOp>, if_b: GcDestr<FnOp>, else_b: GcDestr<FnOp>) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::IfElse {
        condition: Box::from(cond),
        if_block: Box::from(if_b),
        else_block: Box::from(else_b),
    });
}

pub fn u_throw(what: GcDestr<FnOp>) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::Throw {
        what: Box::from(what),
    });
}

pub fn u_not(of: GcDestr<FnOp>) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::BoolNot { of: Box::new(of) });
}

pub fn u_while(condition: GcDestr<FnOp>, block: GcDestr<FnOp>) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::While {
        condition: Box::new(condition),
        block: Box::new(block),
    });
}

pub fn u_reusable(what: GcDestr<FnOp>) -> (GcDestr<FnOp>, impl Fn() -> GcDestr<FnOp>) {
    let temp = JsVar::new(Rc::new("#temp#".into()));
    (u_write_var(temp.clone(), what), move || {
        u_read_var(temp.clone())
    })
}

pub fn u_array() -> JsValue {
    return JsObjectBuilder::new(None).with_being_array().build();
}

pub fn u_deref(from: GcDestr<FnOp>, key: GcDestr<FnOp>) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::Deref {
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

pub fn u_block(b: Vec<GcDestr<FnOp>>) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::Multi { block: b });
}

pub fn u_function(b: GcDestr<FnOp>) -> JsValue {
    return JsObjectBuilder::new(None)
        .with_callable(JSCallable::Js {
            content: Rc::new("#manual#".to_string()),
            creator: Gc::from(JsFn::new(b, move |f, ret, args, this| StackFrame {
                vars: vec![],
                remaining_ops: vec![f.destroy_move()],
                ret_store: ret,
            })),
        })
        .build();
}

pub fn u_call_simple(on: GcDestr<FnOp>) -> GcDestr<FnOp> {
    u_call(on, vec![])
}

pub fn u_call(on: GcDestr<FnOp>, args: Vec<GcDestr<FnOp>>) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::CallFunction {
        on: Box::new(on),
        arg_vars: (0..args.len())
            .into_iter()
            .map(|i| JsVar::new(Rc::new("#temp#".into())))
            .collect(),
        arg_fillers: args,
    });
}
