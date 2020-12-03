use crate::js::data::js_types::{JSCallable, JsProperty, JsValue, Identity, JsObj};
use crate::js::data::EngineConstants::{EngineConstants, ConstantStrings};
use gc::{Gc, GcCell};
use std::rc::Rc;
use crate::js::data::gc_util::GcDestr;
use crate::js::data::js_execution::{FnOp, JsVar};

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
                is_symbol: false
            },
            strings: constants
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
        self.obj.content.insert(key, JsProperty {
            enumerable,
            configurable,
            writable,
            value
        });
        self
    }

    pub fn with_proto(mut self, proto: JsValue) -> Self {
        return self.with_prop_full(
            self.strings.proto.clone(),
            proto,
            false,
            false,
            false,
        );
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
        self.obj.symbol_keys.insert(key, JsProperty {
            enumerable,
            configurable,
            writable,
            value
        });
        return self;
    }

    pub fn build(self) -> JsValue {
        return JsValue::Object(Gc::new(GcCell::new(self.obj));
    }
}

pub fn u_standard_load_global(name: &str) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::LoadGlobal { name: Rc::new(name.to_string()) });
}

pub fn u_read_var(var: JsVar) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::ReadVar { which: var })
}

pub fn u_write_var(var: JsVar, what: GcDestr<FnOp>) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::Assign { target: var, what: Box::from(what) })
}

pub fn u_literal(value: JsValue) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::LoadStatic { value })
}

pub fn u_throw(what: GcDestr<FnOp>) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::Throw {what: Box::from(what) });
}

pub fn u_while(condition: GcDestr<FnOp>, block: GcDestr<FnOp>) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::While { condition: Box::new(condition), block: Box::new(block) })
}

pub fn u_deref(from: GcDestr<FnOp>, key: GcDestr<FnOp>) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::Deref {
        from: Box::new(from),
        key: Box::new(key),
        from_store: JsVar::new(Rc::new("#temp#".into())),
        key_store: JsVar::new(Rc::new("#temp#".into())),
        done: JsVar::new(Rc::new("#temp#".into()))
    })
}

pub fn u_string(s: &str) -> JsValue {
    return JsValue::String(Rc::new(s.into()));
}

pub fn u_call_simple(on: GcDestr<FnOp>) -> GcDestr<FnOp> {
    u_call(on, vec![])
}

pub fn u_call(on: GcDestr<FnOp>, args: Vec<GcDestr<FnOp>>) -> GcDestr<FnOp> {
    return GcDestr::new(FnOp::CallFunction {
        on: Box::new(on),
        arg_vars: (0..args.len()).into_iter().map(|i| JsVar::new(Rc::new("#temp#".into()))).collect(),
        arg_fillers: args
    })
}
