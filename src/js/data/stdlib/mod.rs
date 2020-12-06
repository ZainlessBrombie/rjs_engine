use crate::js::data::gc_util::GcDestr;
use crate::js::data::js_execution::{FnOpRepr};
use crate::js::data::js_types::{JSCallable, JsFn, JsValue};
use crate::js::data::util::JsObjectBuilder;
use crate::js::data::EngineConstants::{ConstantStrings, EngineConstants};
use gc::Gc;
use std::rc::Rc;

pub struct StdLib {
    protos: Protos,
}

pub struct Protos {
    symbol: JsValue,
}

impl StdLib {
    pub fn new(strings: Rc<ConstantStrings>, symbol_constr: JsValue) -> StdLib {
        let symbol_proto = JsObjectBuilder::new(Some(&strings))
            .with_prop(strings.constructor.clone(), symbol_constr)
            .build();

        StdLib {
            protos: Protos {
                symbol: symbol_proto,
            },
        }
    }
}
