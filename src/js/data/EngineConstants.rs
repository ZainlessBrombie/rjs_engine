use crate::js::data::js_types::{Identity, JSCallable, JsFn, JsValue};
use crate::js::data::stdlib::StdLib;
use crate::js::data::util::JsObjectBuilder;
use gc::Gc;
use std::rc::Rc;

pub struct EngineConstants {
    pub symbols: ConstantSymbols,
    pub methods: ConstantMethods,
    pub strings: Rc<ConstantStrings>,
}

pub struct ConstantStrings {
    pub proto: Rc<String>,
    pub description: Rc<String>,
    pub constructor: Rc<String>,
}

pub struct ConstantSymbols {
    pub iterator: JsValue,
}

pub struct ConstantMethods {}

fn build_symbol(name: Rc<String>, strings: &ConstantStrings, sym_proto: JsValue) -> JsValue {
    return JsObjectBuilder::new(Some(strings))
        .with_proto(sym_proto)
        .with_prop(strings.description.clone(), JsValue::String(name))
        .with_being_symbol()
        .build();
}

impl EngineConstants {
    pub fn new() -> EngineConstants {
        let strings = Rc::new(ConstantStrings {
            proto: Rc::new("__proto__".to_string()),
            description: Rc::new("description".to_string()),
            constructor: Rc::new("constructor".to_string()),
        });

        let strings_clone = strings.clone();
        let symbol_constr = JsObjectBuilder::new(Some(&strings))
            .with_callable(JSCallable::Native {
                creator: Gc::new(JsFn::simple_call((), move |_, mut args| {
                    Ok(JsObjectBuilder::new(Some(&strings_clone))
                        .with_being_symbol()
                        .with_prop(strings_clone.description.clone(), args)
                        .build())
                })),
            })
            .build();

        let std = StdLib::new(strings.clone(), symbol_constr.clone());

        let iterator_sym =
            build_symbol(Rc::new("Iterator".into()), &strings, symbol_constr.clone()).into();
        EngineConstants {
            symbols: ConstantSymbols {
                iterator: iterator_sym,
            },
            methods: ConstantMethods {},
            strings,
        }
    }
}
