pub mod resolver;

use crate::js::data::execution_v2::native_fn::native_from;
use crate::js::data::js_types::{JSCallable, JsValue};
use crate::js::data::util::{s_pool, JsObjectBuilder};

pub fn build_std_global() -> JsValue {
    let console = JsObjectBuilder::new()
        .with_prop(
            s_pool("log"),
            JsObjectBuilder::new()
                .with_callable(JSCallable::Native {
                    op: native_from(|_this, args| {
                        return {
                            println!("Print: {}", args.to_system_string());
                            Ok(JsValue::Undefined)
                        };
                    }),
                })
                .build(),
        )
        .build();
    return JsObjectBuilder::new()
        .with_prop(s_pool("console"), console)
        .build();
}
