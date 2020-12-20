use crate::js::data::execution_v2::constants::BEGIN_VARS;
use crate::js::data::execution_v2::function::{
    CodeSource, FunctionExecution, FunctionInstance, FunctionMeta, OpFunction,
};
use crate::js::data::execution_v2::opcode::{Op, OpCode, Target};
use crate::js::data::execution_v2::stack_element::{FunctionHead, StackElement};
use crate::js::data::execution_v2::var::JsVar;
use crate::js::data::intermediate::CodeLoc;
use crate::js::data::js_types::JsValue;
use crate::js::data::util::{s_pool, JsObjectBuilder};
use safe_gc::{Gc, GcCell};
use std::rc::Rc;

pub mod constants;
pub mod function;
pub mod native_fn;
pub mod opcode;
pub mod stack_element;
pub mod stack_executor;
pub mod var;

pub struct Stack {
    pub values: Vec<StackElement>,
    pub current_function: usize,
    pub current_exception: Option<JsValue>,
}

impl Stack {
    pub fn create_stack(function: JsValue, global: JsValue) -> Stack {
        let function = JsVar {
            name: Rc::new("<initial_function>".to_string()),
            value: Gc::new(GcCell::new(function)),
        };
        Stack {
            values: vec![
                StackElement::Value(global),
                StackElement::FunctionHead(FunctionHead {
                    prev_function: 0,
                    execution: FunctionExecution {
                        op_pointer: 0,
                        catch_pointer: 0,
                        instance: Rc::new(FunctionInstance {
                            code: Rc::new(OpFunction {
                                instructions: vec![
                                    Op {
                                        target: Target::BlackHole,
                                        code: OpCode::Call {
                                            what: Target::Stack(BEGIN_VARS),
                                            this: Target::BlackHole,
                                            args: Target::Stack(BEGIN_VARS + 1),
                                        },
                                        loc: CodeLoc { line: 1, column: 0 },
                                    },
                                    Op {
                                        target: Target::BlackHole,
                                        code: OpCode::Return {
                                            what: Target::BlackHole,
                                        },
                                        loc: CodeLoc { line: 1, column: 5 },
                                    },
                                ],
                                number_of_vars: 10,
                                meta: FunctionMeta {
                                    code_source: CodeSource::String(Rc::new("<init-2>".into())),
                                },
                            }),
                            heap_vars: Rc::new(vec![]),
                        }),
                    },
                }),
                StackElement::Value(JsValue::Undefined),
                StackElement::Value(JsValue::Undefined),
                StackElement::Value(JsValue::Undefined),
                StackElement::Value(JsValue::Undefined),
                StackElement::HeapVar(function.clone()),
                StackElement::Value(
                    JsObjectBuilder::new()
                        .with_prop(s_pool("type"), JsValue::String(s_pool("init_arr")))
                        .build(),
                ),
                StackElement::Value(JsValue::Undefined),
                StackElement::Value(JsValue::Undefined),
            ],
            current_function: 1,
            current_exception: None,
        }
    }
}
