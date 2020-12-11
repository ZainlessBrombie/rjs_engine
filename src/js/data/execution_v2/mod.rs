use crate::js::data::execution_v2::function::{
    CodeSource, FunctionExecution, FunctionInstance, FunctionMeta, OpFunction,
};
use crate::js::data::execution_v2::opcode::{Op, OpCode, Target};
use crate::js::data::execution_v2::stack_element::{FunctionHead, StackElement};
use crate::js::data::execution_v2::var::JsVar;
use crate::js::data::util::JsObjectBuilder;
use safe_gc::{Gc, GcCell};
use std::rc::Rc;
use crate::js::data::js_types::JsValue;

pub mod constants;
pub mod function;
pub mod opcode;
pub mod stack_element;
pub mod stack_executor;
pub mod var;

pub struct Stack {
    pub values: Vec<StackElement>,
    pub current_function: usize,
}

impl Stack {
    pub fn create_stack(function: JsValue) -> Stack {
        let global = JsVar {
            name: Rc::new("".to_string()),
            value: Gc::new(GcCell::new(JsObjectBuilder::new(None).build())),
        };
        let function = JsVar {
            name: Rc::new("".to_string()),
            value: Gc::new(GcCell::new(function)),
        };
        Stack {
            values: vec![
                StackElement::HeapVar(global),
                StackElement::HeapVar(function),
                StackElement::FunctionHead(FunctionHead {
                    prev_function: 0,
                    execution: FunctionExecution {
                        op_pointer: 0,
                        catch_pointer: 0,
                        instance: Rc::new(FunctionInstance {
                            code: Rc::new(OpFunction {
                                instructions: vec![Op {
                                    target: Target::Stack(1),
                                    code: OpCode::Call {
                                        what: Target::BlackHole,
                                        this: Target::BlackHole,
                                        args: Target::BlackHole,
                                    },
                                }],
                                number_of_vars: 2,
                                meta: FunctionMeta {
                                    line_map: vec![],
                                    column_map: vec![],
                                    code_source: CodeSource::String(Rc::new("".into())),
                                },
                            }),
                            heap_vars: Rc::new(vec![]),
                        }),
                    },
                }),
            ],
            current_function: 0,
        }
    }
}
