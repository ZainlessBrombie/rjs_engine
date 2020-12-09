use crate::js::data::execution_v2::function::{FunctionExecution, FunctionInstance};
use crate::js::data::execution_v2::opcode::{Op, OpCode, Target};
use crate::js::data::execution_v2::stack_element::{FunctionHead, StackElement};
use crate::js::data::execution_v2::Stack;
use crate::js::data::js_types::{JSCallable, JsProperty, JsValue};
use crate::js::data::util::JsObjectBuilder;
use safe_gc::{Gc, GcCell};
use std::borrow::BorrowMut;
use std::f64::NAN;
use std::rc::Rc;

pub fn run_stack(stack: &mut Stack, run_for: usize) -> usize {
    let mut consumed = 0;

    if stack.values.is_empty() {
        return 0;
    }

    let mut head = stack
        .values
        .get_mut(stack.current_function)
        .unwrap()
        .assume_head();

    // No need to re-fetch all the time
    let mut instance = head.execution.instance.clone();
    let mut code = instance.code.clone();

    while consumed < run_for {
        let op = instance
            .code
            .instructions
            .get(head.execution.op_pointer)
            .expect("Method failed to return");

        match &op.code {
            OpCode::Jump { to } => {
                head.execution.op_pointer = *to;
            }
            OpCode::ConditionalJump { to } => {
                if head.execution.flag {
                    head.execution.op_pointer = *to;
                }
            }
            OpCode::NewObject { is_array } => {
                let mut object_builder = JsObjectBuilder::new(None);
                if is_array {
                    object_builder = object_builder.with_being_array();
                }
                target_write(&op.target, object_builder.build(), stack, &instance);
            }
            OpCode::Not { source } => {
                let is_true = target_read(source, stack, &instance).truthy();
                target_write(&op.target, JsValue::Boolean(!is_true), stack, &instance);
            }
            OpCode::Assign { source } => {
                target_write(
                    &op.target,
                    target_read(source, stack, &instance),
                    stack,
                    &instance,
                );
            }
            OpCode::Static { value } => {
                target_write(&op.target, value.clone(), stack, &instance);
            }
            OpCode::CreateFunction { captures, template } => {
                let value = JsObjectBuilder::new(None)
                    .with_callable(JSCallable::Js {
                        content: Rc::new("".to_string()),
                        creator: Rc::new(FunctionInstance {
                            code: template.clone(),
                            heap_vars: Rc::new(
                                captures
                                    .iter()
                                    .map(|local_heap_var| {
                                        head.execution
                                            .instance
                                            .heap_vars
                                            .get(*local_heap_var)
                                            .expect("Missing local heap var")
                                            .get()
                                            .clone()
                                    })
                                    .collect(),
                            ),
                        }),
                    })
                    .build();
                target_write(&op.target, value, stack, &instance);
            }
            OpCode::Call { this, args, what } => {
                let this_value = target_read(this, stack, &instance);
                let args_value = target_read(args, stack, &instance);
                let what_value = target_read(what, stack, &instance);

                match what_value {
                    JsValue::Object(_) => {}
                    // TODO Operations in JS can generally fail.
                    // This means we have to go up the stack, step by step. We thus need to keep
                    // An Option of an exception and walk up a step to the next best try catch
                    // while it is set
                    _ => unimplemented!(),
                }

                let new_head = FunctionHead {
                    prev_function: stack.current_function,
                    execution: FunctionExecution {
                        op_pointer: 0,
                        catch_pointer: 0,
                        flag: false,
                        instance,
                    },
                };
                stack.current_function = stack.values.len();

                // put our head into storage
                stack
                    .values
                    .get_mut(stack.current_function)
                    .unwrap()
                    .place_head(head);
                head = new_head;
            }
            OpCode::Throw { .. } => {}
            OpCode::Return { .. } => {}
            OpCode::ReadProp { .. } => {}
            OpCode::Nop { .. } => {}
            OpCode::FuzzyCompare { .. } => {}
            OpCode::StrictCompare { .. } => {}
            OpCode::TypeOf { .. } => {}
            OpCode::Await { .. } => {}
            OpCode::AssignProp { .. } => {}
            OpCode::Add { .. } => {}
            OpCode::Arithmetic2 { .. } => {}
        }
    }
    unimplemented!()
}

fn target_read(
    target: &Target,
    stack: &mut Stack,
    current_function: &Rc<FunctionInstance>,
) -> JsValue {
    match target {
        Target::Stack(stack_pointer) => {
            match stack
                .values
                .get(*stack_pointer)
                .expect("Unexpected stack size")
            {
                StackElement::Value(value) => {
                    return value.clone();
                }
                _ => panic!("Expected value at stack location"),
            }
        }
        Target::Heap(heap_index) => {
            return current_function
                .heap_vars
                .get(*heap_index)
                .expect("Unexpected missing heap var entry")
                .get();
        }
        Target::Global(global_name) => {
            match &stack
                .values
                .get(0)
                .expect("Unexpected empty stack")
                .assume_value()
            {
                JsValue::Object(o) => {
                    return GcCell::borrow_mut(&Gc::borrow(o))
                        .content
                        .get(global_name)
                        .map(|prop| prop.value.clone())
                        .unwrap_or(JsValue::Undefined);
                }
                _ => panic!("Global at 0 was not an object"),
            };
        }
        Target::BlackHole => {
            return JsValue::Undefined;
        }
    }
}

fn target_write(
    target: &Target,
    what: JsValue,
    stack: &mut Stack,
    current_function: &Rc<FunctionInstance>,
) {
    match target {
        Target::Stack(stack_pointer) => {
            *stack
                .values
                .get_mut(*stack_pointer)
                .expect("Unexpected stack size") = StackElement::Value(what);
        }
        Target::Heap(heap_index) => {
            current_function
                .heap_vars
                .get(*heap_index)
                .expect("Unexpected missing heap var entry")
                .set(what);
        }
        Target::Global(global_name) => {
            match &stack
                .values
                .get(0)
                .expect("Unexpected empty stack")
                .assume_value()
            {
                JsValue::Object(o) => {
                    return GcCell::borrow_mut(&Gc::borrow(o)).content.insert(
                        global_name.clone(),
                        JsProperty {
                            enumerable: false,
                            configurable: false,
                            writable: true,
                            value: what,
                        },
                    )
                }
                _ => panic!("Global at 0 was not an object"),
            };
        }
        Target::BlackHole => {
            return;
        }
    }
}

fn coerce(val: JsValue) -> f64 {
    match val {
        JsValue::Undefined => NAN,
        JsValue::Null => 0.0,
        JsValue::Number(n) => n,
        JsValue::Boolean(b) => {
            if b {
                1.0
            } else {
                0.0
            }
        }
        _ => NAN,
    }
}
