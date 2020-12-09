use crate::js::data::execution_v2::constants::{BEGIN_VARS, HEAD_LOCATION, RETURN_TO_LOCATION};
use crate::js::data::execution_v2::function::{FunctionExecution, FunctionInstance};
use crate::js::data::execution_v2::opcode::{Arithmetic2Op, OpCode, Target};
use crate::js::data::execution_v2::stack_element::{FunctionHead, StackElement};
use crate::js::data::execution_v2::Stack;
use crate::js::data::js_types::{JSCallable, JsProperty, JsValue};
use crate::js::data::util::{s_pool, u_bool, u_number, u_string, JsObjectBuilder};
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

    let mut exception: Option<JsValue> = None;

    while consumed < run_for {
        let op = instance
            .code
            .instructions
            .get(head.execution.op_pointer)
            .expect("Method failed to return");
        head.execution.op_pointer += 1;

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

                let new_instance = match what_value {
                    JsValue::Object(obj) => match &GcCell::borrow_mut(&Gc::borrow(&obj)).call {
                        JSCallable::NotCallable => {
                            exception = Some(u_string("cannot call object"));
                            continue;
                        }
                        JSCallable::Js { content, creator } => creator.clone(),
                        JSCallable::Native { op } => {
                            let result = op.call(this_value, args_value);
                            match result {
                                Ok(ok_val) => {
                                    *stack
                                        .values
                                        .get_mut(stack.current_function + RETURN_TO_LOCATION) =
                                        StackElement::Value(ok_val);
                                }
                                Err(err_val) => {
                                    exception = Some(err_val);
                                }
                            }
                            continue;
                        }
                    },
                    // TODO Operations in JS can generally fail.
                    // This means we have to go up the stack, step by step. We thus need to keep
                    // An Option of an exception and walk up a step to the next best try catch
                    // while it is set
                    _ => {
                        exception = Some(u_string("cannot call value"));
                        continue;
                    }
                };

                let new_head = FunctionHead {
                    prev_function: stack.current_function,
                    execution: FunctionExecution {
                        op_pointer: 0,
                        catch_pointer: 0,
                        flag: false,
                        instance: new_instance.clone(),
                    },
                };
                instance = new_instance;
                stack.current_function = stack.values.len();

                // put our head into storage
                stack
                    .values
                    .get_mut(stack.current_function)
                    .unwrap()
                    .place_head(head);
                head = new_head;

                // Finally, extend the stack
                stack.values.push(StackElement::Borrowed); // We are using it right now
                stack.values.push(StackElement::Value(this_value)); // this
                stack.values.push(StackElement::Value(args_value)); // args
                stack.values.push(StackElement::Value(JsValue::Undefined)); // return-to
                stack.values.push(StackElement::Value(JsValue::Undefined)); // jump flag todo

                assert_eq!(stack.current_function + BEGIN_VARS, stack.values.len());

                stack
                    .values
                    .reserve(head.execution.instance.code.number_of_vars);
                stack.values.extend(
                    [0..head.execution.instance.code.number_of_vars]
                        .iter()
                        .map(|_| StackElement::Value(JsValue::Undefined)),
                );
            }
            OpCode::Throw { what } => {
                exception = Some(target_read(what, stack, &instance));
            }
            OpCode::Return { what } => {
                let ret_val = target_read(what, stack, &instance);

                // Truncate the stack
                stack.values.truncate(stack.current_function);
                // restore stack execution point
                stack.current_function = head.prev_function;
                // return return value
                *stack
                    .values
                    .get_mut(stack.current_function + RETURN_TO_LOCATION) =
                    StackElement::Value(ret_val);
                // set head
                head = stack
                    .values
                    .get_mut(stack.current_function + HEAD_LOCATION)
                    .expect("Could not find out head. This is unlikely.")
                    .assume_head();
                // set instance
                instance = head.execution.instance.clone();
            }
            OpCode::ReadProp { from, key } => {
                let from_val = target_read(from, stack, &instance);
                let key_val = target_read(key, stack, &instance);

                match proto_inclusive_read(
                    from_val,
                    key_val,
                    stack.values.get(0).expect("Empty stack").assume_value(),
                ) {
                    Ok(ok_val) => {
                        target_write(&op.target, ok_val, stack, &instance);
                    }
                    Err(err_val) => exception = Some(err_val),
                };
            }
            OpCode::Nop { .. } => {}
            OpCode::FuzzyCompare { left, right } => {
                let left_val = target_read(left, stack, &instance);
                let right_val = target_read(right, stack, &instance);

                let result = fuzzy_compare(left_val, right_val);

                target_write(&op.target, u_bool(result), stack, &instance);
            }
            OpCode::StrictCompare { right, left } => {
                let left_val = target_read(left, stack, &instance);
                let right_val = target_read(right, stack, &instance);

                let result = strict_compare(left_val, right_val);

                target_write(&op.target, u_bool(result), stack, &instance);
            }
            OpCode::TypeOf { what } => {
                let what_val = target_read(what, stack, &instance);

                let type_val = js_typeof(what_val);

                target_write(&op.target, JsValue::String(type_val), stack, &instance);
            }
            OpCode::Await { what } => {
                unimplemented!()
            }
            OpCode::AssignProp { value, key, of } => {
                let to_val = target_read(value, stack, &instance);
                let to_key = target_read(key, stack, &instance);
                let to_of = target_read(of, stack, &instance);

                let result = assign_prop(to_val, to_key, to_of);

                match result {
                    Ok(ok_val) => {
                        target_write(&op.target, ok_val, stack, &instance);
                    }
                    Err(err_val) => {
                        exception = Some(err_val);
                    }
                }
            }
            OpCode::Add { left, right } => {
                let left_val = target_read(left, stack, &instance);
                let right_val = target_read(right, stack, &instance);

                let result_val = fuzzy_add(left_val, right_val);

                target_write(&op.target, result_val, stack, &instance);
            }
            OpCode::Arithmetic2 {
                right,
                left,
                variant,
            } => {
                let left_val = target_read(left, stack, &instance);
                let right_val = target_read(right, stack, &instance);

                target_write(
                    &op.target,
                    arithmetic2(left_val, right_val),
                    stack,
                    &instance,
                );
            }
        }
    }
}

fn assign_prop(to: JsValue, key: JsValue, value: JsValue) -> Result<JsValue, JsValue> {
    match to {
        JsValue::Undefined => Err(u_string("cannot assign prop to undefined")),
        JsValue::Null => Err(u_string("cannot assign prop to undefined")),
        JsValue::Number(_) => Ok(key),
        JsValue::Boolean(_) => Ok(key),
        JsValue::String(_) => Ok(key),
        JsValue::Object(obj) => {
            let obj_ref = GcCell::borrow_mut(&Gc::borrow(&obj));

            if key.is_symbol() {
                obj_ref.symbol_keys.insert(
                    key.clone(),
                    JsProperty {
                        enumerable: false,
                        configurable: true,
                        writable: true,
                        value,
                    },
                );
                return Ok(key);
            } else {
                obj_ref.content.insert(
                    key.to_system_string(),
                    JsProperty {
                        enumerable: false,
                        configurable: true,
                        writable: true,
                        value,
                    },
                );
                return Ok(key);
            }
        }
    }
}

// TODO proto loops, global object -> Number (for example) being a number
fn proto_inclusive_read(
    value: JsValue,
    key: JsValue,
    global_object: JsValue,
) -> Result<JsValue, JsValue> {
    let mut is_proto = false;
    let mut source = value;
    loop {
        match source {
            JsValue::Undefined => {
                if is_proto {
                    return Ok(JsValue::Undefined);
                } else {
                    return Err(u_string("Cannot read key of undefined"));
                }
            }
            JsValue::Null => {
                if is_proto {
                    return Ok(JsValue::Undefined);
                } else {
                    return Err(u_string("Cannot read key of null"));
                }
            }
            JsValue::Number(_) => {
                if is_proto {
                    return Err(u_string("proto is a number"));
                }
                if let Ok(number_proto) = proto_inclusive_read(
                    global_object.clone(),
                    u_string("Number"),
                    global_object.clone(),
                ) {
                    is_proto = true;
                    source = number_proto;
                } else {
                    return Err(u_string("missing global Number proto"));
                }
            }
            JsValue::Boolean(_) => {
                if is_proto {
                    return Err(u_string("proto is a boolean"));
                }
                if let Ok(bool_proto) = proto_inclusive_read(
                    global_object.clone(),
                    u_string("Boolean"),
                    global_object.clone(),
                ) {
                    is_proto = true;
                    source = bool_proto;
                } else {
                    return Err(u_string("missing global Boolean proto"));
                }
            }
            JsValue::String(ref s) => {
                if let Ok(index) = key.to_system_string().parse::<usize>() {
                    if s.len() > index {
                        return Ok(u_string(&s[index..(index + 1)]));
                    }
                }

                if is_proto {
                    return Err(u_string("proto is a number"));
                }

                if let Ok(string_proto) = proto_inclusive_read(
                    global_object.clone(),
                    u_string("String"),
                    global_object.clone(),
                ) {
                    is_proto = true;
                    source = string_proto;
                } else {
                    return Err(u_string("missing global String proto"));
                }
            }
            JsValue::Object(ref obj) => {
                let obj_borrow = GcCell::borrow(&Gc::borrow(&obj));

                if key.is_symbol() {
                    if let Some(found_value) = obj_borrow.symbol_keys.get(&key) {
                        return Ok(found_value.value.clone());
                    } else {
                        source = proto_inclusive_read(
                            source,
                            u_string("__proto__"),
                            global_object.clone(),
                        )
                        .unwrap_or(JsValue::Undefined);
                        is_proto = true;
                    }
                } else {
                    if let Some(found_value) = obj_borrow.content.get(&key.to_system_string()) {
                        return Ok(found_value.value.clone());
                    } else {
                        source = proto_inclusive_read(
                            source,
                            u_string("__proto__"),
                            global_object.clone(),
                        )
                        .unwrap_or(JsValue::Undefined);
                        is_proto = true;
                    }
                }
            }
        }
    }
}

fn fuzzy_compare(left: JsValue, right: JsValue) -> bool {
    return match &(left, right) {
        (JsValue::Undefined | JsValue::Null, JsValue::Undefined | JsValue::Null) => true,
        (JsValue::Undefined | JsValue::Null, _) => false,
        (_, JsValue::Undefined | JsValue::Null) => false,
        (JsValue::Number(n1), JsValue::Number(n2)) => n1 == n2,
        (JsValue::String(s), JsValue::Number(n)) => *n == s.parse().unwrap_or(NAN),
        (JsValue::Number(n), JsValue::String(s)) => *n == s.parse().unwrap_or(NAN),
        (JsValue::Number(n), JsValue::Boolean(b)) => *n == (if *b { 1.0 } else { 0.0 }),
        (JsValue::Boolean(b), JsValue::Number(n)) => *n == (if *b { 1.0 } else { 0.0 }),
        (JsValue::Boolean(b1), JsValue::Boolean(b2)) => b1 == b2,
        (JsValue::String(s1), JsValue::String(s2)) => s1 == s2,
        (JsValue::String(s), JsValue::Boolean(b)) => s.as_str() == (if *b { "1" } else { "0" }),
        (JsValue::Object(o1), JsValue::Object(o2)) => {
            GcCell::borrow(&o1.borrow()).identity == GcCell::borrow(&o2.borrow()).identity
        }
        (_, _) => false, // TODO toprimitive
    };
}

fn strict_compare(left: JsValue, right: JsValue) -> bool {
    return match &(left, right) {
        (JsValue::Undefined, JsValue::Undefined) => true,
        (JsValue::Null, JsValue::Null) => true,
        (JsValue::Number(n1), JsValue::Number(n2)) => n1 == n2,
        (JsValue::Boolean(b1), JsValue::Boolean(b2)) => b1 == b2,
        (JsValue::String(s1), JsValue::String(s2)) => s1.as_str() == s2.as_str(),
        (JsValue::Object(o1), JsValue::Object(o2)) => {
            &GcCell::borrow(&o1.borrow()).identity == &GcCell::borrow(&o2.borrow()).identity
        }
        (_, _) => false,
    };
}

fn js_typeof(value: JsValue) -> Rc<String> {
    return match value {
        JsValue::Undefined => s_pool("undefined"),
        JsValue::Null => s_pool("object"),
        JsValue::Boolean(_) => s_pool("boolean"),
        JsValue::String(_) => s_pool("string"),
        JsValue::Object(_) => s_pool("object"),
        JsValue::Number(_) => s_pool("number"),
    };
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

fn arithmetic2(left: JsValue, right: JsValue, variant: Arithmetic2Op) -> JsValue {
    let left = coerce(left);
    let right = coerce(right);

    let result = match variant {
        Arithmetic2Op::Add => left + right,
        Arithmetic2Op::Sub => left - right,
        Arithmetic2Op::Multi => left * right,
        Arithmetic2Op::Div => left / right,
        Arithmetic2Op::Pow => left.powf(right),
    };
    u_number(result)
}

fn fuzzy_add(left: JsValue, right: JsValue) -> JsValue {
    match (left, right) {
        (JsValue::String(s1), val) => {
            JsValue::String(Rc::new(s1.to_string() + val.to_system_string().as_str()))
        }
        (val, JsValue::String(s1)) => {
            JsValue::String(Rc::new(val.to_system_string().to_string() + s1.as_str()))
        }
        (o @ JsValue::Object(_), val) => JsValue::String(Rc::new(
            o.to_system_string().to_string() + val.to_system_string().as_str(),
        )),
        (val, o @ JsValue::Object(_)) => JsValue::String(Rc::new(
            val.to_system_string().to_string() + o.to_system_string().as_str(),
        )),
        (v1, v2) => u_number(coerce(v1) + coerce(v2)),
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
