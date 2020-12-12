use crate::js::data::execution_v2::constants::{
    BEGIN_VARS, HEAD_LOCATION, JUMP_FLAG_LOCATION, RETURN_TO_LOCATION,
};
use crate::js::data::execution_v2::function::{FunctionExecution, FunctionInstance};
use crate::js::data::execution_v2::opcode::{Arithmetic2Op, Op, OpCode, Target};
use crate::js::data::execution_v2::stack_element::{FunctionHead, StackElement};
use crate::js::data::execution_v2::var::JsVar;
use crate::js::data::execution_v2::Stack;
use crate::js::data::js_types::{JSCallable, JsProperty, JsValue};
use crate::js::data::util::{s_pool, u_bool, u_number, u_string, JsObjectBuilder};
use safe_gc::{Gc, GcCell};
use std::f64::NAN;
use std::rc::Rc;

pub fn run_stack(stack: &mut Stack, run_for: usize, do_print: bool) -> usize {
    let mut consumed = 0;

    if stack.values.is_empty() || stack.current_function == 0 {
        return 0;
    }

    let mut head = stack
        .values
        .get_mut(stack.current_function)
        .unwrap()
        .assume_head();

    // No need to re-fetch all the time
    let mut instance = head.execution.instance.clone();

    let mut source_instance;

    while consumed < run_for {
        consumed += 1;
        source_instance = instance.clone(); // Lifetime stuff
        let op = source_instance
            .code
            .instructions
            .get(head.execution.op_pointer)
            .expect("Method failed to return");
        head.execution.op_pointer += 1;

        if stack.current_exception.is_some() {
            if head.execution.catch_pointer != 0 {
                head.execution.op_pointer = head.execution.catch_pointer;
            } else {
                // Truncate the stack
                stack.values.truncate(stack.current_function);

                // restore stack execution point
                stack.current_function = head.prev_function;

                // Stack is at its end
                if stack.current_function == 0 {
                    println!(
                        "Uncaught exception!\n{}",
                        &stack.current_exception.as_ref().unwrap().to_system_string()
                    );
                    return consumed;
                }

                // set head
                head = stack
                    .values
                    .get_mut(stack.current_function + HEAD_LOCATION)
                    .expect("Could not find our head. This is unlikely.")
                    .assume_head();
                // set instance
                instance = head.execution.instance.clone();
                // TODO
            }
            continue;
        }

        match &op.code {
            OpCode::Jump { to } => {
                head.execution.op_pointer = *to;
            }
            OpCode::ConditionalJump { to } => {
                if target_read(&Target::Stack(JUMP_FLAG_LOCATION), stack, &instance).truthy() {
                    head.execution.op_pointer = *to;
                }
            }
            OpCode::NewObject { is_array } => {
                let mut object_builder = JsObjectBuilder::new(None);
                if *is_array {
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
                                    .map(|local_heap_var| match local_heap_var {
                                        Target::Stack(var) => {
                                            match stack
                                                .values
                                                .get(stack.current_function + *var)
                                                .expect("Heap to short (unexpected)")
                                            {
                                                StackElement::HeapVar(var) => var.clone(),
                                                _ => panic!("Uncapturable element!"),
                                            }
                                        }
                                        _ => panic!("Uncapturable target!"),
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
                            stack.current_exception = Some(u_string("cannot call object"));
                            continue;
                        }
                        JSCallable::Js { content, creator } => creator.clone(),
                        JSCallable::Native { op } => {
                            let result = op.call(this_value, args_value);
                            match result {
                                Ok(ok_val) => {
                                    *stack
                                        .values
                                        .get_mut(stack.current_function + RETURN_TO_LOCATION)
                                        .expect("Unexpected empty stack in native return") =
                                        StackElement::Value(ok_val);
                                }
                                Err(err_val) => {
                                    stack.current_exception = Some(err_val);
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
                        stack.current_exception = Some(u_string(
                            &("cannot call value: ".to_string()
                                + what_value.to_system_string().as_str()),
                        ));
                        continue;
                    }
                };

                let new_head = FunctionHead {
                    prev_function: stack.current_function,
                    execution: FunctionExecution {
                        op_pointer: 0,
                        catch_pointer: 0,
                        instance: new_instance.clone(),
                    },
                };
                instance = new_instance;

                // put our head into storage
                stack
                    .values
                    .get_mut(stack.current_function)
                    .unwrap()
                    .place_head(head);
                head = new_head;

                // Point to the new head
                stack.current_function = stack.values.len();

                // Finally, extend the stack
                stack.values.push(StackElement::Borrowed); // We are using it right now
                stack.values.push(StackElement::Value(this_value)); // this
                stack.values.push(StackElement::Value(args_value)); // args
                stack.values.push(StackElement::Value(JsValue::Undefined)); // return-to
                stack.values.push(StackElement::Value(JsValue::Undefined)); // jump flag

                assert_eq!(stack.current_function + BEGIN_VARS, stack.values.len());

                stack
                    .values
                    .reserve(head.execution.instance.code.number_of_vars);

                for cap_var in instance.heap_vars.iter() {
                    stack.values.push(StackElement::HeapVar(cap_var.clone()));
                }

                for _ in 0..head.execution.instance.code.number_of_vars {
                    // TODO subtract heap var count
                    stack.values.push(StackElement::Value(JsValue::Undefined));
                }
            }
            OpCode::Throw { what } => {
                stack.current_exception = Some(target_read(what, stack, &instance));
            }
            OpCode::Return { what } => {
                let ret_val = target_read(what, stack, &instance);
                // Truncate the stack
                stack.values.truncate(stack.current_function);

                // restore stack execution point
                stack.current_function = head.prev_function;

                // Stack is at its end
                if stack.current_function == 0 {
                    return consumed;
                }

                // return return value
                *stack
                    .values
                    .get_mut(stack.current_function + RETURN_TO_LOCATION)
                    .expect("Unexpected empty local stack at return") =
                    StackElement::Value(ret_val);
                // set head
                head = stack
                    .values
                    .get_mut(stack.current_function + HEAD_LOCATION)
                    .expect("Could not find our head. This is unlikely.")
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
                    Err(err_val) => stack.current_exception = Some(err_val),
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
                let of_val = target_read(of, stack, &instance);
                let key_val = target_read(key, stack, &instance);
                let val_val = target_read(value, stack, &instance);

                let result = assign_prop(of_val, key_val, val_val);

                match result {
                    Ok(ok_val) => {
                        target_write(&op.target, ok_val, stack, &instance);
                    }
                    Err(err_val) => {
                        stack.current_exception = Some(err_val);
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
                    arithmetic2(left_val, right_val, *variant),
                    stack,
                    &instance,
                );
            }
            OpCode::Transfer { from } => target_write(
                &op.target,
                target_read(from, stack, &instance),
                stack,
                &instance,
            ),
        }

        if do_print {
            let formatted = op
                .loc
                .formatted(instance.code.meta.code_source.as_string().as_str());
            println!(
                "------------------------------------------\n\n{}",
                formatted
            );
            if stack.current_exception.is_some() {
                println!("Exception is now set...")
            }
            println!("> {}", debug_op(stack, op, &instance));
        }
    }

    stack
        .values
        .get_mut(stack.current_function)
        .expect("Current function stack point unavailable")
        .place_head(head);

    return consumed;
}

fn capture_heap_var(target: &Target, stack: &mut Stack, head: &FunctionHead) -> JsVar {
    match target {
        Target::Stack(stack_index) => {
            match stack
                .values
                .get(stack.current_function + *stack_index)
                .expect("Unexpected stack shortness")
            {
                StackElement::HeapVar(v) => v.clone(),
                _ => panic!("If a variable is captured from stack, it has to be a heap var"),
            }
        }
        Target::Global(_) => {
            panic!("cannot capture a global, that makes no sense!")
        }
        Target::BlackHole => {
            panic!("Cannot capture a black hole target!")
        }
    }
}

fn debug_op(stack: &Stack, op: &Op, instance: &Rc<FunctionInstance>) -> String {
    let mut ret = String::new();
    ret.push_str("Op:");
    match &op.code {
        OpCode::Jump { to } => ret.push_str(&format!("Jump <to='{}'>", to)),
        OpCode::ConditionalJump { to } => ret.push_str(&format!(
            "ConditionalJump <to='{}'; flag='{}'>",
            to,
            stack
                .values
                .get(stack.current_function + JUMP_FLAG_LOCATION)
                .map(|cur| cur.assume_value().truthy().to_string())
                .unwrap_or("<loadfail>".to_string())
        )),
        OpCode::NewObject { is_array } => {
            ret.push_str(&format!("NewObject <is_array='{}'>", is_array))
        }
        OpCode::Not { source } => ret.push_str(&format!(
            "Not <source='{}{}'>",
            target_read(source, stack, instance).to_log_string(),
            debug_vartype(source),
        )),
        OpCode::Assign { source } => ret.push_str(&format!(
            "Assign <source='{}{}'>",
            target_read(source, stack, instance).to_log_string(),
            debug_vartype(source)
        )),
        OpCode::Static { value } => {
            ret.push_str(&format!("Static <value='{}'>", value.to_log_string()))
        }
        OpCode::CreateFunction { captures, template } => {
            ret.push_str(&format!("CreateFunction <captures=",));
            for cap in captures {
                ret.push_str(&target_read(cap, stack, instance).to_log_string());
                ret.push_str(&debug_vartype(cap));
                ret.push_str("; ");
            }
            ret.push_str(">")
        }
        OpCode::Call { args, this, what } => ret.push_str(&format!(
            "Call <this='{}{}'; what='{}{}'; args='{}{}'>",
            target_read(this, stack, instance).to_log_string(),
            debug_vartype(this),
            target_read(what, stack, instance).to_log_string(),
            debug_vartype(what),
            target_read(args, stack, instance).to_log_string(),
            debug_vartype(args)
        )),
        OpCode::Throw { what } => ret.push_str(&format!(
            "Throw <what='{}{}'>",
            target_read(what, stack, instance).to_log_string(),
            debug_vartype(what)
        )),
        OpCode::Return { what } => ret.push_str(&format!(
            "Return <what='{}{}'>",
            target_read(what, stack, instance).to_log_string(),
            debug_vartype(what)
        )),
        OpCode::ReadProp { key, from } => ret.push_str(&format!(
            "ReadProp <from='{}{}'; key='{}{}'>",
            target_read(from, stack, instance).to_log_string(),
            debug_vartype(from),
            target_read(key, stack, instance).to_log_string(),
            debug_vartype(key)
        )),
        OpCode::Nop {} => ret.push_str(&format!("Nop <>",)),
        OpCode::Transfer { from } => ret.push_str(&format!(
            "Transfer <from='{}{}'>",
            target_read(from, stack, instance).to_log_string(),
            debug_vartype(from)
        )),
        OpCode::FuzzyCompare { right, left } => ret.push_str(&format!(
            "FuzzyCompare <left='{}{}'; right='{}{}'>",
            target_read(left, stack, instance).to_log_string(),
            debug_vartype(left),
            target_read(right, stack, instance).to_log_string(),
            debug_vartype(right),
        )),
        OpCode::StrictCompare { left, right } => ret.push_str(&format!(
            "StrictCompare <left='{}{}'; right='{}{}'>",
            target_read(left, stack, instance).to_log_string(),
            debug_vartype(left),
            target_read(right, stack, instance).to_log_string(),
            debug_vartype(right)
        )),
        OpCode::TypeOf { what } => ret.push_str(&format!(
            "TypeOf <what='{}{}'>",
            target_read(what, stack, instance).to_log_string(),
            debug_vartype(what),
        )),
        OpCode::Await { what } => ret.push_str(&format!(
            "Await <what='{}{}'>",
            target_read(what, stack, instance).to_log_string(),
            debug_vartype(what),
        )),
        OpCode::AssignProp { of, key, value } => ret.push_str(&format!(
            "AssignProp <of='{}{}'; key='{}{}'; value='{}{}'>",
            target_read(of, stack, instance).to_log_string(),
            debug_vartype(of),
            target_read(key, stack, instance).to_log_string(),
            debug_vartype(of),
            target_read(value, stack, instance).to_log_string(),
            debug_vartype(value)
        )),
        OpCode::Add { right, left } => ret.push_str(&format!(
            "Add <left='{}{}'; right='{}{}'>",
            target_read(left, stack, instance).to_log_string(),
            debug_vartype(left),
            target_read(right, stack, instance).to_log_string(),
            debug_vartype(right)
        )),
        OpCode::Arithmetic2 {
            left,
            right,
            variant,
        } => ret.push_str(&format!(
            "Arithmetic2 <left='{}{}'; right='{}{}'; op='{:?}'>",
            target_read(left, stack, instance).to_log_string(),
            debug_vartype(left),
            target_read(right, stack, instance).to_log_string(),
            debug_vartype(right),
            variant
        )),
    }
    ret.push_str(" => ");

    match &op.target {
        Target::BlackHole => ret.push_str("[hole]"),
        _ => ret.push_str(&format!(
            "{}{}",
            target_read(&op.target, stack, instance).to_log_string(),
            debug_vartype(&op.target)
        )),
    }

    return ret;
}

fn debug_vartype(target: &Target) -> String {
    match target {
        Target::Stack(n) => "[".to_string() + &n.to_string() + "]",
        Target::Global(v) => "[g:".to_string() + &v + "]",
        Target::BlackHole => "[hole]".to_string(),
    }
}

fn assign_prop(to: JsValue, key: JsValue, value: JsValue) -> Result<JsValue, JsValue> {
    match to {
        JsValue::Undefined => Err(u_string("cannot assign prop to undefined")),
        JsValue::Null => Err(u_string("cannot assign prop to undefined")),
        JsValue::Number(_) => Ok(value),
        JsValue::Boolean(_) => Ok(value),
        JsValue::String(_) => Ok(value),
        JsValue::Object(obj) => {
            let gc_ref = Gc::borrow(&obj);
            let mut obj_ref = GcCell::borrow_mut(&gc_ref);

            if key.is_symbol() {
                obj_ref.symbol_keys.insert(
                    key.clone(),
                    JsProperty {
                        enumerable: false,
                        configurable: true,
                        writable: true,
                        value: value.clone(),
                    },
                );
                return Ok(value);
            } else {
                obj_ref.content.insert(
                    key.to_system_string(),
                    JsProperty {
                        enumerable: false,
                        configurable: true,
                        writable: true,
                        value: value.clone(),
                    },
                );
                return Ok(value);
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
                let gc_ref = Gc::borrow(&obj);
                let obj_borrow = GcCell::borrow(&gc_ref);

                if key.is_symbol() {
                    if let Some(found_value) = obj_borrow.symbol_keys.get(&key) {
                        return Ok(found_value.value.clone());
                    } else {
                        std::mem::drop(obj_borrow);
                        std::mem::drop(gc_ref);
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
                        if key.to_system_string().as_str() != "__proto__" {
                            std::mem::drop(obj_borrow);
                            std::mem::drop(gc_ref);
                            source = proto_inclusive_read(
                                source,
                                u_string("__proto__"),
                                global_object.clone(),
                            )
                            .unwrap_or(JsValue::Undefined);
                            is_proto = true;
                        } else {
                            std::mem::drop(obj_borrow);
                            std::mem::drop(gc_ref);
                            source = JsValue::Undefined;
                        }
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

fn target_read(target: &Target, stack: &Stack, current_function: &Rc<FunctionInstance>) -> JsValue {
    match target {
        Target::Stack(stack_pointer) => {
            match stack
                .values
                .get(*stack_pointer + stack.current_function)
                .expect("Unexpected stack size")
            {
                StackElement::Value(value) => {
                    return value.clone();
                }
                StackElement::HeapVar(v) => {
                    return v.get();
                }
                _ => panic!("Expected value or heap var at stack location"),
            }
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
            match stack
                .values
                .get_mut(*stack_pointer + stack.current_function)
                .expect("Unexpected stack size")
            {
                StackElement::HeapVar(heap) => {
                    heap.set(what);
                }
                el @ StackElement::Value(_) => *el = StackElement::Value(what),
                _ => panic!("Cannot write to incorrect heap part"),
            };
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
