use crate::js::data::js_types::JsValue;
use gc::{Finalize, Trace};
use gc::{Gc, GcCell};
use std::borrow::{Borrow, BorrowMut};
use std::collections::HashSet;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use std::sync::Mutex;

pub struct EngineState {
    tick_queue: Vec<AsyncStack>,
    external_calls: Mutex<Box<dyn FnOnce()>>,
}

impl EngineState {
    fn run_queue(&mut self, ticks: u64) {}
}

pub struct AsyncStack {
    stack: Vec<StackFrame>,
}

#[derive(Clone)]
pub enum FnOp {
    Assign {
        target: JsVar,
        what: Box<FnOp>,
    },
    LoadStatic {
        value: JsValue,
    },
    ReadVar {
        which: JsVar,
    },
    // Args are reversed for popping
    CallFunction {
        on: Box<FnOp>,
        arg_vars: Vec<JsVar>,
        arg_fillers: Vec<FnOp>,
    },
    Throw {
        what: Box<FnOp>,
    },
    Deref {
        from: Box<FnOp>,
        key: Box<FnOp>,
        from_store: Option<JsValue>,
        key_store: Option<JsValue>,
    },
    IfElse {
        condition: Box<FnOp>,
        if_block: Vec<FnOp>,
        else_block: Vec<FnOp>,
    },
    While {
        condition: Box<FnOp>,
        block: Vec<FnOp>,
    },
    For {
        initial: Box<FnOp>,
        condition: Box<FnOp>,
        each: Box<FnOp>,
        block: Vec<FnOp>,
    },
    Nop {},
    Multi {
        block: Vec<FnOp>,
    },
}

impl FnOp {
    fn proto_from_global(name: &str, consumer: impl FnOnce(JsVar) -> FnOp) -> FnOpResult {
        let temp_var = JsVar::new(Rc::new("#temp#".into()));
        return FnOpResult::LoadGlobal {
            cost: 1,
            name: Rc::new(name.into()),
            into: temp_var.clone(),
            next: FnOp::Deref {
                from: Box::from(FnOp::Deref {
                    from: Box::from(FnOp::ReadVar {
                        which: temp_var.clone(),
                    }),
                    key: Box::from(FnOp::LoadStatic {
                        value: JsValue::String(Rc::new("__proto__".into())),
                    }),
                    from_store: None,
                    key_store: None,
                }),
                key: Box::from(consumer(temp_var)),
                from_store: None,
                key_store: None,
            },
        };
    }

    fn forward_call(call: FnOpResult, then: impl FnOnce(FnOp) -> FnOp) -> FnOpResult {
        match call {
            FnOpResult::Call {
                cost,
                args,
                on,
                result,
                next,
            } => FnOpResult::Call {
                cost,
                on,
                args,
                result,
                next: then(next),
            },
            _ => {
                panic!("forward_call has to be called with a ::Call")
            }
        }
    }

    fn forward_load_global(
        load_global: FnOpResult,
        consumer: impl FnOnce(FnOp) -> FnOp,
    ) -> FnOpResult {
        match load_global {
            FnOpResult::LoadGlobal {
                cost,
                name,
                into,
                next,
            } => FnOpResult::LoadGlobal {
                cost,
                name,
                into,
                next: consumer(next),
            },
            _ => {
                panic!("load_global needs ::LoadGlobal")
            }
        }
    }

    fn do_dissolve(dissolver: FnOpResult, consumer: impl FnOnce(FnOp) -> FnOp) -> FnOpResult {
        match dissolver {
            FnOpResult::Dissolve { mut next, cost } => {
                let new_op = consumer(next.pop().unwrap());
                next.push(new_op);
                FnOpResult::Dissolve { next, cost }
            }
            _ => panic!("wrong option passed to do_dissolve"),
        }
    }

    fn run(mut self, max_cost: u64) -> FnOpResult {
        if max_cost == 0 {
            return FnOpResult::Ongoing {
                cost: 0,
                next: self,
            };
        }
        match self {
            FnOp::Assign { target, what } => {
                let mut result = what.run(max_cost - 1);
                match result {
                    FnOpResult::Throw { .. } => {
                        return result;
                    }
                    FnOpResult::Return { .. } => {
                        return result;
                    }
                    FnOpResult::Ongoing { cost, next } => {
                        return FnOpResult::Ongoing {
                            cost: 0,
                            next: FnOp::Assign {
                                target,
                                what: Box::from(next),
                            },
                        };
                    }
                    FnOpResult::Dissolve { mut next, cost } => {
                        if next.is_empty() {
                            return FnOpResult::Value {
                                cost: cost + 1,
                                what: JsValue::Undefined,
                            };
                        }
                        let new_what = Box::new(next.pop().unwrap());
                        next.push(FnOp::Assign {
                            target,
                            what: new_what,
                        });
                        return FnOpResult::Dissolve { next, cost };
                    }
                    FnOpResult::Value { what, cost } => {
                        (*target.value.try_borrow_mut().unwrap().deref_mut()) = what.clone();
                        return FnOpResult::Value {
                            cost: cost + 1,
                            what,
                        };
                    }
                    FnOpResult::Call { .. } => {
                        return FnOp::forward_call(result, |op| FnOp::Assign {
                            target,
                            what: Box::from(op),
                        })
                    }
                    FnOpResult::LoadGlobal { .. } => {
                        return FnOp::forward_load_global(result, move |loaded| FnOp::Assign {
                            target,
                            what: Box::from(loaded),
                        })
                    }
                }
            }
            FnOp::LoadStatic { value } => {
                return FnOpResult::Value {
                    cost: 1,
                    what: value.clone(),
                }
            }
            FnOp::ReadVar { which } => {
                return FnOpResult::Value {
                    cost: 1,
                    what: Deref::deref((&GcCell::borrow(&which.value))).clone(),
                }
            }
            FnOp::CallFunction {
                on,
                mut arg_vars,
                mut arg_fillers,
            } => {
                if arg_fillers.is_empty() {
                    let result = on.run(max_cost);
                    return match result {
                        FnOpResult::Dissolve { .. } => {
                            FnOp::do_dissolve(result, |then| FnOp::CallFunction {
                                on: Box::from(then),
                                arg_vars,
                                arg_fillers,
                            })
                        }
                        FnOpResult::Throw { .. } => result,
                        FnOpResult::Return { .. } => result,
                        FnOpResult::Value { cost, mut what } => {
                            let temp_var = JsVar::new(Rc::new("#result_holder#".into()));
                            FnOpResult::Call {
                                cost: cost + 1,
                                on: what,
                                args: arg_vars,
                                result: temp_var.clone(),
                                next: FnOp::ReadVar { which: temp_var },
                            }
                        }
                        FnOpResult::Ongoing { cost, mut next } => FnOpResult::Ongoing {
                            cost,
                            next: FnOp::CallFunction {
                                on: Box::from(next),
                                arg_vars,
                                arg_fillers,
                            },
                        },
                        FnOpResult::Call { .. } => {
                            FnOp::forward_load_global(result, |op| FnOp::CallFunction {
                                on: Box::from(op),
                                arg_vars,
                                arg_fillers,
                            })
                        }
                        FnOpResult::LoadGlobal { .. } => {
                            FnOp::forward_load_global(result, |loaded| FnOp::CallFunction {
                                on: Box::from(loaded),
                                arg_vars,
                                arg_fillers,
                            })
                        }
                    };
                }
                return FnOpResult::Dissolve {
                    next: arg_fillers
                        .drain(..)
                        .zip((&arg_vars).clone().drain(..))
                        .map(|filler_var| FnOp::Assign {
                            target: filler_var.1,
                            what: Box::new(filler_var.0),
                        })
                        .chain(vec![FnOp::CallFunction {
                            on,
                            arg_vars,
                            arg_fillers: vec![],
                        }])
                        .collect(),
                    cost: 0,
                };
            }
            FnOp::Throw { what } => {
                let result = what.run(max_cost);
                return match result {
                    FnOpResult::Dissolve { cost, mut next } => {
                        let new_what = Box::from(next.pop().unwrap());
                        next.push(FnOp::Throw { what: new_what });
                        FnOpResult::Dissolve {
                            next,
                            cost: cost + 1,
                        }
                    }
                    FnOpResult::Throw { .. } => result,
                    FnOpResult::Return { .. } => result,
                    FnOpResult::Value { what, cost } => FnOpResult::Throw {
                        cost: cost + 1,
                        what,
                    },
                    FnOpResult::Ongoing { next, cost } => FnOpResult::Ongoing {
                        cost: cost + 1,
                        next: FnOp::Throw {
                            what: Box::from(next),
                        },
                    },
                    FnOpResult::Call { .. } => FnOp::forward_call(result, |temp_var| FnOp::Throw {
                        what: Box::from(temp_var),
                    }),
                    FnOpResult::LoadGlobal { .. } => {
                        FnOp::forward_load_global(result, |op| FnOp::Throw {
                            what: Box::from(op),
                        })
                    }
                };
            }
            FnOp::Deref {
                from,
                key,
                from_store,
                key_store,
            } => {
                if let Some(from_stored) = from_store {
                    if let Some(key_stored) = key_store {
                        match &from_stored {
                            JsValue::Undefined => {
                                return FnOpResult::Throw {
                                    cost: 1,
                                    what: JsValue::String(
                                        format!(
                                            "Cannot read value {} of undefined",
                                            from_stored.to_system_string()
                                        )
                                        .into(),
                                    ),
                                }
                            }
                            JsValue::Null => {
                                return FnOpResult::Throw {
                                    cost: 1,
                                    what: JsValue::String(
                                        format!(
                                            "Cannot read value {} of null",
                                            from_stored.to_system_string()
                                        )
                                        .into(),
                                    ),
                                };
                            }
                            JsValue::Number(n) => {
                                let temp_var = JsVar::new(Rc::new("#temp_var#".into()));
                                return FnOpResult::LoadGlobal {
                                    cost: 1,
                                    name: Rc::new("Number".into()),
                                    into: temp_var.clone(),
                                    next: FnOp::Deref {
                                        from: Box::from(FnOp::ReadVar { which: temp_var }),
                                        key: Box::from(FnOp::LoadStatic { value: key_stored }),
                                        from_store: None,
                                        key_store: None,
                                    },
                                };
                            }
                            JsValue::Boolean(_) => {
                                return FnOp::proto_from_global("Boolean", |proto| FnOp::Deref {
                                    from: Box::from(FnOp::ReadVar { which: proto }),
                                    key: Box::from(FnOp::LoadStatic { value: key_stored }),
                                    from_store: None,
                                    key_store: None,
                                });
                            }
                            JsValue::String(s) => {
                                let k = key_stored.to_system_string();
                                if let Ok(index) = k.parse::<i32>() {
                                    let letter_at = &s[(index as usize)..((index + 1) as usize)];
                                    // ^ TODO check: safe? ^
                                    {
                                        return FnOpResult::Value {
                                            cost: 1,
                                            what: JsValue::String(Rc::new(letter_at.into())),
                                        };
                                    }
                                }
                                return FnOp::proto_from_global("String", |v| FnOp::ReadVar {
                                    which: v,
                                });
                            }
                            JsValue::Object { content, .. } => {
                                if let Some(found_value) =
                                    GcCell::borrow(&content).get(&key_stored.to_system_string())
                                {
                                    return FnOpResult::Value {
                                        cost: 1,
                                        what: (&GcCell::borrow(&found_value.value) as &JsValue)
                                            .clone(),
                                    };
                                }
                            }
                        }
                    }
                }
            }
            FnOp::IfElse {
                condition,
                if_block,
                else_block,
            } => {
                let condition_result = condition.run(max_cost);
                match condition_result {
                    FnOpResult::Dissolve { cost, mut next } => {
                        let new_condition = Box::from(next.pop().unwrap());
                        next.push(FnOp::IfElse {
                            condition: new_condition,
                            if_block,
                            else_block,
                        });
                        return FnOpResult::Dissolve { next, cost: cost };
                    }
                    FnOpResult::Throw { .. } => {
                        return condition_result;
                    }
                    FnOpResult::Return { .. } => {
                        return condition_result;
                    }
                    FnOpResult::Value { cost, what } => {
                        return if what.truthy() {
                            FnOpResult::Dissolve {
                                next: if_block,
                                cost: cost,
                            }
                        } else {
                            FnOpResult::Dissolve {
                                next: else_block,
                                cost: cost,
                            }
                        }
                    }
                    FnOpResult::Ongoing { cost, mut next } => {
                        return FnOpResult::Ongoing {
                            cost: cost,
                            next: FnOp::IfElse {
                                condition: Box::from(next),
                                if_block,
                                else_block,
                            },
                        }
                    }
                    FnOpResult::Call { .. } => {
                        return FnOp::forward_call(condition_result, |op| FnOp::IfElse {
                            condition: Box::from(op),
                            if_block,
                            else_block,
                        });
                    }
                    FnOpResult::LoadGlobal { .. } => {
                        return FnOp::forward_load_global(condition_result, |op| FnOp::IfElse {
                            condition: Box::from(op),
                            if_block,
                            else_block,
                        })
                    }
                }
            }
            FnOp::While {
                condition,
                mut block,
            } => {
                let next_loop = FnOp::While {
                    condition: condition.clone(),
                    block: block.clone(),
                };
                block.push(next_loop);
                return FnOpResult::Ongoing {
                    cost: 1,
                    next: FnOp::IfElse {
                        condition,
                        if_block: block,
                        else_block: vec![],
                    },
                };
            }
            FnOp::For {
                initial,
                condition,
                each,
                block,
            } => {
                return FnOpResult::Dissolve {
                    next: vec![
                        *initial,
                        FnOp::While {
                            condition,
                            block: vec![*each, FnOp::Multi { block }],
                        },
                    ],
                    cost: 0,
                }
            }
            FnOp::Nop { .. } => {
                return FnOpResult::Value {
                    cost: 1,
                    what: JsValue::Undefined,
                }
            }
            FnOp::Multi { block } => {
                return FnOpResult::Dissolve {
                    next: block,
                    cost: 1,
                }
            }
        };
        unimplemented!()
    }
}

pub enum FnOpResult {
    Dissolve {
        next: Vec<FnOp>,
        cost: u64,
    },
    Throw {
        cost: u64,
        what: JsValue,
    },
    Return {
        cost: u64,
        what: JsValue,
    },
    Value {
        cost: u64,
        what: JsValue,
    },
    Ongoing {
        cost: u64,
        next: FnOp,
    },
    Call {
        cost: u64,
        on: JsValue,
        args: Vec<JsVar>,
        result: JsVar,
        next: FnOp,
    },
    LoadGlobal {
        cost: u64,
        name: Rc<String>,
        into: JsVar,
        next: FnOp,
    },
}

pub struct StackFrame {
    vars: Vec<JsVar>,
    remaining_ops: Vec<FnOp>, // REVERSE ORDER list of remaining ops. Using pop
}

#[derive(Clone)]
pub struct JsVar {
    name: Rc<String>,
    value: Gc<GcCell<JsValue>>,
    defined: bool, // Global variables may be referenced, but not instantiated
}

impl JsVar {
    fn map<T>(&self, mapper: impl FnOnce(&JsValue) -> T) -> T {
        let br = GcCell::borrow(&self.value);
        return mapper(br.borrow());
    }

    fn new(name: Rc<String>) -> JsVar {
        return JsVar {
            name,
            value: Gc::new(GcCell::new(JsValue::Undefined)),
            defined: true,
        };
    }
}

impl Finalize for JsVar {}

unsafe impl Trace for JsVar {
    unsafe fn trace(&self) {
        self.value.trace();
    }

    unsafe fn root(&self) {
        self.value.root();
    }

    unsafe fn unroot(&self) {
        self.value.unroot();
    }

    fn finalize_glue(&self) {
        self.value.finalize_glue();
    }
}
