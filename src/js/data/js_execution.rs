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
                        next.push(FnOp::Assign {
                            target,
                            what: Box::new(next.pop().unwrap()),
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
                    FnOpResult::Call { .. } => {}
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
                arg_vars,
                arg_fillers,
            } => {
                if arg_fillers.is_empty() {
                    let result = on.run(max_cost);
                    match result {
                        FnOpResult::Dissolve { mut next, cost } => {
                            next.push(FnOp::CallFunction {
                                on: Box::from(next.pop().unwrap()),
                                arg_vars,
                                arg_fillers,
                            });
                        }
                        FnOpResult::Throw { .. } => {
                            return result;
                        }
                        FnOpResult::Return { .. } => {
                            return result;
                        }
                        FnOpResult::Value { what, cost } => {
                            return FnOpResult::Call {
                                cost: cost + 1,
                                on: what,
                                args: arg_vars.iter().map(|var| var.map(|v| v.clone())).collect(),
                                result: JsVar::new("#result_holder#".into()),
                            };
                        }
                        FnOpResult::Ongoing { cost, next } => {
                            return FnOpResult::Ongoing {
                                cost,
                                next: FnOp::CallFunction {
                                    on: Box::from(next),
                                    arg_vars,
                                    arg_fillers,
                                },
                            }
                        }
                        FnOpResult::Call {
                            cost,
                            on: inner_on,
                            args,
                            result,
                        } => {
                            let result_holder = JsVar::new("#result#".into());
                            return FnOpResult::Dissolve {
                                next: vec![
                                    FnOp::CallFunction {
                                        on: Box::from(FnOp::LoadStatic { value: inner_on }),
                                        arg_vars: args,
                                        arg_fillers: Vec::new(),
                                    },
                                    FnOp::CallFunction {
                                        on: Box::from(FnOp::ReadVar {
                                            which: result_holder,
                                        }),
                                        arg_vars,
                                        arg_fillers,
                                    },
                                ],
                                cost: cost + 1,
                            };
                        }
                    }
                }
            }
            FnOp::Throw { what } => match what.run(max_cost) {
                FnOpResult::Dissolve { cost, mut next } => {
                    next.push(FnOp::Throw {
                        what: Box::from(next.pop().unwrap()),
                    });
                    return FnOpResult::Dissolve {
                        next,
                        cost: cost + 1,
                    };
                }
                FnOpResult::Throw { .. } => {
                    return result;
                }
                FnOpResult::Return { .. } => {
                    return result;
                }
                FnOpResult::Value { what, cost } => {
                    return FnOpResult::Throw {
                        cost: cost + 1,
                        what,
                    }
                }
                FnOpResult::Ongoing { .. } => {}
                FnOpResult::Call { .. } => {}
            },
            FnOp::Deref { .. } => {}
            FnOp::IfElse { .. } => {}
            FnOp::While { .. } => {}
            FnOp::For { .. } => {}
            FnOp::Nop { .. } => {}
            FnOp::Multi { .. } => {}
        }
        unimplemented!();
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
