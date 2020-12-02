use crate::js::data::gc_util::GcDestr;
use crate::js::data::js_types::{JSCallable, JsFn, JsProperty, JsValue};
use gc::{Finalize, GcCellRef, Trace};
use gc::{Gc, GcCell};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::mem::take;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use std::sync::Mutex;

pub struct EngineState {
    pub(crate) tick_queue: Vec<Gc<GcCell<AsyncStack>>>,
    pub(crate) external_calls: Mutex<Vec<Box<dyn FnOnce()>>>,
    channel_write: std::sync::mpsc::Sender<Box<dyn FnOnce()>>,
    channel_read: std::sync::mpsc::Receiver<Box<dyn FnOnce()>>,
}

impl EngineState {
    pub fn run_queue(&mut self, ticks: u64) -> u64 {
        let mut consumed = 0;

        loop {
            while consumed < ticks {
                if let Some(cur) = self.tick_queue.rl_front_mut() {
                    let mut ref_mut = GcCell::borrow_mut(&cur);
                    let (result, cost) = ref_mut.run(ticks - consumed);
                    consumed += cost;
                    match result {
                        AsyncStackResult::Forget => {
                            std::mem::drop(ref_mut);
                            self.tick_queue.rl_pop_front();
                        }
                        AsyncStackResult::Keep => {}
                    }
                } else {
                    return consumed;
                }
            }
            let guard = self
                .external_calls
                .get_mut()
                .expect("Mutex poisoned. This is an internal error.");
            if !guard.is_empty() {
                for ext_call in guard.drain(..) {
                    (ext_call)();
                }
            }
            if consumed >= ticks {
                return consumed;
            }
        }
    }

    pub fn enqueue_js_fn(&self, val: JsValue) {
        let guard = self
            .external_calls
            .get_mut()
            .expect("Mutex poisoned. This is an internal error.");
    }
}

#[derive(Trace, Finalize)]
pub struct AsyncStack {
    stack: Vec<StackFrame>,
}

enum AsyncStackResult {
    Forget,
    Keep,
}

impl AsyncStack {
    fn run(&mut self, max: u64) -> (AsyncStackResult, u64) {
        let mut consumed = 0;
        loop {
            if consumed >= max {
                return (AsyncStackResult::Keep, consumed);
            }

            if let Some(last) = self.stack.last_mut() {
                while consumed <= max {
                    if let Some(op) = last.remaining_ops.rl_pop_front() {
                        let result = FnOp::run(op, max - consumed);
                        match result {
                            FnOpResult::Dissolve { cost, next } => {
                                consumed += cost;
                                last.remaining_ops.rl_append_front(next);
                            }
                            FnOpResult::Throw { cost, what } => {
                                consumed += cost;
                                self.stack.pop();
                                if let Some(above) = self.stack.last_mut() {
                                    above.remaining_ops.rl_push_front(GcDestr::new(FnOp::Throw {
                                        what: Box::from(GcDestr::from(FnOp::LoadStatic {
                                            value: what,
                                        })),
                                    }));
                                    break;
                                } else {
                                    println!("Uncaught Promise!");
                                    return (AsyncStackResult::Forget, consumed);
                                }
                            }
                            FnOpResult::Return { what, cost } => {
                                consumed += cost;
                                last.ret_store.set(what);
                                self.stack.rl_pop_front();
                            }
                            FnOpResult::Value { cost, what } => {
                                consumed += cost;
                            }
                            FnOpResult::Ongoing { cost, next } => {
                                consumed += cost;
                                last.remaining_ops.rl_push_front(next);
                            }
                            FnOpResult::Call {
                                cost,
                                on,
                                args,
                                result,
                                next,
                            } => {
                                consumed += cost;
                                last.remaining_ops.rl_push_front(next);
                                self.stack.push(call_to_js_stack(on, result));
                                break;
                            }
                            FnOpResult::LoadGlobal {
                                cost,
                                name,
                                into,
                                next,
                            } => {
                                consumed += cost;
                                if name.as_str() == "console" {
                                    let mut console_map = HashMap::new();
                                    console_map.insert(
                                        Rc::new("log".into()),
                                        JsProperty {
                                            enumerable: false,
                                            configurable: false,
                                            writable: false,
                                            value: Gc::new(GcCell::new(JsValue::Object {
                                                is_array: false,
                                                content: Default::default(),
                                                call: GcCell::new(JSCallable::Native {
                                                    creator: Gc::new(JsFn {
                                                        builder: Box::new(|ret_val, args| {
                                                            println!(
                                                                "{}",
                                                                args.iter()
                                                                    .map(|j| j
                                                                        .to_system_string()
                                                                        .to_string())
                                                                    .collect::<Vec<String>>()
                                                                    .as_slice()
                                                                    .join(" ")
                                                            );
                                                            return StackFrame {
                                                                vars: vec![],
                                                                remaining_ops: vec![],
                                                                ret_store: JsVar::new(Rc::new(
                                                                    "#ignored#".into(),
                                                                )),
                                                            };
                                                        }),
                                                        tracer: Box::new(()),
                                                    }),
                                                }),
                                            })),
                                        },
                                    );
                                    last.remaining_ops.rl_push_front(GcDestr::new(
                                        FnOp::LoadStatic {
                                            value: JsValue::Object {
                                                is_array: false,
                                                content: Gc::new(GcCell::new(console_map)),
                                                call: GcCell::new(JSCallable::NotCallable),
                                            },
                                        },
                                    ))
                                }
                            }
                            FnOpResult::Await { .. } => {
                                return (AsyncStackResult::Forget, consumed);
                            }
                        }
                    } else {
                        last.remaining_ops.rl_push_front(GcDestr::new(FnOp::Return {
                            what: Box::from(GcDestr::from(FnOp::LoadStatic {
                                value: JsValue::Undefined,
                            })),
                        }))
                    }
                    break;
                }
            } else {
                return (AsyncStackResult::Forget, consumed);
            }
        }
    }
}

fn call_to_js_stack(val: JsValue, ret_val: JsVar) -> StackFrame {
    return match &val {
        JsValue::String(s) => throw_frame(JsValue::from_string("[string] is not a function")),
        JsValue::Object { call, .. } => match GcCellRef::deref(&GcCell::borrow(&call)) {
            JSCallable::NotCallable => {
                throw_frame(JsValue::from_string("[object Object] is not a function"))
            }
            JSCallable::Js { creator, .. } => (creator.builder)(ret_val, vec![]),
            JSCallable::Native { creator } => (creator.builder)(ret_val, vec![]), // TODO
        },
        v => throw_frame(JsValue::from_string(
            format!("cannot read value of {}", v.to_system_string()).as_str(),
        )),
    };
}

fn throw_frame(val: JsValue) -> StackFrame {
    return StackFrame {
        vars: vec![],
        remaining_ops: vec![GcDestr::new(FnOp::Throw {
            what: Box::from(GcDestr::new(FnOp::LoadStatic { value: val })),
        })],
        ret_store: JsVar::new(Rc::new("ignored".into())),
    };
}

pub fn build_demo_fn() -> JsValue {
    return JsValue::Object {
        is_array: false,
        content: Gc::new(GcCell::new(Default::default())),
        call: GcCell::new(JSCallable::Js {
            content: Rc::new("a = 'demo function'; console.log(a)".to_string()),
            creator: Gc::new(JsFn {
                builder: Box::new(|store, args| {
                    let a_var = JsVar::new(Rc::new("a".into()));
                    let console_var = JsVar::new(Rc::new("console".into()));
                    return StackFrame {
                        vars: vec![a_var.clone()],
                        remaining_ops: vec![
                            GcDestr::new(FnOp::Assign {
                                target: a_var,
                                what: Box::from(GcDestr::new(FnOp::LoadStatic {
                                    value: JsValue::String(Rc::new("demo function".into())),
                                })),
                            }),
                            GcDestr::new(FnOp::LoadGlobal {
                                into: console_var.clone(),
                                name: Rc::new("console".to_string()),
                            }),
                            GcDestr::new(FnOp::CallFunction {
                                on: Box::from(GcDestr::from(FnOp::Deref {
                                    from: Box::from(GcDestr::new(FnOp::ReadVar {
                                        which: console_var,
                                    })),
                                    key: Box::from(GcDestr::new(FnOp::LoadStatic {
                                        value: JsValue::String(Rc::new("log".into())),
                                    })),
                                    from_store: None,
                                    key_store: None,
                                })),
                                arg_vars: vec![],
                                arg_fillers: vec![],
                            }),
                        ],
                        ret_store: store,
                    };
                }),
                tracer: Box::new(0),
            }),
        }),
    };
}

// Would I ever lie to you?
pub trait ReverseList<T> {
    fn rl_index(&self, i: usize) -> Option<&T>;
    fn rl_index_mut(&mut self, u: usize) -> Option<&mut T>;

    fn rl_front(&self) -> Option<&T>;
    fn rl_front_mut(&mut self) -> Option<&mut T>;

    fn rl_pop_front(&mut self) -> Option<T>;
    fn rl_push_front(&mut self, o: T);
    fn rl_append_front(&mut self, v: Vec<T>);
}

impl<T> ReverseList<T> for Vec<T> {
    fn rl_index(&self, i: usize) -> Option<&T> {
        return self.get(self.len() - i - 1);
    }

    fn rl_index_mut(&mut self, i: usize) -> Option<&mut T> {
        let index = self.len() - i - 1;
        return self.get_mut(index);
    }

    fn rl_front(&self) -> Option<&T> {
        return self.rl_index(0);
    }

    fn rl_front_mut(&mut self) -> Option<&mut T> {
        return self.rl_index_mut(0);
    }

    fn rl_pop_front(&mut self) -> Option<T> {
        return self.pop();
    }

    fn rl_push_front(&mut self, o: T) {
        self.push(o);
    }

    fn rl_append_front(&mut self, mut v: Vec<T>) {
        v.reverse();
        self.append(&mut v);
    }
}

#[derive(Clone, Trace, Finalize)]
pub enum FnOp {
    LoadGlobal {
        into: JsVar,
        name: Rc<String>,
    },
    Assign {
        target: JsVar,
        what: Box<GcDestr<FnOp>>,
    },
    LoadStatic {
        value: JsValue,
    },
    ReadVar {
        which: JsVar,
    },
    // Args are reversed for popping
    CallFunction {
        on: Box<GcDestr<FnOp>>,
        arg_vars: Vec<JsVar>,
        arg_fillers: Vec<GcDestr<FnOp>>,
    },
    Throw {
        what: Box<GcDestr<FnOp>>,
    },
    Return {
        what: Box<GcDestr<FnOp>>,
    },
    Deref {
        from: Box<GcDestr<FnOp>>,
        key: Box<GcDestr<FnOp>>,
        from_store: Option<JsValue>,
        key_store: Option<JsValue>,
    },
    IfElse {
        condition: Box<GcDestr<FnOp>>,
        if_block: Vec<GcDestr<FnOp>>,
        else_block: Vec<GcDestr<FnOp>>,
    },
    While {
        condition: Box<GcDestr<FnOp>>,
        block: Vec<GcDestr<FnOp>>,
    },
    For {
        initial: Box<GcDestr<FnOp>>,
        condition: Box<GcDestr<FnOp>>,
        each: Box<GcDestr<FnOp>>,
        block: Vec<GcDestr<FnOp>>,
    },
    Nop {},
    Multi {
        block: Vec<GcDestr<FnOp>>,
    },
    Await {},
}

impl FnOp {
    fn proto_from_global(name: &str, consumer: impl FnOnce(JsVar) -> FnOp) -> FnOpResult {
        let temp_var = JsVar::new(Rc::new("#temp#".into()));
        return FnOpResult::LoadGlobal {
            cost: 1,
            name: Rc::new(name.into()),
            into: temp_var.clone(),
            next: GcDestr::new(FnOp::Deref {
                from: Box::from(GcDestr::from(FnOp::Deref {
                    from: Box::from(GcDestr::from(FnOp::ReadVar {
                        which: temp_var.clone(),
                    })),
                    key: Box::from(GcDestr::from(FnOp::LoadStatic {
                        value: JsValue::String(Rc::new("__proto__".into())),
                    })),
                    from_store: None,
                    key_store: None,
                })),
                key: Box::from(GcDestr::from(consumer(temp_var))),
                from_store: None,
                key_store: None,
            }),
        };
    }

    fn forward_call(
        call: FnOpResult,
        then: impl FnOnce(GcDestr<FnOp>) -> GcDestr<FnOp>,
    ) -> FnOpResult {
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
        consumer: impl FnOnce(GcDestr<FnOp>) -> GcDestr<FnOp>,
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

    // TODO this needs to have a next
    fn forward_await(js_await: FnOpResult) -> FnOpResult {
        match js_await {
            FnOpResult::Await { into, cost } => FnOpResult::Await { cost, into },
            _ => {
                panic!("js_await needs ::Await")
            }
        }
    }

    fn do_dissolve(
        dissolver: FnOpResult,
        consumer: impl FnOnce(GcDestr<FnOp>) -> GcDestr<FnOp>,
    ) -> FnOpResult {
        match dissolver {
            FnOpResult::Dissolve { mut next, cost } => {
                let new_op = consumer(next.pop().unwrap());
                next.push(new_op);
                FnOpResult::Dissolve { next, cost }
            }
            _ => panic!("wrong option passed to do_dissolve"),
        }
    }

    fn run(mut this: GcDestr<FnOp>, max_cost: u64) -> FnOpResult {
        if max_cost == 0 {
            return FnOpResult::Ongoing {
                cost: 0,
                next: this.destroy_move(),
            };
        }
        match this.deref_mut() {
            FnOp::Assign { target, what } => {
                let mut result = FnOp::run(what.destroy_move(), max_cost - 1);
                match result {
                    FnOpResult::Throw { .. } => {
                        return result;
                    }
                    FnOpResult::Return { .. } => {
                        return result;
                    }
                    FnOpResult::Ongoing { cost, ref mut next } => {
                        return FnOpResult::Ongoing {
                            cost,
                            next: GcDestr::new(FnOp::Assign {
                                target: target.clone(),
                                what: Box::new(next.destroy_move()),
                            }),
                        };
                    }
                    FnOpResult::Dissolve { mut next, cost } => {
                        let new_what = Box::new(next.pop().unwrap());
                        next.push(GcDestr::new(FnOp::Assign {
                            target: target.clone(),
                            what: new_what,
                        }));
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
                        return FnOp::forward_call(result, |op| {
                            GcDestr::new(FnOp::Assign {
                                target: target.clone(),
                                what: Box::from(op),
                            })
                        })
                    }
                    FnOpResult::LoadGlobal { .. } => {
                        return FnOp::forward_load_global(result, move |loaded| {
                            GcDestr::new(FnOp::Assign {
                                target: target.clone(),
                                what: Box::from(loaded),
                            })
                        })
                    }
                    FnOpResult::Await { .. } => {
                        return result;
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
                    what: Deref::deref(&GcCell::borrow(&which.value)).clone(),
                }
            }
            FnOp::CallFunction {
                on,
                ref mut arg_vars,
                arg_fillers,
            } => {
                if arg_fillers.is_empty() {
                    let result = FnOp::run(on.destroy_move(), max_cost);
                    return match result {
                        FnOpResult::Dissolve { .. } => FnOp::do_dissolve(result, |then| {
                            GcDestr::new(FnOp::CallFunction {
                                on: Box::from(then),
                                arg_vars: take(arg_vars),
                                arg_fillers: vec![],
                            })
                        }),
                        FnOpResult::Throw { .. } => result,
                        FnOpResult::Return { .. } => result,
                        FnOpResult::Value { cost, what } => {
                            let temp_var = JsVar::new(Rc::new("#result_holder#".into()));
                            FnOpResult::Call {
                                cost: cost + 1,
                                on: what,
                                args: take(arg_vars),
                                result: temp_var.clone(),
                                next: GcDestr::new(FnOp::ReadVar { which: temp_var }),
                            }
                        }
                        FnOpResult::Ongoing { cost, next } => FnOpResult::Ongoing {
                            cost,
                            next: GcDestr::new(FnOp::CallFunction {
                                on: Box::from(next),
                                arg_vars: take(arg_vars),
                                arg_fillers: vec![],
                            }),
                        },
                        FnOpResult::Call { .. } => FnOp::forward_load_global(result, |op| {
                            GcDestr::new(FnOp::CallFunction {
                                on: Box::from(op),
                                arg_vars: arg_vars.clone(),
                                arg_fillers: vec![],
                            })
                        }),
                        FnOpResult::LoadGlobal { .. } => {
                            FnOp::forward_load_global(result, |loaded| {
                                GcDestr::new(FnOp::CallFunction {
                                    on: Box::from(loaded),
                                    arg_vars: take(arg_vars),
                                    arg_fillers: vec![],
                                })
                            })
                        }
                        FnOpResult::Await { .. } => {
                            return result;
                        }
                    };
                }
                return FnOpResult::Dissolve {
                    next: arg_fillers
                        .drain(..)
                        .zip((&mut arg_vars.clone()).drain(..))
                        .map(|filler_var| {
                            GcDestr::new(FnOp::Assign {
                                target: filler_var.1.clone(),
                                what: Box::from(filler_var.0),
                            })
                        })
                        .chain(vec![GcDestr::new(FnOp::CallFunction {
                            on: Box::from(on.destroy_move()),
                            arg_vars: take(arg_vars),
                            arg_fillers: vec![],
                        })])
                        .collect(),
                    cost: 0,
                };
            }
            FnOp::Throw { what } => {
                let result = FnOp::run(what.destroy_move(), max_cost);
                return match result {
                    FnOpResult::Dissolve { cost, mut next } => {
                        let new_what = Box::new(next.pop().unwrap());
                        next.push(GcDestr::new(FnOp::Throw { what: new_what }));
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
                        next: GcDestr::new(FnOp::Throw {
                            what: Box::from(next),
                        }),
                    },
                    FnOpResult::Call { .. } => FnOp::forward_call(result, |temp_var| {
                        GcDestr::new(FnOp::Throw {
                            what: Box::from(temp_var),
                        })
                    }),
                    FnOpResult::LoadGlobal { .. } => FnOp::forward_load_global(result, |op| {
                        GcDestr::new(FnOp::Throw {
                            what: Box::from(op),
                        })
                    }),
                    FnOpResult::Await { .. } => {
                        return result;
                    }
                };
            }
            FnOp::Deref {
                from: _,
                key: _,
                ref mut from_store,
                ref mut key_store,
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
                            JsValue::Number(_) => {
                                let temp_var = JsVar::new(Rc::new("#temp_var#".into()));
                                return FnOpResult::LoadGlobal {
                                    cost: 1,
                                    name: Rc::new("Number".into()),
                                    into: temp_var.clone(),
                                    next: GcDestr::new(FnOp::Deref {
                                        from: Box::from(GcDestr::new(FnOp::ReadVar {
                                            which: temp_var,
                                        })),
                                        key: Box::from(GcDestr::from(FnOp::LoadStatic {
                                            value: take(key_stored),
                                        })),
                                        from_store: None,
                                        key_store: None,
                                    }),
                                };
                            }
                            JsValue::Boolean(_) => {
                                return FnOp::proto_from_global("Boolean", |proto| FnOp::Deref {
                                    from: Box::from(GcDestr::from(FnOp::ReadVar { which: proto })),
                                    key: Box::from(GcDestr::from(FnOp::LoadStatic {
                                        value: take(key_stored),
                                    })),
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
                let condition_result = FnOp::run(condition.destroy_move(), max_cost);
                match condition_result {
                    FnOpResult::Dissolve { cost, mut next } => {
                        let new_condition = Box::new(next.pop().unwrap());
                        next.push(GcDestr::new(FnOp::IfElse {
                            condition: new_condition,
                            if_block: take(if_block),
                            else_block: take(else_block),
                        }));
                        return FnOpResult::Dissolve { next, cost };
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
                                next: take(if_block),
                                cost,
                            }
                        } else {
                            FnOpResult::Dissolve {
                                next: take(else_block),
                                cost,
                            }
                        }
                    }
                    FnOpResult::Ongoing { cost, next } => {
                        return FnOpResult::Ongoing {
                            cost,
                            next: GcDestr::new(FnOp::IfElse {
                                condition: Box::from(next),
                                if_block: take(if_block),
                                else_block: take(else_block),
                            }),
                        }
                    }
                    FnOpResult::Call { .. } => {
                        return FnOp::forward_call(condition_result, |op| {
                            GcDestr::new(FnOp::IfElse {
                                condition: Box::new(op),
                                if_block: take(if_block),
                                else_block: take(else_block),
                            })
                        });
                    }
                    FnOpResult::LoadGlobal { .. } => {
                        return FnOp::forward_load_global(condition_result, |op| {
                            GcDestr::new(FnOp::IfElse {
                                condition: Box::from(op),
                                if_block: take(if_block),
                                else_block: take(else_block),
                            })
                        })
                    }
                    FnOpResult::Await { .. } => {
                        return condition_result;
                    }
                }
            }
            FnOp::While { condition, block } => {
                let next_loop = GcDestr::new(FnOp::While {
                    condition: Box::from(condition.destroy_move()),
                    block: block.clone(),
                });
                let mut block = take(block);
                block.push(next_loop);
                return FnOpResult::Ongoing {
                    cost: 1,
                    next: GcDestr::new(FnOp::IfElse {
                        condition: Box::from(condition.destroy_move()),
                        if_block: block,
                        else_block: vec![],
                    }),
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
                        initial.destroy_move(),
                        GcDestr::new(FnOp::While {
                            condition: Box::from(condition.destroy_move()),
                            block: vec![
                                each.destroy_move(),
                                GcDestr::new(FnOp::Multi { block: take(block) }),
                            ],
                        }),
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
                    next: take(block),
                    cost: 1,
                }
            }
            FnOp::Await { .. } => {
                let temp_var = JsVar::new(Rc::new("#temp'".into()));
                // Compiler makes sure we are registered for being called.
                return FnOpResult::Await {
                    cost: 0,
                    into: temp_var,
                };
            }
            FnOp::LoadGlobal { .. } => {}
            FnOp::Return { .. } => {}
        };
        unimplemented!()
    }
}

pub enum FnOpResult {
    Dissolve {
        next: Vec<GcDestr<FnOp>>,
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
        next: GcDestr<FnOp>,
    },
    Call {
        cost: u64,
        on: JsValue,
        args: Vec<JsVar>,
        result: JsVar,
        next: GcDestr<FnOp>,
    },
    LoadGlobal {
        cost: u64,
        name: Rc<String>,
        into: JsVar,
        next: GcDestr<FnOp>,
    },
    Await {
        cost: u64,
        into: JsVar,
    },
}

#[derive(Trace, Finalize)]
pub struct StackFrame {
    vars: Vec<JsVar>,
    remaining_ops: Vec<GcDestr<FnOp>>, // REVERSE ORDER list of remaining ops. Using pop
    ret_store: JsVar,
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

    fn set(&self, val: JsValue) {
        *self.value.borrow_mut() = val;
    }

    fn get(&self) -> JsValue {
        GcCellRef::deref(&GcCell::borrow(self.value.borrow())).clone()
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
