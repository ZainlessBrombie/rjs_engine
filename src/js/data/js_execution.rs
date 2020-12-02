use crate::js::data::gc_util::GcDestr;
use crate::js::data::js_types::{JSCallable, JsFn, JsProperty, JsValue};
use gc::{Finalize, GcCellRef, Trace};
use gc::{Gc, GcCell};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::mem::take;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use std::sync::{Arc, Mutex};

type ExtCallType = Arc<Mutex<Vec<Box<dyn FnOnce(&mut dyn FnMut(Gc<GcCell<AsyncStack>>))>>>>;

pub struct EngineState {
    pub(crate) tick_queue: Vec<Gc<GcCell<AsyncStack>>>,
    pub(crate) external_calls: ExtCallType,
    //channel_write: std::sync::mpsc::Sender<Box<dyn FnOnce()>>,
    //channel_read: std::sync::mpsc::Receiver<Box<dyn FnOnce()>>,
}

#[derive(Clone)]
pub struct EngineQueuer {
    queue: ExtCallType,
}

impl EngineQueuer {
    pub fn enqueue_js_fn(&mut self, val: JsValue) {
        let mut guard = self
            .queue
            .lock()
            .expect("Mutex poisoned. This is an internal error.");
        guard.push(Box::new(|cb| {
            cb(Gc::new(GcCell::new(AsyncStack {
                stack: vec![StackFrame {
                    vars: vec![],
                    remaining_ops: vec![GcDestr::new(FnOp::CallFunction {
                        on: Box::new(GcDestr::new(FnOp::LoadStatic { value: val })),
                        arg_vars: vec![],
                        arg_fillers: vec![],
                    })],
                    ret_store: JsVar::new(Rc::new("#ignored#".into())),
                }],
            })))
        }));
    }
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
                    break;
                }
            }
            let ext_call_clone = self.external_calls.clone();
            let mut guard = ext_call_clone
                .lock()
                .expect("Mutex poisoned. This is an internal error.");
            if !guard.is_empty() {
                for ext_call in guard.drain(..) {
                    (ext_call)(&mut |tick| self.tick_queue.push(tick));
                }
            }
            if consumed >= ticks || self.tick_queue.is_empty() {
                return consumed;
            }
        }
    }

    pub fn get_queuer(&self) -> EngineQueuer {
        EngineQueuer {
            queue: self.external_calls.clone(),
        }
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
                                last.remaining_ops.rl_prepend_front(next);
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
                                    println!("Uncaught Promise!\n{}", what.to_system_string());
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
                                self.stack.push(call_to_js_stack(on, result, args));
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
                                    last.remaining_ops.rl_push_front(GcDestr::new(FnOp::Multi {
                                        block: vec![
                                            GcDestr::new(FnOp::Assign {
                                                target: into,
                                                what: Box::from(GcDestr::new(FnOp::LoadStatic {
                                                    value: JsValue::Object {
                                                        is_array: false,
                                                        content: Gc::new(GcCell::new(console_map)),
                                                        call: GcCell::new(JSCallable::NotCallable),
                                                    },
                                                })),
                                            }),
                                            next,
                                        ],
                                    }));
                                }
                            }
                            FnOpResult::Await { into, next, cost } => {
                                last.remaining_ops.rl_push_front(next); // TODO
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

fn call_to_js_stack(val: JsValue, ret_val: JsVar, mut args: Vec<JsVar>) -> StackFrame {
    return match &val {
        JsValue::String(s) => throw_frame(JsValue::from_string("[string] is not a function")),
        JsValue::Object { call, .. } => match GcCellRef::deref(&GcCell::borrow(&call)) {
            JSCallable::NotCallable => {
                throw_frame(JsValue::from_string("[object Object] is not a function"))
            }
            JSCallable::Js { creator, .. } => {
                (creator.builder)(ret_val, args.drain(..).map(|v| v.get()).collect())
            }
            JSCallable::Native { creator } => {
                (creator.builder)(ret_val, args.drain(..).map(|v| v.get()).collect())
            }
        },
        v => throw_frame(JsValue::from_string(
            format!("{} is not a function", v.to_system_string()).as_str(),
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
            content: Rc::new("a = 'Hello Wonderful World!'; console.log(a)".to_string()),
            creator: Gc::new(JsFn {
                builder: Box::new(|store, args| {
                    let a_var = JsVar::new(Rc::new("a".into()));
                    let arg_var = JsVar::new(Rc::new("#arg_var_1#".into()));
                    return StackFrame {
                        vars: vec![a_var.clone()],
                        remaining_ops: vec![
                            GcDestr::new(FnOp::CallFunction {
                                on: Box::from(GcDestr::from(FnOp::Deref {
                                    from: Box::from(GcDestr::new(FnOp::LoadGlobal {
                                        name: Rc::new("console".to_string()),
                                    })),
                                    key: Box::from(GcDestr::new(FnOp::LoadStatic {
                                        value: JsValue::String(Rc::new("log".into())),
                                    })),
                                    from_store: JsVar::new(Rc::new("#deref_from_store#".into())),
                                    key_store: JsVar::new(Rc::new("#deref_key_store#".into())),
                                    done: JsVar::new(Rc::new("#deref_is_done#".into())),
                                })),
                                arg_vars: vec![arg_var.clone()],
                                arg_fillers: vec![GcDestr::new(FnOp::Assign {
                                    target: arg_var,
                                    what: Box::from(GcDestr::new(FnOp::ReadVar {
                                        which: a_var.clone(),
                                    })),
                                })],
                            }),
                            GcDestr::new(FnOp::Assign {
                                target: a_var,
                                what: Box::from(GcDestr::new(FnOp::LoadStatic {
                                    value: JsValue::String(Rc::new(
                                        "Hello Wonderful World!".into(),
                                    )),
                                })),
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
    fn rl_prepend_front(&mut self, v: Vec<T>);
}

impl<T> ReverseList<T> for Vec<T> {
    fn rl_index(&self, i: usize) -> Option<&T> {
        if self.len() < i + 1 {
            return None;
        }
        return self.get(self.len() - i - 1);
    }

    fn rl_index_mut(&mut self, i: usize) -> Option<&mut T> {
        if self.len() < i + 1 {
            return None;
        }
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

    fn rl_prepend_front(&mut self, mut v: Vec<T>) {
        v.reverse();
        self.append(&mut v);
    }
}

#[derive(Clone, Trace, Finalize)]
pub enum FnOp {
    LoadGlobal {
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
        from_store: JsVar,
        key_store: JsVar,
        done: JsVar,
    },
    IfElse {
        condition: Box<GcDestr<FnOp>>,
        if_block: Box<GcDestr<FnOp>>,
        else_block: Box<GcDestr<FnOp>>,
    },
    While {
        condition: Box<GcDestr<FnOp>>,
        block: Box<GcDestr<FnOp>>,
    },
    For {
        initial: Box<GcDestr<FnOp>>,
        condition: Box<GcDestr<FnOp>>,
        each: Box<GcDestr<FnOp>>,
        block: Box<GcDestr<FnOp>>,
    },
    Nop {},
    Multi {
        block: Vec<GcDestr<FnOp>>,
    },
    Await {},
}

impl FnOp {
    fn proto_from_global(name: &str, consumer: impl FnOnce(JsVar) -> GcDestr<FnOp>) -> FnOpResult {
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
                    from_store: JsVar::new(Rc::new("#from_store#".into())),
                    key_store: JsVar::new(Rc::new("#key_store#".into())),
                    done: JsVar::new(Rc::new("#done_store#".into())),
                })),
                key: Box::from(consumer(temp_var)),
                from_store: JsVar::new(Rc::new("#from_store#".into())),
                key_store: JsVar::new(Rc::new("#key_store#".into())),
                done: JsVar::new(Rc::new("#done_store#".into())),
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

    fn forward_await(
        js_await: FnOpResult,
        consumer: impl FnOnce(GcDestr<FnOp>) -> GcDestr<FnOp>,
    ) -> FnOpResult {
        match js_await {
            FnOpResult::Await { into, cost, next } => FnOpResult::Await {
                cost,
                into,
                next: consumer(next),
            },
            _ => {
                panic!("js_await needs ::Await")
            }
        }
    }

    fn throw_internal(reason: &str) -> GcDestr<FnOp> {
        GcDestr::new(FnOp::Throw {
            what: Box::new(GcDestr::new(FnOp::LoadStatic {
                value: JsValue::String(Rc::new(format!("internal error: {}", reason))),
            })),
        })
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
        return match this.deref_mut() {
            FnOp::Assign { target, what } => {
                let mut result = FnOp::run(what.destroy_move(), max_cost - 1);
                match result {
                    FnOpResult::Throw { .. } => result,
                    FnOpResult::Return { .. } => result,
                    FnOpResult::Ongoing { cost, ref mut next } => FnOpResult::Ongoing {
                        cost,
                        next: GcDestr::new(FnOp::Assign {
                            target: target.clone(),
                            what: Box::new(next.destroy_move()),
                        }),
                    },
                    FnOpResult::Dissolve { mut next, cost } => {
                        let new_what = Box::new(next.pop().unwrap());
                        next.push(GcDestr::new(FnOp::Assign {
                            target: target.clone(),
                            what: new_what,
                        }));
                        FnOpResult::Dissolve { next, cost }
                    }
                    FnOpResult::Value { what, cost } => {
                        (*target.value.try_borrow_mut().unwrap().deref_mut()) = what.clone();
                        FnOpResult::Value {
                            cost: cost + 1,
                            what,
                        }
                    }
                    FnOpResult::Call { .. } => FnOp::forward_call(result, |op| {
                        GcDestr::new(FnOp::Assign {
                            target: target.clone(),
                            what: Box::from(op),
                        })
                    }),
                    FnOpResult::LoadGlobal { .. } => {
                        FnOp::forward_load_global(result, move |loaded| {
                            GcDestr::new(FnOp::Assign {
                                target: target.clone(),
                                what: Box::from(loaded),
                            })
                        })
                    }
                    FnOpResult::Await { .. } => result,
                }
            }
            FnOp::LoadStatic { value } => FnOpResult::Value {
                cost: 1,
                what: value.clone(),
            },
            FnOp::ReadVar { which } => FnOpResult::Value {
                cost: 1,
                what: Deref::deref(&GcCell::borrow(&which.value)).clone(),
            },
            FnOp::CallFunction {
                on,
                ref mut arg_vars,
                arg_fillers,
            } => {
                if arg_fillers.is_empty() {
                    let result = FnOp::run(on.destroy_move(), max_cost);
                    match result {
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
                    }
                } else {
                    FnOpResult::Dissolve {
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
                        cost: 1,
                    }
                }
            }
            FnOp::Throw { what } => {
                let result = FnOp::run(what.destroy_move(), max_cost);
                match result {
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
                }
            }
            FnOp::Deref {
                from,
                key,
                from_store,
                key_store,
                done,
            } => {
                if GcCellRef::deref(&GcCell::borrow(&done.value)).truthy() {
                    match GcCellRef::deref(&GcCell::borrow(&from_store.value)) {
                        JsValue::Undefined => FnOpResult::Throw {
                            cost: 1,
                            what: JsValue::String(
                                format!(
                                    "Cannot read value {} of undefined",
                                    GcCellRef::deref(&GcCell::borrow(&from_store.value))
                                        .to_system_string()
                                )
                                .into(),
                            ),
                        },
                        JsValue::Null => FnOpResult::Throw {
                            cost: 1,
                            what: JsValue::String(
                                format!(
                                    "Cannot read value {} of null",
                                    GcCellRef::deref(&GcCell::borrow(&from_store.value))
                                        .to_system_string()
                                )
                                .into(),
                            ),
                        },
                        JsValue::Number(_) => {
                            FnOp::proto_from_global("Number", |proto_var| {
                                GcDestr::new(FnOp::Deref {
                                    from: Box::new(GcDestr::new(FnOp::ReadVar {
                                        which: proto_var,
                                    })),
                                    key: Box::new(GcDestr::new(FnOp::ReadVar {
                                        which: key_store.clone(),
                                    })),
                                    from_store: from_store.clone(),
                                    key_store: key_store.clone(),
                                    done: JsVar::new(Rc::new("#done_marker#".into())),
                                })
                            }) // TODO proto loops
                        }
                        JsValue::Boolean(_) => FnOp::proto_from_global("Boolean", |proto_var| {
                            GcDestr::new(FnOp::Deref {
                                from: Box::new(GcDestr::new(FnOp::ReadVar { which: proto_var })),
                                key: Box::new(GcDestr::new(FnOp::ReadVar {
                                    which: key_store.clone(),
                                })),
                                from_store: from_store.clone(),
                                key_store: key_store.clone(),
                                done: JsVar::new(Rc::new("#done_marker#".into())),
                            })
                        }),
                        JsValue::String(s) => {
                            let k = GcCellRef::deref(&GcCell::borrow(&key_store.value))
                                .to_system_string();
                            if let Ok(index) = k.parse::<i32>() {
                                let letter_at = &s[(index as usize)..((index + 1) as usize)];
                                // ^ TODO check: safe? ^
                                {
                                    FnOpResult::Value {
                                        cost: 1,
                                        what: JsValue::String(Rc::new(letter_at.into())),
                                    }
                                }
                            } else {
                                FnOp::proto_from_global("String", |proto_var| {
                                    GcDestr::new(FnOp::Deref {
                                        from: Box::new(GcDestr::new(FnOp::ReadVar {
                                            which: proto_var,
                                        })),
                                        key: Box::new(GcDestr::new(FnOp::ReadVar {
                                            which: key_store.clone(),
                                        })),
                                        from_store: from_store.clone(),
                                        key_store: key_store.clone(),
                                        done: JsVar::new(Rc::new("#done_marker#".into())),
                                    })
                                })
                            }
                        }
                        JsValue::Object { content, .. } => {
                            if let Some(found_value) = GcCell::borrow(&content).get(
                                &GcCellRef::deref(&GcCell::borrow(&key_store.value))
                                    .to_system_string(),
                            ) {
                                FnOpResult::Value {
                                    cost: 1,
                                    what: (&GcCell::borrow(&found_value.value) as &JsValue).clone(),
                                }
                            } else {
                                FnOpResult::Value {
                                    cost: 1,
                                    what: JsValue::Undefined,
                                }
                            }
                        }
                    }
                } else {
                    FnOpResult::Dissolve {
                        next: vec![
                            GcDestr::new(FnOp::Assign {
                                target: key_store.clone(),
                                what: Box::new(key.destroy_move()),
                            }),
                            GcDestr::new(FnOp::Assign {
                                target: from_store.clone(),
                                what: Box::new(from.destroy_move()),
                            }),
                            GcDestr::new(FnOp::Assign {
                                target: done.clone(),
                                what: Box::new(GcDestr::new(FnOp::LoadStatic {
                                    value: JsValue::Boolean(true),
                                })),
                            }),
                            GcDestr::new(FnOp::Deref {
                                from: Box::new(FnOp::throw_internal(
                                    "tried double load for deref (1)",
                                )),
                                key: Box::new(FnOp::throw_internal(
                                    "tried double load for deref (2)",
                                )),
                                from_store: from_store.clone(),
                                key_store: key_store.clone(),
                                done: done.clone(),
                            }),
                        ],
                        cost: 1,
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
                        FnOpResult::Dissolve { next, cost }
                    }
                    FnOpResult::Throw { .. } => condition_result,
                    FnOpResult::Return { .. } => condition_result,
                    FnOpResult::Value { cost, what } => {
                        if what.truthy() {
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
                    FnOpResult::Ongoing { cost, next } => FnOpResult::Ongoing {
                        cost,
                        next: GcDestr::new(FnOp::IfElse {
                            condition: Box::from(next),
                            if_block: take(if_block),
                            else_block: take(else_block),
                        }),
                    },
                    FnOpResult::Call { .. } => FnOp::forward_call(condition_result, |op| {
                        GcDestr::new(FnOp::IfElse {
                            condition: Box::new(op),
                            if_block: take(if_block),
                            else_block: take(else_block),
                        })
                    }),
                    FnOpResult::LoadGlobal { .. } => {
                        FnOp::forward_load_global(condition_result, |op| {
                            GcDestr::new(FnOp::IfElse {
                                condition: Box::from(op),
                                if_block: take(if_block),
                                else_block: take(else_block),
                            })
                        })
                    }
                    FnOpResult::Await { .. } => condition_result,
                }
            }
            FnOp::While { condition, block } => {
                let next_loop = GcDestr::new(FnOp::While {
                    condition: Box::from(condition.destroy_move()),
                    block: block.clone(),
                });
                let mut block = take(block);
                block.push(next_loop);
                FnOpResult::Ongoing {
                    cost: 1,
                    next: GcDestr::new(FnOp::IfElse {
                        condition: Box::from(condition.destroy_move()),
                        if_block: block,
                        else_block: vec![],
                    }),
                }
            }
            FnOp::For {
                initial,
                condition,
                each,
                block,
            } => FnOpResult::Dissolve {
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
            },
            FnOp::Nop { .. } => FnOpResult::Value {
                cost: 1,
                what: JsValue::Undefined,
            },
            FnOp::Multi { block } => FnOpResult::Dissolve {
                next: take(block),
                cost: 1,
            },
            FnOp::Await { .. } => {
                let temp_var = JsVar::new(Rc::new("#temp'".into()));
                // Compiler makes sure we are registered for being called.
                FnOpResult::Await {
                    cost: 0,
                    into: temp_var.clone(),
                    next: GcDestr::new(FnOp::ReadVar { which: temp_var }),
                }
            }
            FnOp::LoadGlobal { name } => {
                let temp_var = JsVar::new(Rc::new("#temp_var#".into()));
                FnOpResult::LoadGlobal {
                    cost: 1,
                    name: name.clone(),
                    into: temp_var.clone(),
                    next: GcDestr::new(FnOp::ReadVar { which: temp_var }),
                }
            }
            FnOp::Return { what } => {
                let ret = FnOp::run(what.destroy_move(), max_cost);
                match ret {
                    FnOpResult::Dissolve { .. } => FnOp::do_dissolve(ret, |result| {
                        GcDestr::new(FnOp::Return {
                            what: Box::from(result),
                        })
                    }),
                    FnOpResult::Throw { .. } => ret,
                    FnOpResult::Return { .. } => ret,
                    FnOpResult::Value { cost, what } => FnOpResult::Return {
                        cost: cost + 1,
                        what,
                    },
                    FnOpResult::Ongoing { cost, next } => FnOpResult::Ongoing {
                        cost: cost + 1,
                        next: GcDestr::new(FnOp::Return {
                            what: Box::from(next),
                        }),
                    },
                    FnOpResult::Call { .. } => FnOp::forward_call(ret, |fn_retval| {
                        GcDestr::new(FnOp::Return {
                            what: Box::from(fn_retval),
                        })
                    }),
                    FnOpResult::LoadGlobal { .. } => FnOp::forward_load_global(ret, |global| {
                        GcDestr::new(FnOp::Return {
                            what: Box::from(global),
                        })
                    }),
                    FnOpResult::Await { .. } => FnOp::forward_await(ret, |what| {
                        GcDestr::new(FnOp::Return {
                            what: Box::from(what),
                        })
                    }),
                }
            }
        };
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
        next: GcDestr<FnOp>,
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

    pub fn new(name: Rc<String>) -> JsVar {
        return JsVar {
            name,
            value: Gc::new(GcCell::new(JsValue::Undefined)),
            defined: true,
        };
    }

    pub fn set(&self, val: JsValue) {
        *self.value.borrow_mut() = val;
    }

    pub fn get(&self) -> JsValue {
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
