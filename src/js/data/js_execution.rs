use crate::js::data::gc_util::GcDestr;
use crate::js::data::js_execution::FnOpResult::LoadThis;
use crate::js::data::js_types::{Identity, JSCallable, JsFn, JsProperty, JsValue};
use crate::js::data::util::{
    s_pool, u_and, u_array_e, u_block, u_call, u_capture_deref, u_deref, u_function, u_if,
    u_if_else, u_literal, u_load_global, u_not, u_read_var, u_strict_comp, u_string, u_typeof,
    u_undefined, u_write_var, JsObjectBuilder,
};
use gc::{Finalize, GcCellRef, Trace};
use gc::{Gc, GcCell};
use std::borrow::Borrow;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::f64::NAN;
use std::marker::PhantomData;
use std::mem::take;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use std::sync::{Arc, Mutex};

type ExtCallType = Arc<Mutex<Vec<Box<dyn FnOnce(&mut dyn FnMut(Gc<GcCell<AsyncStack>>))>>>>;

pub struct EngineState {
    pub(crate) tick_queue: Vec<Gc<GcCell<AsyncStack>>>,
    pub(crate) external_calls: ExtCallType,
}

#[derive(Clone)]
pub struct EngineQueuer {
    queue: ExtCallType,
}

impl EngineQueuer {
    /// Pushes a js function into the event loop.
    /// Does not have an inherent this - context
    pub fn enqueue_js_fn(&mut self, val: JsValue) {
        let mut guard = self
            .queue
            .lock()
            .expect("Mutex poisoned. This is an internal error.");
        guard.push(Box::new(|cb| {
            cb(Gc::new(GcCell::new(AsyncStack {
                stack: vec![StackFrame {
                    vars: vec![],
                    remaining_ops: vec![GcDestr::new(FnOpRepr::CallFunction {
                        on: Box::new(GcDestr::new(FnOpRepr::LoadStatic { value: val })),
                        arg_array: Box::new(u_literal(u_array_e())),
                        on_var: JsVar::new_t(),
                        args_var: JsVar::new_t(),
                        this_var: JsVar::new_t(),
                        ready: JsVar::new_t(),
                    })],
                    this: Default::default(),
                    ret_store: JsVar::new(Rc::new("#ignored#".into())),
                }],
            })))
        }));
    }
}

impl EngineState {
    pub fn queuer(&self) -> EngineQueuer {
        EngineQueuer {
            queue: self.external_calls.clone(),
        }
    }

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
    stack: Vec<StackElement>, // values first, ops second - otherwise we would run into a value while evaluating!
    method_label: Identity,
    try_label: Identity,
}

enum AsyncStackResult {
    Forget,
    Keep,
}

struct StackAccess<'a> {
    stack: &'a mut AsyncStack,
    latest_fn_pos: usize,
}

/**
Stack layout:
 0) return value
 1) do not use: other functions continuation op
 3) return-label
 4) this
 5) args-array
 5..n) local variables
 n..m) code, local variables
*/
impl<'a> StackAccess<'a> {
    /// Read a stack element, the top being 0
    pub fn read_stack(&'a self, pos: usize) -> &'a StackElement {
        // self.latest_fn_pos
        self.stack
            .stack
            .get(pos + self.latest_fn_pos)
            .expect("corrupt stack (3)")
    }

    pub fn make_local(&mut self) -> JsVar {
        JsVar::Stack {
            pos: self.stack.stack.len() - self.latest_fn_pos,
            name: Rc::new("".to_string()),
        }
    }

    pub fn write_stack(&'a mut self, pos: usize) -> &'a mut StackElement {
        self.stack.stack.get_mut(pos).expect("corrupt stack (4)")
    }

    pub fn method_label(&self) -> Identity {
        self.stack.method_label.clone()
    }

    pub fn try_label(&self) -> Identity {
        self.stack.method_label.clone()
    }
}

impl AsyncStack {
    fn run(&mut self, max: u64) -> (AsyncStackResult, u64) {
        let mut consumed = 0;

        let mut access = StackAccess {
            stack: self,
            latest_fn_pos: 0,
        };

        loop {
            if consumed >= max {
                return (AsyncStackResult::Keep, consumed);
            }

            if let Some(last) = self.stack.pop() {
                let stack_op = match last {
                    StackElement::Op(op) => op,
                    _ => {
                        panic!("Corrupt stack (1)")
                    }
                };
                let action = FnOpRepr::run(stack_op, &mut access);
                match action {
                    FnOpAction::Pop(pop) => self.stack.truncate(pop),
                    FnOpAction::Push(mut push) => self.stack.append(&mut push),
                    FnOpAction::One(one) => self.stack.push(one),
                    FnOpAction::LoadGlobal { name } => {
                        if name.as_str() == "console" {
                            self.stack.push(StackElement::Value(u_undefined()));
                            // TODO
                        }
                    }
                    FnOpAction::PushLabel { id } => self.stack.push(StackElement::Label(id)),
                    FnOpAction::LabelWalkback { id } => {
                        let value = self.stack.pop().expect("corrupt stack (2)");
                        while let Some(el) = self.stack.pop() {
                            match el {
                                StackElement::Value(_) => {}
                                StackElement::Label(label) => {
                                    if label == id {
                                        if label == self.method_label {}
                                        break;
                                    }
                                }
                                StackElement::Op(_) => {}
                            }
                        }
                        self.stack.push(value);
                    }
                };
                consumed += 1;
            } else {
                return (AsyncStackResult::Forget, consumed);
            }
        }
    }
}

fn call_to_js_stack(val: JsValue, ret_val: JsVar, args: JsValue, this: JsValue) -> StackFrame {
    return match &val {
        JsValue::String(s) => throw_frame(JsValue::from_string("[string] is not a function")),
        JsValue::Object(obj) => match &(&GcCell::borrow(&obj)).call {
            JSCallable::NotCallable => {
                throw_frame(JsValue::from_string("[object Object] is not a function"))
            }
            JSCallable::Js { creator, .. } => creator.call(ret_val, args, this),
            JSCallable::Native { creator } => creator.call(ret_val, args, this),
        },
        v => throw_frame(JsValue::from_string(
            format!("{} is not a function", v.to_system_string()).as_str(),
        )),
    };
}

pub fn build_demo_fn() -> JsValue {
    let a = JsVar::new(Rc::new("a".into()));
    return JsObjectBuilder::new(None)
        .with_callable(JSCallable::Js {
            content: Rc::new("".to_string()),
            creator: Gc::new(JsFn::js_value_call(u_function(u_block(vec![
                u_write_var(a.clone(), u_literal(u_string("heyho"))),
                u_call(
                    u_deref(u_load_global("console"), u_literal(u_string("log"))),
                    u_read_var(a),
                ),
            ])))),
        })
        .build();
}

#[derive(Clone, Trace, Finalize)]
pub enum FnOpRepr {
    LoadGlobal {
        name: Rc<String>,
    },
    Assign {
        target: JsVar,
        what: Box<GcDestr<FnOpRepr>>,
    },
    LoadStatic {
        value: JsValue,
    },
    ReadVar {
        which: JsVar,
    },
    CallFunction {
        this_override: Option<JsValue>,
        on: Box<GcDestr<FnOpRepr>>,
        arg_array: Box<GcDestr<FnOpRepr>>,
    },
    Throw {
        what: Box<GcDestr<FnOpRepr>>,
    },
    Return {
        what: Box<GcDestr<FnOpRepr>>,
    },
    Deref {
        from: Box<GcDestr<FnOpRepr>>,
        key: Box<GcDestr<FnOpRepr>>,
    },
    IfElse {
        condition: Box<GcDestr<FnOpRepr>>,
        if_block: Box<GcDestr<FnOpRepr>>,
        else_block: Box<GcDestr<FnOpRepr>>,
    },
    While {
        condition: Box<GcDestr<FnOpRepr>>,
        block: Box<GcDestr<FnOpRepr>>,
    },
    For {
        initial: Box<GcDestr<FnOpRepr>>,
        condition: Box<GcDestr<FnOpRepr>>,
        each: Box<GcDestr<FnOpRepr>>,
        block: Box<GcDestr<FnOpRepr>>,
    },
    Nop {},
    Multi {
        block: Vec<GcDestr<FnOpRepr>>,
    },
    BoolAnd {
        left: Box<GcDestr<FnOpRepr>>,
        right: Box<GcDestr<FnOpRepr>>,
    },
    BoolNot {
        of: Box<GcDestr<FnOpRepr>>,
    },
    BoolOr {
        left: Box<GcDestr<FnOpRepr>>,
        right: Box<GcDestr<FnOpRepr>>,
    },
    FuzzyCompare {
        left: Box<GcDestr<FnOpRepr>>,
        right: Box<GcDestr<FnOpRepr>>,
        l_store: JsVar,
        r_store: JsVar,
        done: JsVar,
    },
    StrictCompare {
        left: Box<GcDestr<FnOpRepr>>,
        right: Box<GcDestr<FnOpRepr>>,
        l_store: JsVar,
        r_store: JsVar,
        done: JsVar,
    },
    TypeOf {
        of: Box<GcDestr<FnOpRepr>>,
        result: JsVar,
        done: JsVar,
    },
    Await {
        what: Box<GcDestr<FnOpRepr>>,
    },
    AssignRef {
        to: Box<GcDestr<FnOpRepr>>,
        key: Box<GcDestr<FnOpRepr>>,
        what: Box<GcDestr<FnOpRepr>>,
    },
    Plus {
        left: Box<GcDestr<FnOpRepr>>,
        right: Box<GcDestr<FnOpRepr>>,
    },
    NumeralPlus {
        left: Box<GcDestr<FnOpRepr>>,
        right: Box<GcDestr<FnOpRepr>>,
    },
}

impl FnOpRepr {
    fn call_stack(stack: &mut StackAccess, this: &Box<Vec<FnOpRepr>>) -> Vec<StackElement> {
        //let ret = Vec::new();
        unimplemented!()
    }
}

#[derive(Clone, Trace, Finalize)]
pub enum FnOp {
    LoadGlobal {
        name: Rc<String>,
        target: JsVar,
    },
    Assign {
        source: JsVar,
        target: JsVar,
    },
    LoadStatic {
        value: JsValue,
        target: JsVar,
    },
    CallFunction {
        func: Box<Vec<FnOpRepr>>,
    },
    Label {
        id: Identity,
    },
    Throw {
        what: JsVar,
    },
    Return {
        target: JsVar,
    },
    Deref {
        from: JsVar,
        key: JsVar,
        target: JsVar,
        optional: bool,
    },
    Nop {},
    BoolAnd {
        left: JsVar,
        right: JsVar,
        target: JsVar,
    },
    BoolNot {
        left: JsVar,
        right: JsVar,
        target: JsVar,
    },
    BoolOr {
        left: JsVar,
        right: JsVar,
        target: JsVar,
    },
    FuzzyCompare {
        left: JsVar,
        right: JsVar,
        target: JsVar,
    },
    StrictCompare {
        left: JsVar,
        right: JsVar,
        target: JsVar,
    },
    TypeOf {
        value: JsVar,
        target: JsVar,
    },
    Await {
        what: JsVar,
        target: JsVar,
    },
    AssignRef {
        left: JsVar,
        right: JsVar,
        target: JsVar,
    },
    Plus {
        left: JsVar,
        right: JsVar,
        target: JsVar,
    },
    NumeralPlus {
        left: JsVar,
        right: JsVar,
        target: JsVar,
    },
}

impl FnOp {
    fn throw_internal(reason: &str) -> GcDestr<FnOpRepr> {
        GcDestr::new(FnOpRepr::Throw {
            what: Box::new(GcDestr::new(FnOpRepr::LoadStatic {
                value: JsValue::String(Rc::new(format!("internal error: {}", reason))),
            })),
        })
    }

    fn run<'a, F1: Fn(usize) -> &'a StackElement, F2: Fn(usize, StackElement)>(
        this: FnOp,
        mut stack: StackAccess,
    ) -> FnOpAction {
        return match this {
            FnOp::LoadGlobal { name, target } => FnOpAction::LoadGlobal { name, target },
            FnOp::Assign { source, target } => {
                target.set(Some(&mut stack), source.get(Some(&mut stack)));
                FnOpAction::Pop(1)
            }
            FnOp::LoadStatic { value, target } => {
                target.set(Some(&mut stack), value);
                FnOpAction::Nop
            }
            FnOp::CallFunction { func } => {
                FnOpAction::Push(FnOpRepr::call_stack(&mut stack, &func))
            }
            FnOp::Throw { what } => FnOpAction::Two(
                StackElement::Value(what.get(Some(&mut stack))),
                StackElement::Op(FnOp::Label {
                    id: stack.try_label(),
                }),
            ),
            FnOp::Return { target } => FnOpAction::Two(
                StackElement::Value(target.get(Some(&mut stack))),
                StackElement::Op(FnOp::Label {
                    id: stack.method_label(),
                }),
            ),
            FnOp::Deref {
                from,
                key,
                target,
                optional,
            } => {
                match from.get(Some(&mut stack)) {
                    JsValue::Undefined => {
                        if optional {
                            target.set(Some(&mut stack), JsValue::Undefined);
                            FnOpAction::Nop
                        } else {
                            let v = stack.make_local();
                            FnOpAction::Two(
                                StackElement::Op(FnOp::LoadStatic {
                                    value: u_string("cannot read key of undefined"),
                                    target: v.clone(),
                                }),
                                StackElement::Op(FnOp::Throw { what: v }),
                            )
                        }
                    }
                    JsValue::Null => {
                        if optional {
                            target.set(Some(&mut stack), JsValue::Undefined);
                            FnOpAction::Nop
                        } else {
                            let v = stack.make_local();
                            FnOpAction::Two(
                                StackElement::Op(FnOp::LoadStatic {
                                    value: u_string("cannot read key of null"),
                                    target: v.clone(),
                                }),
                                StackElement::Op(FnOp::Throw { what: v }),
                            )
                        }
                    }
                    JsValue::Number(_) => {
                        // TODO proto loops
                        let proto_name = stack.make_local();
                        proto_name.set(Some(&mut stack), JsValue::String(s_pool("__proto__")));
                        let number = stack.make_local();
                        let proto_loc = stack.make_local();
                        FnOpAction::Three(
                            StackElement::Op(FnOp::LoadGlobal {
                                name: s_pool("Number"),
                                target: number.clone(),
                            }),
                            StackElement::Op(FnOp::Deref {
                                from: number,
                                key: proto_name,
                                target: proto_loc.clone(),
                                optional: true,
                            }),
                            StackElement::Op(FnOp::Deref {
                                from: proto_loc,
                                key,
                                target,
                                optional: true,
                            }),
                        )
                    }
                    JsValue::Boolean(_) => {
                        let proto_name = stack.make_local();
                        proto_name.set(Some(&mut stack), JsValue::String(s_pool("__proto__")));
                        let number = stack.make_local();
                        let proto_loc = stack.make_local();
                        FnOpAction::Three(
                            StackElement::Op(FnOp::LoadGlobal {
                                name: s_pool("Boolean"),
                                target: number.clone(),
                            }),
                            StackElement::Op(FnOp::Deref {
                                from: number,
                                key: proto_name,
                                target: proto_loc.clone(),
                                optional: true,
                            }),
                            StackElement::Op(FnOp::Deref {
                                from: proto_loc,
                                key,
                                target,
                                optional: true,
                            }),
                        )
                    }
                    JsValue::String(s) => {
                        if let Ok(index) = key.get(Some(&mut stack)).to_system_string().parse() {
                            if let Some(at) = s.get(index..(index + 1)) {
                                // TODO safe? correct?
                                target.set()
                            }
                        }
                        let proto_name = stack.make_local();
                        proto_name.set(Some(&mut stack), JsValue::String(s_pool("__proto__")));
                        let number = stack.make_local();
                        let proto_loc = stack.make_local();
                        FnOpAction::Three(
                            StackElement::Op(FnOp::LoadGlobal {
                                name: s_pool("Number"),
                                target: number.clone(),
                            }),
                            StackElement::Op(FnOp::Deref {
                                from: number,
                                key: proto_name,
                                target: proto_loc.clone(),
                                optional: true,
                            }),
                            StackElement::Op(FnOp::Deref {
                                from: proto_loc,
                                key,
                                target,
                                optional: true,
                            }),
                        )
                    }
                    JsValue::Object(_) => {}
                }
            }
            FnOp::IfElse { .. } => {}
            FnOp::While { .. } => {}
            FnOp::For { .. } => {}
            FnOp::Nop { .. } => {}
            FnOp::BoolAnd { .. } => {}
            FnOp::BoolNot { .. } => {}
            FnOp::BoolOr { .. } => {}
            FnOp::FuzzyCompare { .. } => {}
            FnOp::StrictCompare { .. } => {}
            FnOp::TypeOf { .. } => {}
            FnOp::Await { .. } => {}
            FnOp::AssignRef { .. } => {}
            FnOp::Plus { .. } => {}
            FnOp::NumeralPlus { .. } => {}
        };
    }
}

pub enum FnOpAction {
    Nop,
    Pop(usize),              // pop n op-frames
    Push(Vec<StackElement>), // push additional ops, Pop us
    One(StackElement),       // replace just us
    Two(StackElement, StackElement),
    Three(StackElement, StackElement, StackElement),
    Four(StackElement, StackElement, StackElement, StackElement),
    CopyStack(usize), // re-append the last n elements
    LoadGlobal {
        // Needed for finding prototypes etc
        name: Rc<String>, // name of global
        target: JsVar,
    },
    PushLabel {
        id: Identity,
    },
    LabelWalkback {
        id: Identity,
    },
}

#[derive(Trace, Finalize)]
pub enum StackElement {
    Value(JsValue),
    StackPointer(usize),
    Label(Identity), // Label like for while. There are special labels for method call and catch
    Op(FnOp),
}

impl StackElement {
    pub fn assume_val(&self) -> JsValue {
        match self {
            StackElement::Value(val) => val.clone(),
            _ => {
                panic!("stack corruption (5)")
            }
        }
    }
}

#[derive(Clone)]
pub enum JsVar {
    Stack {
        pos: usize,
        name: Rc<String>,
    },
    Heap {
        name: Rc<String>,
        value: Gc<GcCell<JsValue>>,
        defined: bool, // Global variables may be referenced, but not instantiated
    },
}

impl JsVar {
    pub fn new_t() -> JsVar {
        return JsVar::new_n("#temp#");
    }

    pub fn new_n(name: &'static str) -> JsVar {
        return JsVar::new(s_pool(name));
    }

    pub fn new(name: Rc<String>) -> JsVar {
        return JsVar::Stack { name, pos: 0 };
    }

    pub fn ensure_heap(&mut self) {
        match self {
            JsVar::Stack { pos, name } => std::mem::swap(
                self,
                &mut JsVar::Heap {
                    name: name.clone(),
                    value: Gc::new(GcCell::new(JsValue::Undefined)),
                    defined: true,
                },
            ),
            JsVar::Heap { .. } => {}
        }
    }

    pub fn set(&self, stack: Option<&StackAccess>, val: JsValue) {
        match self {
            JsVar::Stack { pos, .. } => {
                stack
                    .expect("Stack variable get without stack!")
                    .read_stack(*pos);
            }
            JsVar::Heap { value, .. } => {
                *value.borrow_mut() = val;
            }
        }
    }

    // TODO inline
    pub fn get(&self, stack: Option<&StackAccess>) -> JsValue {
        match self {
            JsVar::Stack { pos, .. } => stack
                .expect("Stack variable set without stack!")
                .read_stack(*pos)
                .assume_val(),
            JsVar::Heap { value, .. } => return value.borrow_mut().clone(),
        }
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
