use crate::js::data::js_execution::FnOp::Throw;
use crate::js::data::js_types::{Identity, JSCallable, JsFn, JsProperty, JsValue};
use crate::js::data::util::{
    s_pool, u_array_e, u_block, u_bool, u_call, u_capture_deref, u_deref, u_function, u_if,
    u_if_else, u_literal, u_load_global, u_not, u_number, u_read_var, u_strict_comp, u_string,
    u_typeof, u_undefined, u_write_var, JsObjectBuilder,
};
use gc::{Finalize, Trace};
use gc::{Gc, GcCell};
use std::cell::RefCell;
use std::f64::NAN;
use std::ops::Deref;
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
            .expect("Mutex poisoned. This is possibly an internal error.");
        // TODO
        unimplemented!()
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
    latest_fn_pos: usize, // Position of "this"
}

/**
Stack layout:
-3) return value
-2) do not use: other functions continuation op
-1) return-label
 0) this
 1) args-array
 2..n) local variables
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
        self.stack
            .stack
            .push(StackElement::Value(Default::default()));
        self.stack.stack.push(StackElement::Op(FnOp::Pop { n: 1 }));
        JsVar::Stack {
            pos: self.stack.stack.len() - 2,
            name: Rc::new("".to_string()),
        }
    }

    pub fn make_manual_local(&mut self) -> JsVar {
        self.stack
            .stack
            .push(StackElement::Value(Default::default()));
        JsVar::Stack {
            pos: self.stack.stack.len() - 1,
            name: Rc::new("".to_string()),
        }
    }

    pub fn local_at(&mut self, pos: usize) -> JsVar {
        JsVar::Stack {
            pos: self.latest_fn_pos + pos,
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
                let action = FnOp::run(stack_op, &mut access);
                match action {
                    FnOpAction::Pop(pop) => self.stack.truncate(pop),
                    FnOpAction::Push(mut push) => self.stack.append(&mut push),
                    FnOpAction::One(one) => self.stack.push(one),
                    FnOpAction::LoadGlobal { name, target } => {
                        if name.as_str() == "console" {
                            target.set(Some(&mut access), u_undefined());
                            // TODO
                        }
                    }
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
                                StackElement::HeapVar(_) => {}
                            }
                        }
                        let temp = self.stack.pop().expect("corrupt stack (5)");
                        self.stack.push(value);
                        self.stack.push(temp);
                    }
                    FnOpAction::Nop => {}
                    FnOpAction::Two(e1, e2) => {
                        self.stack.push(e2);
                        self.stack.push(e1);
                    }
                    FnOpAction::Three(e1, e2, e3) => {
                        self.stack.push(e3);
                        self.stack.push(e2);
                        self.stack.push(e1);
                    }
                    FnOpAction::Four(e1, e2, e3, e4) => {
                        self.stack.push(e4);
                        self.stack.push(e3);
                        self.stack.push(e2);
                        self.stack.push(e1);
                    }
                    FnOpAction::CopyStack(n) => {
                        self.stack.extend_from_slice(
                            &self.stack[(self.stack.len() - n)..self.stack.len()],
                        );
                    }
                    FnOpAction::Await { target, what } => {
                        return (AsyncStackResult::Forget, consumed)
                    }
                };
                consumed += 1;
            } else {
                return (AsyncStackResult::Forget, consumed);
            }
        }
    }
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
pub enum VarAlloc {
    CapturedAt(usize),
    LocalAt(usize),
}

#[derive(Clone, Trace, Finalize)]
pub enum FnOpRepr {
    LoadGlobal {
        name: Rc<String>,
    },
    Assign {
        target: VarAlloc,
        what: Rc<FnOpRepr>,
    },
    LoadStatic {
        value: JsValue,
    },
    ReadVar {
        which: VarAlloc,
    },
    CallFunction {
        this: VarAlloc,
        on: Rc<FnOpRepr>,
        arg_array: Rc<FnOpRepr>,
    },
    Throw {
        what: Rc<FnOpRepr>,
    },
    Return {
        what: Rc<FnOpRepr>,
    },
    Deref {
        from: Rc<FnOpRepr>,
        key: Rc<FnOpRepr>,
    },
    IfElse {
        condition: Rc<FnOpRepr>,
        if_block: Rc<FnOpRepr>,
        else_block: Rc<FnOpRepr>,
    },
    While {
        condition: Rc<FnOpRepr>,
        block: Rc<FnOpRepr>,
    },
    For {
        initial: Rc<FnOpRepr>,
        condition: Rc<FnOpRepr>,
        each: Rc<FnOpRepr>,
        block: Rc<FnOpRepr>,
    },
    Nop {},
    Multi {
        block: Vec<FnOpRepr>,
    },
    BoolAnd {
        left: Rc<FnOpRepr>,
        right: Rc<FnOpRepr>,
    },
    BoolNot {
        of: Rc<FnOpRepr>,
    },
    BoolOr {
        left: Rc<FnOpRepr>,
        right: Rc<FnOpRepr>,
    },
    FuzzyCompare {
        left: Rc<FnOpRepr>,
        right: Rc<FnOpRepr>,
    },
    StrictCompare {
        left: Rc<FnOpRepr>,
        right: Rc<FnOpRepr>,
    },
    TypeOf {
        of: Rc<FnOpRepr>,
    },
    Await {
        what: Rc<FnOpRepr>,
    },
    AssignRef {
        to: Rc<FnOpRepr>,
        key: Rc<FnOpRepr>,
        what: Rc<FnOpRepr>,
    },
    Plus {
        left: Rc<FnOpRepr>,
        right: Rc<FnOpRepr>,
    },
    NumeralPlus {
        left: Rc<FnOpRepr>,
        right: Rc<FnOpRepr>,
    },
    NewObject {
        is_array: bool,
    },
    InstantiateFunction {
        vars: Rc<Vec<VarAlloc>>,
        code: Rc<FnOpRepr>,
    },
}

/*

-3) return value
-2) do not use: other functions continuation op
-1) return-label
 0) this
 1) args-array
 3..a) captures
 a..b) local variables
 b..c) code, local variables
*/
impl FnOpRepr {
    fn call_stack(
        stack: &mut StackAccess,
        op: &FnOpRepr,
        store_at: JsVar,
        into: &mut Vec<StackElement>,
    ) {
        let to_var = |var_alloc: &VarAlloc| match var_alloc {
            VarAlloc::CapturedAt(pos) => stack.read_stack(*pos).assume_capture(),
            VarAlloc::LocalAt(pos) => stack.local_at(*pos),
        };
        match op {
            FnOpRepr::LoadGlobal { name } => {
                into.push(StackElement::Op(FnOp::LoadGlobal {
                    name: name.clone(),
                    target: store_at.clone(),
                }));
            }
            FnOpRepr::Assign { what, target } => {
                FnOpRepr::call_stack(stack, what.deref(), to_var(target), into);
            }
            FnOpRepr::LoadStatic { value } => into.push(StackElement::Op(FnOp::LoadStatic {
                value: value.clone(),
                target: store_at.clone(),
            })),
            FnOpRepr::ReadVar { which } => into.push(StackElement::Op(FnOp::Assign {
                source: to_var(which),
                target: store_at.clone(),
            })),
            FnOpRepr::CallFunction {
                this,
                on,
                arg_array,
            } => {
                let on_v = stack.make_local();
                let arg_v = stack.make_local();
                let ret = stack.make_local();

                into.push(StackElement::Op(FnOp::CallFunction {
                    this: to_var(this),
                    func: on_v,
                    args: arg_v,
                    target: ret.clone(),
                }));
                FnOpRepr::call_stack(stack, arg_array, arg_v.clone(), into);
                FnOpRepr::call_stack(stack, on, on_v.clone(), into);
            }
            FnOpRepr::Throw { what } => {
                let t_v = stack.make_local();
                into.push(StackElement::Op(FnOp::Throw { what: t_v.clone() }));
                FnOpRepr::call_stack(stack, what, t_v, into);
            }
            FnOpRepr::Return { what } => {
                let v = stack.make_local();
                into.push(StackElement::Op(FnOp::Return { target: v.clone() }));
                FnOpRepr::call_stack(stack, what, v, into);
            }
            FnOpRepr::Deref { from, key } => {
                let f_v = stack.make_local();
                let k_v = stack.make_local();

                into.push(StackElement::Op(FnOp::Deref {
                    from: f_v.clone(),
                    key: k_v.clone(),
                    target: store_at.clone(),
                    optional: false,
                }))
            }
            FnOpRepr::IfElse {
                condition,
                if_block,
                else_block,
            } => {
                let end = Identity::new();
                let else_label = Identity::new();

                into.push(StackElement::Op(FnOp::MakeLocal {})); // 1) make sure our jump target is ok
                into.push(StackElement::Label(end.clone())); // 2) set ending label

                into.push(StackElement::Op(FnOp::ToLabel { id: end.clone() }));

                FnOpRepr::call_stack(stack, else_block, stack.make_local(), into); // 3) load else block

                into.push(StackElement::Value(JsValue::Undefined));
                into.push(StackElement::Op(FnOp::Pop { n: 1 }));
                into.push(StackElement::Label(else_label.clone()));
                into.push(StackElement::Op(FnOp::ToLabel { id: end.clone() }));

                FnOpRepr::call_stack(stack, if_block, stack.make_local(), into); // 4) load if block

                let is_true = stack.make_local();

                into.push(StackElement::Op(FnOp::ToLabel { id: else_label }));

                into.push(StackElement::Op(FnOp::SkipIf {
                    condition: is_true.clone(),
                }));

                FnOpRepr::call_stack(stack, condition, is_true, into);
            }
            FnOpRepr::While { condition, block } => {
                let end_label = Identity::new();
                let condition_result = stack.make_local();
                into.push(StackElement::Op(FnOp::Pop { n: 1 }));
                into.push(StackElement::Label(end_label.clone()));
                into.push(StackElement::Op(FnOp::ToLabel {
                    id: end_label.clone(),
                }));

                let before = into.len();

                FnOpRepr::call_stack(stack, block, stack.make_local(), into);
                into.push(StackElement::Op(FnOp::ToLabel {
                    id: end_label.clone(),
                }));
                into.push(StackElement::Op(FnOp::SkipIf {
                    condition: condition_result.clone(),
                }));
                FnOpRepr::call_stack(stack, condition, condition_result, into);

                into.push(StackElement::Op(FnOp::Repeat {
                    n: into.len() - before,
                }));
            }
            FnOpRepr::For {
                initial,
                condition,
                each,
                block,
            } => {
                let end_label = Identity::new();
                let condition_result = stack.make_local();
                into.push(StackElement::Op(FnOp::Pop { n: 1 }));
                into.push(StackElement::Label(end_label.clone()));
                into.push(StackElement::Op(FnOp::ToLabel {
                    id: end_label.clone(),
                }));

                let before = into.len();

                FnOpRepr::call_stack(stack, block, stack.make_local(), into);
                FnOpRepr::call_stack(stack, each, stack.make_local(), into);
                into.push(StackElement::Op(FnOp::ToLabel {
                    id: end_label.clone(),
                }));
                into.push(StackElement::Op(FnOp::SkipIf {
                    condition: condition_result.clone(),
                }));
                FnOpRepr::call_stack(stack, condition, condition_result, into);

                into.push(StackElement::Op(FnOp::Repeat {
                    n: into.len() - before,
                }));

                FnOpRepr::call_stack(stack, initial, stack.make_local(), into);
            }
            FnOpRepr::Nop {} => {}
            FnOpRepr::Multi { block } => {
                for b in block.iter().rev() {
                    FnOpRepr::call_stack(stack, b, store_at.clone(), into);
                }
            }
            FnOpRepr::BoolAnd { left, right } => {
                let end_label = Identity::new();

                into.push(StackElement::Op(FnOp::Pop { n: 1 }));
                into.push(StackElement::Label(end_label.clone()));
                into.push(StackElement::Op(FnOp::ToLabel {
                    id: end_label.clone(),
                }));

                FnOpRepr::call_stack(stack, right, store_at.clone(), into);

                into.push(StackElement::Op(FnOp::ToLabel { id: end_label }));
                into.push(StackElement::Op(FnOp::SkipIf {
                    condition: store_at.clone(),
                }));

                FnOpRepr::call_stack(stack, left, store_at.clone(), into);
            }
            FnOpRepr::BoolNot { of } => {
                into.push(StackElement::Op(FnOp::Not {
                    source: store_at.clone(),
                    target: store_at.clone(),
                }));

                FnOpRepr::call_stack(stack, of, store_at, into);
            }
            FnOpRepr::BoolOr { right, left } => {
                let end_label = Identity::new();
                let temp_not = stack.make_local();

                into.push(StackElement::Op(FnOp::Pop { n: 1 }));
                into.push(StackElement::Label(end_label.clone()));
                into.push(StackElement::Op(FnOp::ToLabel {
                    id: end_label.clone(),
                }));

                FnOpRepr::call_stack(stack, right, store_at.clone(), into);

                into.push(StackElement::Op(FnOp::ToLabel { id: end_label }));
                into.push(StackElement::Op(FnOp::SkipIf {
                    condition: temp_not,
                }));
                into.push(StackElement::Op(FnOp::Not {
                    source: store_at.clone(),
                    target: temp_not.clone(),
                }));
                FnOpRepr::call_stack(stack, left, store_at.clone(), into);
            }
            FnOpRepr::FuzzyCompare { right, left } => {
                let l_store = stack.make_local();
                let r_store = stack.make_local();

                into.push(StackElement::Op(FnOp::FuzzyCompare {
                    left: l_store.clone(),
                    right: r_store.clone(),
                    target: store_at,
                }));

                FnOpRepr::call_stack(stack, left, l_store, into);
                FnOpRepr::call_stack(stack, right, r_store, into);
            }
            FnOpRepr::StrictCompare { left, right } => {
                let l_store = stack.make_local();
                let r_store = stack.make_local();

                into.push(StackElement::Op(FnOp::StrictCompare {
                    left: l_store.clone(),
                    right: r_store.clone(),
                    target: store_at,
                }));

                FnOpRepr::call_stack(stack, left, l_store, into);
                FnOpRepr::call_stack(stack, right, r_store, into);
            }
            FnOpRepr::TypeOf { of } => {
                let temp = stack.make_local();

                into.push(StackElement::Op(FnOp::TypeOf {
                    value: temp.clone(),
                    target: store_at,
                }));
                FnOpRepr::call_stack(stack, of, temp.clone(), into);
            }
            FnOpRepr::Await { what } => {
                let temp = stack.make_local();

                into.push(StackElement::Op(FnOp::Await {
                    what: temp.clone(),
                    target: store_at,
                }));
                FnOpRepr::call_stack(stack, what, temp, into);
            }
            FnOpRepr::AssignRef { what, key, to } => {
                let k_v = stack.make_local();
                let to_v = stack.make_local();
                let what_v = stack.make_local();

                into.push(StackElement::Op(FnOp::AssignRef {
                    of: to_v.clone(),
                    key: k_v.clone(),
                    what: what_v.clone(),
                }));

                FnOpRepr::call_stack(stack, what, what_v, into);
                FnOpRepr::call_stack(stack, key, k_v, into);
                FnOpRepr::call_stack(stack, to, to_v, into);
            }
            FnOpRepr::Plus { left, right } => {
                let l_v = stack.make_local();
                let r_v = stack.make_local();

                into.push(StackElement::Op(FnOp::Plus {
                    left: l_v.clone(),
                    right: r_v.clone(),
                    target: store_at,
                }));

                FnOpRepr::call_stack(stack, right, r_v, into);
                FnOpRepr::call_stack(stack, left, l_v, into);
            }
            FnOpRepr::NumeralPlus { left, right } => {
                let l_v = stack.make_local();
                let r_v = stack.make_local();

                into.push(StackElement::Op(FnOp::NumeralPlus {
                    left: l_v.clone(),
                    right: r_v.clone(),
                    target: store_at,
                }));

                FnOpRepr::call_stack(stack, right, r_v, into);
                FnOpRepr::call_stack(stack, left, l_v, into);
            }
            FnOpRepr::NewObject { is_array } => into.push(StackElement::Op(FnOp::NewObject {
                target: store_at,
                is_array: *is_array,
            })),
            FnOpRepr::InstantiateFunction { vars, code } => {
                into.push(StackElement::Op(FnOp::CreateFunction {
                    captures: Rc::new(vars.iter().map(|va| to_var(va)).collect()), // TODO straight to stack?
                    code: code.clone(),
                    target: store_at,
                }))
            }
        }
    }
}

#[derive(Clone, Trace, Finalize)]
pub enum FnOp {
    NewObject {
        target: JsVar,
        is_array: bool,
    },
    Not {
        source: JsVar,
        target: JsVar,
    },
    LoadGlobal {
        name: Rc<String>,
        target: JsVar,
    },
    SkipIf {
        condition: JsVar,
    },
    Repeat {
        n: usize,
    },
    MakeLocal {},
    Assign {
        source: JsVar,
        target: JsVar,
    },
    LoadStatic {
        value: JsValue,
        target: JsVar,
    },
    CreateFunction {
        captures: Rc<Vec<JsVar>>,
        code: Rc<FnOpRepr>,
        target: JsVar,
    },
    CallFunction {
        this: JsVar,
        func: JsVar,
        args: JsVar,
        target: JsVar,
    },
    Pop {
        n: usize, // used for popping local temp vars
    },
    LabelSet {
        id: Identity,
    },
    ToLabel {
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
        of: JsVar,
        key: JsVar,
        what: JsVar,
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
    fn run(this: FnOp, stack: &mut StackAccess) -> FnOpAction {
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
        return match this {
            FnOp::LoadGlobal { name, target } => FnOpAction::LoadGlobal { name, target },
            FnOp::Assign { source, target } => {
                target.set(Some(stack), source.get(Some(stack)));
                FnOpAction::Pop(1)
            }
            FnOp::LoadStatic { value, target } => {
                target.set(Some(stack), value);
                FnOpAction::Nop
            }
            FnOp::CallFunction {
                this,
                func,
                args,
                target,
            } => {
                unimplemented!()
            }
            FnOp::Throw { what } => FnOpAction::Two(
                StackElement::Value(what.get(Some(stack))),
                StackElement::Op(FnOp::ToLabel {
                    id: stack.try_label(),
                }),
            ),
            FnOp::Return { target } => FnOpAction::Two(
                StackElement::Value(target.get(Some(stack))),
                StackElement::Op(FnOp::ToLabel {
                    id: stack.method_label(),
                }),
            ),
            FnOp::Deref {
                from,
                key,
                target,
                optional,
            } => {
                match from.get(Some(stack)) {
                    JsValue::Undefined => {
                        if optional {
                            target.set(Some(stack), JsValue::Undefined);
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
                            target.set(Some(stack), JsValue::Undefined);
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
                        proto_name.set(Some(stack), JsValue::String(s_pool("__proto__")));
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
                        proto_name.set(Some(stack), JsValue::String(s_pool("__proto__")));
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
                        if let Ok(index) = key.get(Some(stack)).to_system_string().parse() {
                            if let Some(at) = s.get(index..(index + 1)) {
                                // TODO safe? correct?
                                target.set(Some(stack), JsValue::String(Rc::new(at.into())))
                            }
                        }
                        let proto_name = stack.make_local();
                        proto_name.set(Some(stack), JsValue::String(s_pool("__proto__")));
                        let number = stack.make_local();
                        let proto_loc = stack.make_local();
                        FnOpAction::Three(
                            StackElement::Op(FnOp::LoadGlobal {
                                name: s_pool("String"),
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
                    JsValue::Object(obj) => {
                        let mut key_v = key.get(Some(stack));
                        if key_v.is_symbol() {
                            key_v = JsValue::String(key_v.to_system_string());
                        }

                        if let Some(v) = GcCell::borrow(&obj).content.get(&key_v.to_system_string())
                        {
                            target.set(Some(stack), v.value.clone());
                            FnOpAction::Nop
                        } else {
                            let proto_name = stack.make_local();
                            proto_name.set(Some(stack), JsValue::String(s_pool("__proto__")));
                            let proto_loc = stack.make_local();
                            FnOpAction::Two(
                                StackElement::Op(FnOp::Deref {
                                    from,
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
                    }
                }
            }
            FnOp::Nop {} => FnOpAction::Nop,
            FnOp::FuzzyCompare {
                left,
                right,
                target,
            } => {
                let ret = match &(left.get(Some(stack)), right.get(Some(stack))) {
                    (JsValue::Undefined | JsValue::Null, JsValue::Undefined | JsValue::Null) => {
                        true
                    }
                    (JsValue::Undefined | JsValue::Null, _) => false,
                    (_, JsValue::Undefined | JsValue::Null) => false,
                    (JsValue::Number(n1), JsValue::Number(n2)) => n1 == n2,
                    (JsValue::String(s), JsValue::Number(n)) => *n == s.parse().unwrap_or(NAN),
                    (JsValue::Number(n), JsValue::String(s)) => *n == s.parse().unwrap_or(NAN),
                    (JsValue::Number(n), JsValue::Boolean(b)) => *n == (if *b { 1.0 } else { 0.0 }),
                    (JsValue::Boolean(b), JsValue::Number(n)) => *n == (if *b { 1.0 } else { 0.0 }),
                    (JsValue::Boolean(b1), JsValue::Boolean(b2)) => b1 == b2,
                    (JsValue::String(s1), JsValue::String(s2)) => s1 == s2,
                    (JsValue::String(s), JsValue::Boolean(b)) => {
                        s.as_str() == (if *b { "1" } else { "0" })
                    }
                    (JsValue::Object(o1), JsValue::Object(o2)) => {
                        GcCell::borrow(&o1).identity == GcCell::borrow(&o2).identity
                    }
                    (_, _) => false, // TODO toprimitive
                };
                target.set(Some(stack), u_bool(ret));
                FnOpAction::Nop
            }
            FnOp::StrictCompare {
                left,
                right,
                target,
            } => {
                let ret = match &(left.get(Some(stack)), right.get(Some(stack))) {
                    (JsValue::Undefined, JsValue::Undefined) => true,
                    (JsValue::Null, JsValue::Null) => true,
                    (JsValue::Number(n1), JsValue::Number(n2)) => n1 == n2,
                    (JsValue::Boolean(b1), JsValue::Boolean(b2)) => b1 == b2,
                    (JsValue::String(s1), JsValue::String(s2)) => s1.as_str() == s2.as_str(),
                    (JsValue::Object(o1), JsValue::Object(o2)) => {
                        &GcCell::borrow(&o1).identity == &GcCell::borrow(&o2).identity
                    }
                    (_, _) => false,
                };
                target.set(Some(stack), u_bool(ret));
                FnOpAction::Nop
            }
            FnOp::TypeOf { target, value } => {
                let ret = match value.get(Some(stack)) {
                    JsValue::Undefined => s_pool("undefined"),
                    JsValue::Null => s_pool("object"),
                    JsValue::Boolean(_) => s_pool("boolean"),
                    JsValue::String(_) => s_pool("string"),
                    JsValue::Object(_) => s_pool("object"),
                    JsValue::Number(_) => s_pool("number"),
                };
                target.set(Some(stack), JsValue::String(ret));
                FnOpAction::Nop
            }
            FnOp::Await { target, what } => FnOpAction::Await { target, what },
            FnOp::AssignRef { of, key, what } => {
                let key = key.get(Some(stack));
                let of = of.get(Some(stack));
                match of {
                    JsValue::Undefined => {
                        let err = stack.make_local();
                        err.set(
                            Some(stack),
                            JsValue::String(s_pool("cannot assign value to undefined")),
                        );
                        return FnOpAction::One(StackElement::Op(Throw { what: err }));
                    }
                    JsValue::Null => {
                        let err = stack.make_local();
                        err.set(
                            Some(stack),
                            JsValue::String(s_pool("cannot assign value to undefined")),
                        );
                        return FnOpAction::One(StackElement::Op(Throw { what: err }));
                    }
                    JsValue::Object(obj) => {
                        if key.is_symbol() {
                            obj.borrow_mut().symbol_keys.insert(
                                key,
                                JsProperty {
                                    enumerable: false,
                                    configurable: false,
                                    writable: false,
                                    value: what.get(Some(stack)),
                                },
                            );
                        } else {
                            obj.borrow_mut().content.insert(
                                key.to_system_string(),
                                JsProperty {
                                    enumerable: true,
                                    configurable: true,
                                    writable: true,
                                    value: what.get(Some(stack)),
                                },
                            )
                        }
                    }
                    JsValue::Number(_) => {}
                    JsValue::Boolean(_) => {}
                    JsValue::String(_) => {}
                }

                FnOpAction::Nop
            }
            FnOp::Plus {
                left,
                right,
                target,
            } => {
                match (left.get(Some(&stack)), right.get(Some(&stack))) {
                    (JsValue::String(s1), val) => target.set(
                        Some(stack),
                        JsValue::String(Rc::new(s1.into() + val.to_system_string())),
                    ),
                    (val, JsValue::String(s1)) => target.set(
                        Some(stack),
                        JsValue::String(Rc::new(val.to_system_string().into() + s1)),
                    ),
                    (o @ JsValue::Object(_), val) => target.set(
                        Some(stack),
                        JsValue::String(Rc::new(
                            o.to_system_string().into() + val.to_system_string(),
                        )),
                    ),
                    (val, o @ JsValue::Object(_)) => target.set(
                        Some(stack),
                        JsValue::String(Rc::new(
                            val.to_system_string().into() + o.to_system_string(),
                        )),
                    ),
                    (v1, v2) => target.set(Some(stack), u_number(coerce(v1) + coerce(v2))),
                }
                FnOpAction::Nop
            }
            FnOp::NumeralPlus {
                left,
                right,
                target,
            } => {
                target.set(
                    Some(stack),
                    u_number(coerce(left.get(Some(stack))) + coerce(right.get(Some(stack)))),
                );
                FnOpAction::Nop
            }
            FnOp::LabelSet { id } => FnOpAction::One(StackElement::Label(id)),
            FnOp::ToLabel { id } => FnOpAction::LabelWalkback { id },
            FnOp::NewObject { is_array, target } => {
                let mut builder = JsObjectBuilder::new(None);
                if is_array {
                    builder = builder.with_being_array();
                }
                target.set(Some(stack), builder.build());
                FnOpAction::Nop
            }
            FnOp::Not { target, source } => {
                target.set(Some(stack), u_bool(source.get(Some(stack)).truthy()));
                FnOpAction::Nop
            }
            FnOp::SkipIf { condition } => {
                if condition.get(Some(stack)).truthy() {
                    FnOpAction::Pop(1)
                } else {
                    FnOpAction::Nop
                }
            }
            FnOp::Repeat { n } => FnOpAction::CopyStack(n),
            FnOp::MakeLocal { .. } => {
                stack.make_local(); // TODO no reason
                FnOpAction::Nop
            }
            FnOp::CreateFunction {
                captures,
                code,
                target,
            } => {
                let f = JsObjectBuilder::new(None)
                    .with_callable(JSCallable::Js {
                        content: Rc::new("".to_string()),
                        creator: Gc::new(JsFn {
                            ops: code,
                            captures: captures.clone(),
                        }),
                    })
                    .build();
                target.set(Some(stack), f);
                FnOpAction::Nop
            }
            FnOp::Pop { n } => FnOpAction::Pop(n),
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
    LabelWalkback {
        id: Identity,
    },
    Await {
        target: JsVar,
        what: JsVar,
    },
}

#[derive(Trace, Finalize, Clone)]
pub enum StackElement {
    Value(JsValue),
    HeapVar(JsVar),
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

    pub fn assume_capture(&self) -> JsVar {
        match self {
            StackElement::HeapVar(var) => var.clone(),
            _ => {
                panic!("stack corruption (6)")
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
