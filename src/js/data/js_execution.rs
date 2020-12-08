use crate::js::data::js_execution::FnOp::Throw;
use crate::js::data::js_execution::VarAlloc::CapturedAt;
use crate::js::data::js_types::{Identity, JSCallable, JsFn, JsProperty, JsValue};
use crate::js::data::util::{s_pool, u_bool, u_number, u_string, u_undefined, JsObjectBuilder};
use safe_gc::Mark;
use safe_gc::{Gc, GcCell};
use std::cell::{Cell, RefCell};
use std::f64::NAN;
use std::fmt::{Debug, Formatter};
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
        guard.push(Box::new(|f| {
            // todo
            f(Gc::new(GcCell::new(AsyncStack {
                stack: vec![
                    StackElement::Value(u_undefined()),
                    StackElement::Value(val),
                    StackElement::Op(FnOp::Pop { n: 2 }),
                    StackElement::Op(FnOp::Nop {}),
                    StackElement::Op(FnOp::Nop {}),
                    StackElement::Op(FnOp::Nop {}),
                    StackElement::Op(FnOp::CallFunction {
                        this: JsVar::Stack {
                            pos: 0,
                            name: Rc::new("".to_string()),
                        },
                        func: JsVar::Stack {
                            pos: 1,
                            name: Rc::new("".to_string()),
                        },
                        args: JsVar::Stack {
                            pos: 0,
                            name: Rc::new("".to_string()),
                        },
                        target: JsVar::Stack {
                            pos: 0,
                            name: Rc::new("".to_string()),
                        },
                    }),
                ],
                method_label: Identity::new(),
                try_label: Identity::new(),
                latest_fn_pos: 0,
            })));
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
                if let Some(cur) = self.tick_queue.pop() {
                    let gc_ref = cur.borrow();
                    let mut ref_mut = GcCell::borrow_mut(&gc_ref);
                    let (result, cost) = ref_mut.run(ticks - consumed);
                    consumed += cost;
                    match result {
                        AsyncStackResult::Forget => {
                            std::mem::drop(ref_mut);
                            self.tick_queue.pop();
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

#[derive(Mark)]
pub struct AsyncStack {
    stack: Vec<StackElement>, // values first, ops second - otherwise we would run into a value while evaluating!
    method_label: Identity,
    try_label: Identity,
    latest_fn_pos: usize, // not up to date, updated when max ticks runs out and used when started
}

pub enum AsyncStackResult {
    Forget,
    Keep,
}

pub struct StackAccess<'a> {
    stack: &'a mut AsyncStack,
}

/**
Stack layout:
-3) return value
-2) do not use: other functions continuation op
-1) return-label
 0) this
 1) args-array
 2..n) local variables, captures
 n..m) code, local variables
*/
impl<'a> StackAccess<'a> {
    /// Read a stack element, the top being 0
    pub fn read_stack(&'a self, pos: usize) -> &'a StackElement {
        // self.latest_fn_pos
        self.stack
            .stack
            .get(pos + self.stack.latest_fn_pos)
            .expect("corrupt stack (3)")
    }
    pub fn read_stack_absolute(&'a self, pos: usize) -> &'a StackElement {
        // self.latest_fn_pos
        self.stack.stack.get(pos).expect("corrupt stack (3)")
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
            pos: self.stack.latest_fn_pos + pos,
            name: Rc::new("".to_string()),
        }
    }

    pub fn local_at_named(&mut self, pos: usize, name: Rc<String>) -> JsVar {
        JsVar::Stack {
            pos: self.stack.latest_fn_pos + pos,
            name,
        }
    }

    pub fn write_stack(&'a mut self, pos: usize) -> &'a mut StackElement {
        self.stack
            .stack
            .get_mut(pos + self.stack.latest_fn_pos)
            .expect("corrupt stack (4)")
    }

    pub fn write_stack_absolute(&mut self, pos: usize) -> &mut StackElement {
        self.stack.stack.get_mut(pos).expect("corrupt stack (4)")
    }

    pub fn method_label(&self) -> Identity {
        self.stack.method_label.clone()
    }

    pub fn try_label(&self) -> Identity {
        self.stack.try_label.clone()
    }
}

#[derive(Mark)]
struct LogFunction {}

impl NativeFunction for LogFunction {
    fn native_call(&self, _this: JsValue, args: JsValue) -> Result<JsValue, JsValue> {
        println!("{}", args.to_system_string());
        Ok(u_undefined())
    }
}

impl AsyncStack {
    fn run(&mut self, max: u64) -> (AsyncStackResult, u64) {
        let mut consumed = 0;

        let mut access = StackAccess { stack: self };

        println!("return: {:?}", access.method_label());
        println!("catch: {:?}", access.try_label());

        loop {
            if consumed >= max {
                self.latest_fn_pos = access.stack.latest_fn_pos; // TODO use reference for stack access
                return (AsyncStackResult::Keep, consumed);
            }

            if let Some(last) = access.stack.stack.pop() {
                //println!("\n{:?}\n", last);
                for (i, x) in access.stack.stack.iter().enumerate() {
                    //println!("{}: {:?}", i, x)
                }
                //println!("-----------\n");
                let stack_op = match last {
                    // TODO by value
                    StackElement::Op(op) => op,
                    _ => {
                        panic!("Corrupt stack (1)")
                    }
                };
                let action = FnOp::run(stack_op, &mut access);
                match action {
                    FnOpAction::Pop(pop) => {
                        access.stack.stack.truncate(access.stack.stack.len() - pop)
                    }
                    FnOpAction::Push(mut push) => access.stack.stack.append(&mut push),
                    FnOpAction::One(one) => access.stack.stack.push(one),
                    FnOpAction::LoadGlobal { name: _, target: _ } => {
                        unimplemented!() // TODO remove loadglobal
                    }
                    FnOpAction::LabelWalkback { id } => {
                        let value = access.stack.stack.pop().expect("corrupt stack (2)");
                        while let Some(el) = access.stack.stack.pop() {
                            match el {
                                StackElement::Value(_) => {}
                                StackElement::Label(label) => {
                                    if label == id {
                                        if label == access.stack.method_label {}
                                        break;
                                    }
                                }
                                StackElement::Op(_) => {}
                                StackElement::HeapVar(_) => {}
                            }
                        }
                        if access.stack.stack.is_empty() {
                            continue;
                        }
                        let temp = access.stack.stack.pop().expect("corrupt stack (5)");
                        access.stack.stack.push(value);
                        access.stack.stack.push(temp);
                    }
                    FnOpAction::Nop => {}
                    FnOpAction::Two(e1, e2) => {
                        access.stack.stack.push(e2);
                        access.stack.stack.push(e1);
                    }
                    FnOpAction::Three(e1, e2, e3) => {
                        access.stack.stack.push(e3);
                        access.stack.stack.push(e2);
                        access.stack.stack.push(e1);
                    }
                    FnOpAction::Four(e1, e2, e3, e4) => {
                        access.stack.stack.push(e4);
                        access.stack.stack.push(e3);
                        access.stack.stack.push(e2);
                        access.stack.stack.push(e1);
                    }
                    FnOpAction::CopyStack(n) => {
                        access
                            .stack
                            .stack
                            .push(StackElement::Op(FnOp::Repeat { n }));
                        access.stack.stack.append(
                            &mut Vec::from(
                                &access.stack.stack
                                    [(access.stack.stack.len() - n - 1)..access.stack.stack.len()],
                            ), // TODO
                        );
                    }
                    FnOpAction::Await { target: _, what: _ } => {
                        self.latest_fn_pos = access.stack.latest_fn_pos;
                        return (AsyncStackResult::Forget, consumed); // TODO await
                    }
                    FnOpAction::SetFnOffset { to } => {
                        access.stack.latest_fn_pos = to;
                    }
                };
                consumed += 1;
            } else {
                self.latest_fn_pos = access.stack.latest_fn_pos;
                return (AsyncStackResult::Forget, consumed);
            }
        }
    }
}

#[derive(Clone, Mark, Debug)]
pub enum VarAlloc {
    CapturedAt {
        name: Rc<String>,
        from: Box<VarAlloc>,
        target: usize,
    },
    LocalAt(Rc<String>, usize),
    Static(Rc<String>, JsVar),
}

impl VarAlloc {}

pub trait NativeFunction: Mark + Fn(JsValue, JsValue) -> Result<JsValue, JsValue> {}

impl Debug for dyn NativeFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[native function]")
    }
}

#[derive(Clone, Mark, Debug)]
pub enum FnOpRepr {
    LoadGlobal {
        name: Rc<String>,
    },
    NativeCall {
        target: VarAlloc,
        this: VarAlloc,
        args: VarAlloc,
        call: Rc<dyn NativeFunction>,
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
        // let before_len = into.len();
        let to_var = |stack: &mut StackAccess, var_alloc: &VarAlloc| match var_alloc {
            VarAlloc::CapturedAt {
                name: _,
                from: _,
                target,
            } => stack.read_stack(*target).assume_capture(),
            VarAlloc::LocalAt(name, pos) => stack.local_at_named(*pos, name.clone()),
            VarAlloc::Static(_name, var) => var.clone(),
        };
        match op {
            FnOpRepr::LoadGlobal { name } => {
                into.push(StackElement::Op(FnOp::LoadGlobal {
                    name: name.clone(),
                    target: store_at.clone(),
                }));
            }
            FnOpRepr::Assign { what, target } => {
                let var1 = to_var(stack, target);
                FnOpRepr::call_stack(stack, what.deref(), var1, into);
            }
            FnOpRepr::LoadStatic { value } => into.push(StackElement::Op(FnOp::LoadStatic {
                value: value.clone(),
                target: store_at.clone(),
            })),
            FnOpRepr::ReadVar { which } => into.push(StackElement::Op(FnOp::Assign {
                source: to_var(stack, which),
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
                    this: to_var(stack, this),
                    func: on_v.clone(),
                    args: arg_v.clone(),
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
                }));
                FnOpRepr::call_stack(stack, key, k_v.clone(), into);
                FnOpRepr::call_stack(stack, from, f_v.clone(), into);
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

                let var2 = stack.make_local();
                FnOpRepr::call_stack(stack, else_block, var2, into); // 3) load else block

                into.push(StackElement::Value(JsValue::Undefined));
                into.push(StackElement::Op(FnOp::Pop { n: 1 }));
                into.push(StackElement::Label(else_label.clone()));
                into.push(StackElement::Op(FnOp::ToLabel { id: end.clone() }));

                let var3 = stack.make_local();
                FnOpRepr::call_stack(stack, if_block, var3, into); // 4) load if block

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

                let var4 = stack.make_local();
                FnOpRepr::call_stack(stack, block, var4, into);
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

                let var5 = stack.make_local();
                FnOpRepr::call_stack(stack, block, var5, into);
                let var6 = stack.make_local();
                FnOpRepr::call_stack(stack, each, var6, into);
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

                let var7 = stack.make_local();
                FnOpRepr::call_stack(stack, initial, var7, into);
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
                    condition: temp_not.clone(),
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
                    captures: Rc::new(vars.iter().map(|va| to_var(stack, va)).collect()), // TODO straight to stack?
                    code: code.clone(),
                    target: store_at,
                }))
            }
            FnOpRepr::NativeCall {
                target,
                this,
                args,
                call,
            } => {
                into.push(StackElement::Op(FnOp::NativeCall {
                    this: to_var(stack, this),
                    func: call.clone(),
                    args: to_var(stack, args),
                    target: to_var(stack, target),
                }));
            }
        }
        //println!("{:?}\nwurde zu\n{:?}\n", op, &into[before_len..]);
    }
}
// TODO put #[cfg(debug_assertions)] in places
#[derive(Clone, Mark, Debug)]
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
    SetFunctionOffset {
        to: usize,
    },
    StorePop {
        target: JsVar,
        pop_count: usize,
    },
    NativeCall {
        this: JsVar,
        func: Rc<dyn NativeFunction>,
        args: JsVar,
        target: JsVar,
    },
    Expand {
        what: Rc<FnOpRepr>,
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
                let value2 = source.get(Some(stack));
                target.set(Some(stack), value2);
                FnOpAction::Nop
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
                let our_pos = stack.stack.latest_fn_pos;
                let upcoming_pos = stack.stack.stack.len() + 4;
                let mut result = vec![
                    StackElement::Op(FnOp::SetFunctionOffset { to: our_pos }),
                    StackElement::Value(u_undefined()),
                    StackElement::Op(FnOp::StorePop {
                        target: target.clone(),
                        pop_count: 1,
                    }),
                    StackElement::Label(stack.method_label()),
                    StackElement::Value(this.get(Some(stack))),
                    StackElement::Value(args.get(Some(stack))),
                    StackElement::Op(FnOp::ToLabel {
                        id: stack.method_label(),
                    }),
                ];
                let ops = func.get(Some(stack));
                match ops {
                    JsValue::Object(obj) => match &obj.borrow().borrow_mut().call {
                        JSCallable::NotCallable => { /*TODO*/ }
                        JSCallable::Js { creator, .. } => {
                            result.push(StackElement::Op(FnOp::Expand {
                                what: creator.borrow().ops.clone(),
                            }));
                            result.push(StackElement::Op(FnOp::SetFunctionOffset {
                                to: upcoming_pos,
                            }));
                        }
                        JSCallable::Native { op } => {
                            match op.native_call(this.get(Some(stack)), args.get(Some(stack))) {
                                Ok(ok) => {
                                    target.set(Some(stack), ok);
                                    return FnOpAction::Nop;
                                }
                                Err(err) => {
                                    let v = stack.make_local();
                                    v.set(Some(stack), err);
                                    return FnOpAction::One(StackElement::Op(FnOp::Throw {
                                        what: v,
                                    }));
                                }
                            }
                        }
                    },
                    _ => {
                        unimplemented!()
                    }
                }
                FnOpAction::Push(result)
            }
            FnOp::Throw { what } => FnOpAction::Two(
                StackElement::Op(FnOp::ToLabel {
                    id: stack.try_label(),
                }),
                StackElement::Value(what.get(Some(stack))),
            ),
            FnOp::Return { target } => FnOpAction::Two(
                StackElement::Op(FnOp::ToLabel {
                    id: stack.method_label(),
                }),
                StackElement::Value(target.get(Some(stack))),
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
                                    value: u_string(&format!(
                                        "cannot read key {} of undefined",
                                        key.get(Some(stack)).to_system_string()
                                    )),
                                    target: v.clone(),
                                }),
                                StackElement::Op(FnOp::Throw { what: v.clone() }),
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

                        if let Some(v) = GcCell::borrow(&obj.borrow())
                            .content
                            .get(&key_v.to_system_string())
                        {
                            target.set(Some(stack), v.value.clone());
                            FnOpAction::Nop
                        } else {
                            if key.get(Some(stack)).to_system_string().as_str() == "__proto__" {
                                // TODO :(
                                target.set(Some(stack), u_undefined());
                                return FnOpAction::Nop;
                            }
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
                        GcCell::borrow(&o1.borrow()).identity
                            == GcCell::borrow(&o2.borrow()).identity
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
                        &GcCell::borrow(&o1.borrow()).identity
                            == &GcCell::borrow(&o2.borrow()).identity
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
                            obj.borrow().borrow_mut().symbol_keys.insert(
                                key,
                                JsProperty {
                                    enumerable: false,
                                    configurable: false,
                                    writable: false,
                                    value: what.get(Some(stack)),
                                },
                            );
                        } else {
                            obj.borrow().borrow_mut().content.insert(
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
                        JsValue::String(Rc::new(s1.to_string() + val.to_system_string().as_str())),
                    ),
                    (val, JsValue::String(s1)) => target.set(
                        Some(stack),
                        JsValue::String(Rc::new(val.to_system_string().to_string() + s1.as_str())),
                    ),
                    (o @ JsValue::Object(_), val) => target.set(
                        Some(stack),
                        JsValue::String(Rc::new(
                            o.to_system_string().to_string() + val.to_system_string().as_str(),
                        )),
                    ),
                    (val, o @ JsValue::Object(_)) => target.set(
                        Some(stack),
                        JsValue::String(Rc::new(
                            val.to_system_string().to_string() + o.to_system_string().as_str(),
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
                let coerce1 = coerce(left.get(Some(stack)));
                let coerce2 = coerce(right.get(Some(stack)));
                target.set(Some(stack), u_number(coerce1 + coerce2));
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
                let value3 = u_bool(source.get(Some(stack)).truthy());
                target.set(Some(stack), value3);
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
                            captures,
                        }),
                    })
                    .build();
                target.set(Some(stack), f);
                FnOpAction::Nop
            }
            FnOp::Pop { n } => FnOpAction::Pop(n),
            FnOp::NativeCall {
                this,
                func,
                args,
                target,
            } => match func.native_call(this.get(Some(stack)), args.get(Some(stack))) {
                Ok(r) => {
                    target.set(Some(stack), r);
                    FnOpAction::Nop
                }
                Err(err) => {
                    let t_v = stack.make_local();
                    t_v.set(Some(stack), err);
                    FnOpAction::LabelWalkback {
                        id: stack.try_label(),
                    }
                }
            },
            FnOp::SetFunctionOffset { to } => FnOpAction::SetFnOffset { to },
            FnOp::StorePop { target, pop_count } => {
                assert!(pop_count >= 1);
                let value1 = stack.stack.stack.pop().unwrap().assume_val();
                target.set(Some(stack), value1);
                FnOpAction::Pop(pop_count - 0)
            }
            FnOp::Expand { what } => {
                let mut result = Vec::new();
                let var = stack.local_at(0);
                FnOpRepr::call_stack(stack, &what, var, &mut result);
                FnOpAction::Push(result)
            }
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
    //Five(StackElement, StackElement, StackElement, StackElement, StackElement),
    //Six(StackElement, StackElement, StackElement, StackElement, StackElement, StackElement),
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
    SetFnOffset {
        to: usize,
    },
}

#[derive(Mark, Clone, Debug)]
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
            any @ _ => {
                panic!(format!("stack corruption (5) {:?}", any))
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

#[derive(Mark, Clone, Debug)]
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
        return JsVar::Heap {
            name,
            value: Gc::new(GcCell::new(u_undefined())),
            defined: true,
        };
    }

    pub fn ensure_heap(&mut self) {
        match self {
            JsVar::Stack { pos: _, name } => {
                let name = name.clone();
                std::mem::swap(
                    self,
                    &mut JsVar::Heap {
                        name: name.clone(),
                        value: Gc::new(GcCell::new(JsValue::Undefined)),
                        defined: true,
                    },
                )
            }
            JsVar::Heap { .. } => {}
        }
    }

    pub fn set(&self, stack: Option<&mut StackAccess>, val: JsValue) {
        match self {
            JsVar::Stack { pos, .. } => {
                *stack
                    .expect("Stack variable get without stack!")
                    .write_stack_absolute(*pos) = StackElement::Value(val);
            }
            JsVar::Heap { value, .. } => {
                *value.borrow().borrow_mut() = val;
            }
        }
    }

    // TODO inline
    pub fn get(&self, stack: Option<&StackAccess>) -> JsValue {
        match self {
            JsVar::Stack { pos, .. } => stack
                .expect("Stack variable set without stack!")
                .read_stack_absolute(*pos)
                .assume_val(),
            JsVar::Heap { value, .. } => return value.borrow().borrow_mut().clone(),
        }
    }
}
