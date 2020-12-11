pub mod converter;
mod macros;

use crate::js::data::execution_v2::opcode::Arithmetic2Op;
use crate::js::data::js_types::{Identity, JsValue};
use crate::js::data::self_time::SelfTime;
use std::cell::{RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub struct CodeLoc {
    pub(crate) line: usize,
    pub(crate) column: usize,
}

pub struct Module {
    statements: Vec<ScopedBlock>,
    location: CodeLoc,
}

pub struct ScopedBlock {
    pub(crate) content: Vec<LocatedAction>,
    pub(crate) location: CodeLoc,
}

pub struct LocatedAction {
    pub(crate) action: Action,
    pub(crate) location: CodeLoc,
}

pub enum Action {
    LoadGlobal(Rc<String>),
    VariableDeclare(Box<VarDecl>),
    VariableAssign(Box<VarAssign>),
    Literal(JsValue),
    ReadVar(Identity),
    Call(Box<CallFunction>),
    Throw(Box<LocatedAction>),
    Return(Box<Action>),
    Break(Break),
    Labeled(Labeled),
    ReadProp(Box<ReadProp>),
    IfElse(Box<IfElse>),
    While(Box<While>),
    For(Box<For>),
    Nop,
    BoolAnd(Box<LeftRight>),
    BoolOr(Box<LeftRight>),
    BoolNot(Box<LocatedAction>),
    FuzzyCompare(Box<LeftRight>),
    StrictCompare(Box<LeftRight>),
    TypeOf(Box<Action>),
    Await(Box<Action>),
    Add(Box<LeftRight>),
    Arithmetic2(Box<Arithmetic2Action>),
    NewObject(Box<NewObject>),
    InstantiateFunction(Box<InstantiateFunction>),
    Block(Box<ScopedBlock>),
}

pub struct Labeled {
    label: Rc<String>,
}

pub struct Break {
    name: Rc<String>, // Let's name the default one #default# for now
}

pub struct InstantiateFunction {
    pub(crate) name: Option<LocatedAction>,
    //args: Vec<Action>,
    pub(crate) content: LocatedAction,
    pub(crate) captures: Vec<Identity>,
}

pub struct NewObject {
    pub(crate) is_array: bool,
}

pub struct Arithmetic2Action {
    pub(crate) left_right: LeftRight,
    pub(crate) variant: Arithmetic2Op,
}

pub struct LeftRight {
    pub(crate) left: LocatedAction,
    pub(crate) right: LocatedAction,
}

pub struct For {
    pub(crate) initializer: LocatedAction,
    pub(crate) condition: LocatedAction,
    pub(crate) update: LocatedAction,
    pub(crate) body: LocatedAction,
}

pub struct IfElse {
    pub(crate) condition: LocatedAction,
    pub(crate) if_block: LocatedAction,
    pub(crate) else_block: LocatedAction,
}

pub struct While {
    pub(crate) condition: LocatedAction,
    pub(crate) body: LocatedAction,
}

pub struct ReadProp {
    pub(crate) from: LocatedAction,
    pub(crate) key: LocatedAction,
}

pub struct CallFunction {
    pub(crate) this: Option<Action>,
    pub(crate) member: Action, // Used as key if this is Some
    pub(crate) args: Action,
}

pub struct VarDecl {
    pub(crate) var_type: VarType,
    pub(crate) assign: VarAssign,
}

pub struct VarAssign {
    pub(crate) var: Identity,
    pub(crate) right_side: Box<LocatedAction>,
}

pub enum VarType {
    Const,
    Let,
    Var,
}

struct VarRef {
    id: Identity,
    is_local: bool,
}

pub fn empty_var_access(
    prev: Option<Rc<RefCell<VarAccess>>>,
    stack_breaker: bool,
) -> Rc<RefCell<VarAccess>> {
    Rc::new(RefCell::new(VarAccess {
        known_heap_vars: Default::default(),
        prev,
        vars: Default::default(),
        stack_breaker,
    }))
}

pub trait VarAccessTrait {
    fn known_heap_vars(&self) -> Vec<Identity>;

    fn get_or_global(&mut self, name: &Rc<String>) -> Identity;

    fn get_or_global_internal(&mut self, name: &Rc<String>, stack_broken: bool) -> Identity;

    fn local_declare(&mut self, name: Rc<String>) -> Identity;
}

impl VarAccessTrait for Rc<RefCell<VarAccess>> {
    fn known_heap_vars(&self) -> Vec<Identity> {
        self.borrow_mut()
            .known_heap_vars
            .iter()
            .map(|i| i.clone())
            .collect()
    }

    fn get_or_global(&mut self, name: &Rc<String>) -> Identity {
        self.get_or_global_internal(name, false)
    }

    /// stack_broken means we have crossed a heap boundary and need to capture.
    fn get_or_global_internal(&mut self, name: &Rc<String>, stack_broken: bool) -> Identity {
        if let Some(ret) = self.borrow_mut().vars.get_mut(name) {
            if stack_broken {
                ret.is_local = false;
                return ret.id.clone();
            }
            return ret.id.clone();
        } else {
            if let Some(prev) = &mut self.borrow_mut().prev {
                let ret = prev
                    .get_or_global_internal(name, stack_broken || self.borrow_mut().stack_breaker);
                if self.borrow_mut().stack_breaker {
                    self.borrow_mut().known_heap_vars.insert(ret.clone());
                }
                return ret;
            }
            let ret = Identity::new();
            self.borrow_mut().vars.insert(
                name.clone(),
                VarRef {
                    id: ret.clone(),
                    is_local: false,
                },
            );
            self.borrow_mut().known_heap_vars.insert(ret.clone());
            return ret;
        }
    }

    fn local_declare(&mut self, name: Rc<String>) -> Identity {
        let ret = Identity::new();
        self.borrow_mut().vars.insert(
            name,
            VarRef {
                id: ret.clone(),
                is_local: true,
            },
        );
        return ret;
    }
}

pub struct VarAccess {
    known_heap_vars: HashSet<Identity>,
    prev: Option<Rc<RefCell<VarAccess>>>,
    vars: HashMap<Rc<String>, VarRef>,
    stack_breaker: bool,
}

#[macro_export]
macro_rules! js_block {
    {$($st:expr)*} => {
        {
            LocatedAction {
                action: Action::Block(Box::from(ScopedBlock {
                    content: vec![
                        $($st,)*
                    ],
                    location: CodeLoc { line: 0, column: 0 }
                })),
                location: CodeLoc { line: 0, column: 0 }
            }
        }
    };
}

#[macro_export]
macro_rules! js_if_else {
    (($cond:expr) {$($ist:expr)*} else {$($est:expr)*}) => {
        LocatedAction {
            location: CodeLoc { line: 0, column: 0 },
            action: Action::IfElse(Box::new(IfElse {
                condition: $cond,
                if_block: js_block! {
                        $($ist)*
                },
                else_block: js_block! {
                        $($est)*
                }
            }))
        }
    }
}

#[macro_export]
macro_rules! js_if {
    (($cond:expr) {$($ist:expr)*}) => {
        {
            {
                js_if_else!(($cond) {$($ist:expr)*} else {})
            }
        }
    }
}

#[macro_export]
macro_rules! js_while {
    (($cond:expr) {$($body:expr)*}) => {
        {
            LocatedAction {
                action: Action::While(Box::from(While {
                    condition: $cond,
                    body: js_block! {$($body)*}
                })),
                location: CodeLoc { line: 0, column: 0 },
            }
        }
    };
}

#[macro_export]
macro_rules! js_for {
    (($init:expr; $condition:expr; $update:expr) {$($body:expr)*}) => {
        {

            LocatedAction {
                action: Action::For(Box::from(For {
                    initializer: $init,
                    condition: $condition,
                    update: $update,
                    body: (js_block! {$($body)*})
                })),
                location: CodeLoc { line: 0, column: 0 },
            }
        }
    };
}

#[macro_export]
macro_rules! js_bin {
    (($left:expr) == ($right:expr)) => {{
        LocatedAction {
            action: Action::FuzzyCompare(Box::new(LeftRight {
                left: $left,
                right: $right,
            })),
            location: CodeLoc { line: 0, column: 0 },
        }
    }};
    (($left:expr) === ($right:expr)) => {{
        LocatedAction {
            action: Action::StrictCompare(Box::new(LeftRight {
                left: $left,
                right: $right,
            })),
            location: CodeLoc { line: 0, column: 0 },
        }
    }};
    (($left:expr) != ($right:expr)) => {{
        LocatedAction {
            action: Action::BoolNot(Box::new(LocatedAction {
                action: Action::FuzzyCompare(Box::new(LeftRight {
                    left: $left,
                    right: $right,
                })),
                location: CodeLoc { line: 0, column: 0 },
            })),
            location: CodeLoc { line: 0, column: 0 },
        }
    }};
    (($left:expr) !== ($right:expr)) => {{
        LocatedAction {
            action: Action::BoolNot(Box::new(LocatedAction {
                action: Action::StrictCompare(Box::new(LeftRight {
                    left: $left,
                    right: $right,
                })),
                location: CodeLoc { line: 0, column: 0 },
            })),
            location: CodeLoc { line: 0, column: 0 },
        }
    }};
    (($left:expr) + ($right:expr)) => {{
        LocatedAction {
            action: Action::Arithmetic2(Box::new(Arithmetic2Action {
                left_right: LeftRight {
                    left: $left,
                    right: $right,
                },
                variant: Arithmetic2Op::Add,
            })),
            location: CodeLoc { line: 0, column: 0 },
        }
    }};
    (($left:expr) - ($right:expr)) => {{
        LocatedAction {
            action: Action::Arithmetic2(Box::new(Arithmetic2Action {
                left_right: LeftRight {
                    left: $left,
                    right: $right,
                },
                variant: Arithmetic2Op::Sub,
            })),
            location: CodeLoc { line: 0, column: 0 },
        }
    }};
    (($left:expr) * ($right:expr)) => {{
        LocatedAction {
            action: Action::Arithmetic2(Box::new(Arithmetic2Action {
                left_right: LeftRight {
                    left: $left,
                    right: $right,
                },
                variant: Arithmetic2Op::Multi,
            })),
            location: CodeLoc { line: 0, column: 0 },
        }
    }};
    (($left:expr) / ($right:expr)) => {{
        LocatedAction {
            action: Action::Arithmetic2(Box::new(Arithmetic2Action {
                left_right: LeftRight {
                    left: $left,
                    right: $right,
                },
                variant: Arithmetic2Op::Div,
            })),
            location: CodeLoc { line: 0, column: 0 },
        }
    }};
    (($left:expr) ** ($right:expr)) => {{
        LocatedAction {
            action: Action::Arithmetic2(Box::new(Arithmetic2Action {
                left_right: LeftRight {
                    left: $left,
                    right: $right,
                },
                variant: Arithmetic2Op::Pow,
            })),
            location: CodeLoc { line: 0, column: 0 },
        }
    }};
}

#[macro_export]
macro_rules! js_primitive {
    ($what:ident) => {{
        trait ToJs {
            fn to_js(&self) -> JsValue;
        }
        impl ToJs for String {
            fn to_js(&self) -> JsValue {
                JsValue::String(Rc::new(self.to_string()))
            }
        }
        impl ToJs for &str {
            fn to_js(&self) -> JsValue {
                JsValue::String(Rc::new(self.to_string()))
            }
        }
        impl ToJs for f64 {
            fn to_js(&self) -> JsValue {
                JsValue::Number(*self)
            }
        }
        impl ToJs for usize {
            fn to_js(&self) -> JsValue {
                JsValue::Number(*self as f64)
            }
        }
        ToJs::to_js(&$what)
    }};
    ($what:literal) => {{
        trait ToJs {
            fn to_js(&self) -> JsValue;
        }
        impl ToJs for String {
            fn to_js(&self) -> JsValue {
                JsValue::String(Rc::new(self.to_string()))
            }
        }
        impl ToJs for &str {
            fn to_js(&self) -> JsValue {
                JsValue::String(Rc::new(self.to_string()))
            }
        }
        impl ToJs for f64 {
            fn to_js(&self) -> JsValue {
                JsValue::Number(*self)
            }
        }
        impl ToJs for usize {
            fn to_js(&self) -> JsValue {
                JsValue::Number(*self as f64)
            }
        }
        ToJs::to_js(&$what)
    }};
}

#[macro_export]
macro_rules! js_static {
    ($val:expr) => {{
        LocatedAction {
            action: Action::Literal($val),
            location: CodeLoc { line: 0, column: 0 },
        }
    }};
}

#[macro_export]
macro_rules! js_var {
    (($name:expr) = $to:expr) => {{
        LocatedAction {
            action: Action::VariableAssign(Box::new(VarAssign {
                var: $name,
                right_side: Box::new($to),
            })),
            location: CodeLoc { line: 0, column: 0 },
        }
    }};
    ($name:expr) => {{
        LocatedAction {
            action: Action::ReadVar($name),
            location: CodeLoc { line: 0, column: 0 },
        }
    }};
}

#[macro_export]
macro_rules! js_prop {
    (($of:expr) $([$index:expr])+[$other:expr]) => {
        Action::ReadProp(Box::new(ReadProp {
            from: (js_index(($of) $([$index])+)),
            key: $other
        }))
    };
    (($of:expr)[$index:expr]) => {
        {
            LocatedAction{ action: Action::ReadProp(Box::new(crate::js::data::intermediate::ReadProp {
                from: $of,
                key: $index
            })), location: CodeLoc {line:0, column: 0}}
        }
    }
}

fn temp() {}
