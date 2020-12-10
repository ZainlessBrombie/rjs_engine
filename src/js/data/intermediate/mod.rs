mod macros;

use crate::js::data::execution_v2::opcode::Arithmetic2Op;
use crate::js::data::js_types::{Identity, JsValue};
use std::collections::HashMap;
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
    BoolNot(Box<Action>),
    FuzzyCompare(Box<LeftRight>),
    StrictCompare(Box<Box<LeftRight>>),
    TypeOf(Box<Action>),
    Await(Box<Action>),
    Add(Box<Box<LeftRight>>),
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
    name: Option<LocatedAction>,
    args: Vec<Action>,
    content: ScopedBlock,
}

pub struct NewObject {
    pub(crate) is_array: bool,
}

pub struct Arithmetic2Action {
    left_right: LeftRight,
    variant: Arithmetic2Op,
}

pub struct LeftRight {
    left: Action,
    right: Action,
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
    pub(crate) body: Action,
}

pub struct ReadProp {
    from: Action,
    key: Action,
}

pub struct CallFunction {
    this: Option<Action>,
    member: Action, // Used as key if this is Some
    args: Action,
}

pub struct VarDecl {
    var_type: VarType,
    assign: VarAssign,
}

pub struct VarAssign {
    pub(crate) var: Identity,
    pub(crate) right_side: Box<Action>,
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

impl<'a> VarAccess<'a> {
    pub fn empty(prev: Option<&'a mut VarAccess<'a>>, stack_breaker: bool) -> VarAccess<'a> {
        VarAccess {
            prev,
            vars: Default::default(),
            stack_breaker,
        }
    }

    /// stack_broken means we have crossed a heap boundary and need to capture.
    pub fn get_or_global_internal(&mut self, name: &Rc<String>, stack_broken: bool) -> Identity {
        if let Some(ret) = self.vars.get_mut(name) {
            if stack_broken {
                ret.is_local = false;
                return ret.id.clone();
            }
            return ret.id.clone();
        } else {
            if let Some(prev) = self.prev {
                return prev.get_or_global_internal(name, stack_broken || self.stack_breaker);
            }
            let ret = Identity::new();
            self.vars.insert(
                name.clone(),
                VarRef {
                    id: ret.clone(),
                    is_local: false,
                },
            );
            return ret;
        }
    }

    pub fn local_declare(&mut self, name: Rc<String>) -> Identity {
        let ret = Identity::new();
        self.vars.insert(
            name,
            VarRef {
                id: ret.clone(),
                is_local: true,
            },
        );
        return ret;
    }
}

pub struct VarAccess<'a> {
    prev: Option<&'a mut VarAccess<'a>>,
    vars: HashMap<Rc<String>, VarRef>,
    stack_breaker: bool,
}

#[macro_export]
macro_rules! js_block {
    {$($st:expr)*} => {
        {
            fn lifetimed<'a>(f: impl Fn(&'a mut VarAccess<'a>) -> LocatedAction) -> impl Fn(&'a mut VarAccess<'a>) -> LocatedAction {
                return f;
            }
            lifetimed((|a| {
                let mut access = VarAccess::empty(Some(a), false);
                let ret = LocatedAction {
                    action: Action::Block(Box::from(ScopedBlock {
                        content: vec![
                            $($st(&mut a),)*
                        ],
                        location: CodeLoc { line: 0, column: 0 }
                    })),
                    location: CodeLoc { line: 0, column: 0 }
                };
                ret
            }))
        }
    };
}

#[macro_export]
macro_rules! js_if_else {
    (($cond:expr) {$($ist:expr)*} else {$($est:expr)*}) => {
        {
            fn lifetimed<'a>(f: impl Fn(&'a mut VarAccess<'a>) -> LocatedAction) -> impl Fn(&'a mut VarAccess<'a>) -> LocatedAction {
                return f;
            }
            lifetimed((|a| {
                LocatedAction {
                    location: CodeLoc { line: 0, column: 0 },
                    action: Action::IfElse(Box::new(IfElse {
                        condition: $cond(a),

                        if_block: js_block! {
                                $($ist)*
                            }(&mut VarAccess::empty(Some(a), false)),
                        else_block: js_block! {
                                $($est)*
                            }(&mut VarAccess::empty(Some(a), false))
                    }))
                }
            }))
        }
    }
}

#[macro_export]
macro_rules! js_if {
    (($cond:expr) {$($ist:expr)*}) => {
        {
            fn lifetimed<'a>(f: impl Fn(&'a mut VarAccess<'a>) -> LocatedAction) -> impl Fn(&'a mut VarAccess<'a>) -> LocatedAction {
                return f;
            }
            (|a: &mut VarAccess| {
                js_if_else!(($cond) {$($ist:expr)*} else {})(a)
            })
        }
    }
}

#[macro_export]
macro_rules! js_action_lit {
    ($a:ident, $ex:expr) => {{
        fn lifetimed<'a>(
            f: impl Fn(&'a mut VarAccess<'a>) -> LocatedAction,
        ) -> impl Fn(&'a mut VarAccess<'a>) -> LocatedAction {
            return f;
        }
        lifetimed((|$a| $ex))
    }};
}

#[macro_export]
macro_rules! js_while {
    (($cond:expr) {$($body:expr)*}) => {
        {
            fn lifetimed<'a>(f: impl Fn(&'a mut VarAccess<'a>) -> LocatedAction) -> impl Fn(&'a mut VarAccess<'a>) -> LocatedAction {
                return f;
            }
            (|a: &mut VarAccess| LocatedAction {
                action: Action::While(Box::from(While {
                    condition: $cond(a),
                    body: js_block! {$($body)*}(a)
                })),
                location: CodeLoc { line: 0, column: 0 },
            })
        }
    };
}

#[macro_export]
macro_rules! js_for {
    (($init:expr; $condition:expr; $update:expr) {$($body:expr)*}) => {
        {
            fn lifetimed<'a>(f: impl Fn(&'a mut VarAccess<'a>) -> LocatedAction) -> impl Fn(&'a mut VarAccess<'a>) -> LocatedAction {
                return f;
            }
            lifetimed((|a| {
                let a = VarAccess::empty(Some(a), false);
                LocatedAction {
                    action: Action::For(Box::from(For {
                        initializer: $init(&mut a),
                        condition: $condition(&mut a),
                        update: $update(&mut a),
                        body: (js_block! {$($body)*})(&mut a)
                    })),
                    location: CodeLoc { line: 0, column: 0 },
                }
            }))
        }
    };
}
