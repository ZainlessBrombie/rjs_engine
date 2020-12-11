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
    left_right: LeftRight,
    variant: Arithmetic2Op,
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
    from: LocatedAction,
    key: LocatedAction,
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

impl<'a> VarAccess<'a> {
    pub fn empty() -> VarAccess<'a> {
        let self_time = SelfTime::new();
        return VarAccess {
            known_heap_vars: Default::default(),
            prev: None,
            vars: Default::default(),
            stack_breaker: false,
            keep_alive: self_time,
        };
    }

    pub fn child<'b>(&'a self, stack_breaker: bool) -> &'a mut VarAccess<'b>
    where
        'b: 'a,
    {
        let time = Default::default();
        VarAccess {
            known_heap_vars: Default::default(),
            prev: None,
            vars: Default::default(),
            stack_breaker,
            keep_alive: time,
        };
        unimplemented!()
    }

    pub fn known_heap_vars(&self) -> Vec<Identity> {
        self.known_heap_vars.iter().map(|i| i.clone()).collect()
    }

    pub fn get_or_global(&mut self, name: &Rc<String>) -> Identity {
        self.get_or_global_internal(name, false)
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
            if let Some(prev) = &mut self.prev {
                let ret = prev.get_or_global_internal(name, stack_broken || self.stack_breaker);
                if self.stack_breaker {
                    self.known_heap_vars.insert(ret.clone());
                }
                return ret;
            }
            let ret = Identity::new();
            self.vars.insert(
                name.clone(),
                VarRef {
                    id: ret.clone(),
                    is_local: false,
                },
            );
            self.known_heap_vars.insert(ret.clone());
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
    known_heap_vars: HashSet<Identity>,
    prev: Option<RefMut<'a, VarAccess<'a>>>,
    vars: HashMap<Rc<String>, VarRef>,
    stack_breaker: bool,
    keep_alive: SelfTime<'a>, // Just keep em all
}

#[macro_export]
macro_rules! lifetimed {
    ($f:expr) => {{
        fn lifetimed<'a, 'b>(
            f: impl FnOnce(&'a mut VarAccess<'b>) -> LocatedAction,
        ) -> impl FnOnce(&'a mut VarAccess<'b>) -> LocatedAction
        where
            'b: 'a,
        {
            return f;
        }
        lifetimed($f)
    }};
}

#[macro_export]
macro_rules! js_block {
    {$($st:expr)*} => {
        {
            lifetimed!((|a| {
                let mut access = a.child(false);

                let ret = LocatedAction {
                    action: Action::Block(Box::from(ScopedBlock {
                        content: vec![
                            $($st(access),)*
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
            lifetimed!((|a| {
                let condition = $cond(a.child(false));
                let if_block = js_block! {
                                    $($ist)*
                               }(a.child(false));
                let else_block = js_block! {
                                    $($est)*
                                 }(a.child(false));
                LocatedAction {
                    location: CodeLoc { line: 0, column: 0 },
                    action: Action::IfElse(Box::new(IfElse {
                        condition,

                        if_block,
                        else_block
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
            lifetimed!(|a: &mut VarAccess| {
                js_if_else!(($cond) {$($ist:expr)*} else {})(a)
            })
        }
    }
}

#[macro_export]
macro_rules! js_action_lit {
    ($a:ident, $ex:expr) => {{
        lifetimed!((|$a| $ex))
    }};
}

#[macro_export]
macro_rules! js_while {
    (($cond:expr) {$($body:expr)*}) => {
        {
            lifetimed!((|a: &mut VarAccess| LocatedAction {
                action: Action::While(Box::from(While {
                    condition: $cond(a),
                    body: js_block! {$($body)*}(a)
                })),
                location: CodeLoc { line: 0, column: 0 },
            }))
        }
    };
}

#[macro_export]
macro_rules! js_for {
    (($init:expr; $condition:expr; $update:expr) {$($body:expr)*}) => {
        {
            lifetimed!((|a| {
                let a = a.child(false);
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

#[macro_export]
macro_rules! js_bin {
    (($left:expr) == ($right:expr)) => {{
        lifetimed!(|a: &mut VarAccess| LocatedAction {
            action: Action::FuzzyCompare(Box::new(LeftRight {
                left: $left(a),
                right: $right(a),
            })),
            location: CodeLoc { line: 0, column: 0 },
        })
    }};
    (($left:expr) === ($right:expr)) => {{
        lifetimed!(|a| LocatedAction {
            action: Action::StrictCompare(Box::new(LeftRight {
                left: $left(a),
                right: $right(a),
            })),
            location: CodeLoc { line: 0, column: 0 },
        })
    }};
    (($left:expr) != ($right:expr)) => {{
        lifetimed!(|a| LocatedAction {
            action: Action::BoolNot(Box::new(LocatedAction {
                action: Action::FuzzyCompare(Box::new(LeftRight {
                    left: $left(a),
                    right: $right(a),
                })),
                location: CodeLoc { line: 0, column: 0 },
            })),
            location: CodeLoc { line: 0, column: 0 },
        })
    }};
    (($left:expr) !== ($right:expr)) => {{
        lifetimed!(|a| LocatedAction {
            action: Action::BoolNot(Box::new(LocatedAction {
                action: Action::StrictCompare(Box::new(LeftRight {
                    left: $left(a),
                    right: $right(a),
                })),
                location: CodeLoc { line: 0, column: 0 },
            })),
            location: CodeLoc { line: 0, column: 0 },
        })
    }};
    (($left:expr) + ($right:expr)) => {{
        lifetimed!(|a| LocatedAction {
            action: Action::Arithmetic2(Box::new(Arithmetic2Action {
                left_right: LeftRight {
                    left: $left(a),
                    right: $right(a),
                },
                variant: Arithmetic2Op::Add,
            })),
            location: CodeLoc { line: 0, column: 0 },
        })
    }};
    (($left:expr) - ($right:expr)) => {{
        lifetimed!(|a| LocatedAction {
            action: Action::Arithmetic2(Box::new(Arithmetic2Action {
                left_right: LeftRight {
                    left: $left(a),
                    right: $right(a),
                },
                variant: Arithmetic2Op::Sub,
            })),
            location: CodeLoc { line: 0, column: 0 },
        })
    }};
    (($left:expr) * ($right:expr)) => {{
        lifetimed!(|a| LocatedAction {
            action: Action::Arithmetic2(Box::new(Arithmetic2Action {
                left_right: LeftRight {
                    left: $left(a),
                    right: $right(a),
                },
                variant: Arithmetic2Op::Multi,
            })),
            location: CodeLoc { line: 0, column: 0 },
        })
    }};
    (($left:expr) / ($right:expr)) => {{
        lifetimed!(|a| LocatedAction {
            action: Action::Arithmetic2(Box::new(Arithmetic2Action {
                left_right: LeftRight {
                    left: $left(a),
                    right: $right(a),
                },
                variant: Arithmetic2Op::Div,
            })),
            location: CodeLoc { line: 0, column: 0 },
        })
    }};
    (($left:expr) ** ($right:expr)) => {{
        lifetimed!(|a| LocatedAction {
            action: Action::Arithmetic2(Box::new(Arithmetic2Action {
                left_right: LeftRight {
                    left: $left(a),
                    right: $right(a),
                },
                variant: Arithmetic2Op::Pow,
            })),
            location: CodeLoc { line: 0, column: 0 },
        })
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
        lifetimed!(|a| LocatedAction {
            action: Action::Literal($val),
            location: CodeLoc { line: 0, column: 0 },
        })
    }};
}

#[macro_export]
macro_rules! js_var {
    (($name:expr) = $to:expr) => {{
        lifetimed!(|a| LocatedAction {
            action: Action::VariableAssign(Box::new(VarAssign {
                var: $name,
                right_side: Box::new($to(a)),
            })),
            location: CodeLoc { line: 0, column: 0 },
        })
    }};
    ($name:expr) => {{
        lifetimed!(|a| LocatedAction {
            action: Action::ReadVar($name),
            location: CodeLoc { line: 0, column: 0 },
        })
    }};
}

#[macro_export]
macro_rules! js_prop {
    (($of:expr) $([$index:expr])+[$other:expr]) => {
        lifetimed!(|a| Action::ReadProp(Box::new(ReadProp {
            from: (js_index(($of) $([$index])+))(a),
            key: $other(a)
        })))
    };
    (($of:expr)[$index:expr]) => {
        {
            lifetimed!(|a| LocatedAction{ action: Action::ReadProp(Box::new(crate::js::data::intermediate::ReadProp {
                from: $of(a),
                key: $index(a)
            })), location: CodeLoc {line:0, column: 0}})
        }
    }
}

fn temp() {}
