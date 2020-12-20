pub mod converter;
mod macros;
mod optimizer;

use crate::js::data::execution_v2::opcode::Arithmetic2Op;
use crate::js::data::js_types::{Identity, JsValue};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

#[derive(Clone)]
pub struct CodeLoc {
    pub(crate) line: usize,
    pub(crate) column: usize,
}

impl Debug for CodeLoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}

impl CodeLoc {
    pub fn formatted(&self, source: &str) -> String {
        let mut ret = String::new();
        ret.push_str("\u{001b}[0m");
        for (line_number, line) in source.split("\n").enumerate() {
            if line_number + 1 == self.line {
                let mut col = self.column;
                if col != 0 {
                    col -= 1;
                }
                if col != 0 {
                    col -= 1;
                }
                if line.len() < col + 1 {
                    ret.push_str("\u{001b}[41m");
                    ret.push_str(line);
                    ret.push_str("\u{001b}[0m");
                    continue;
                }
                ret.push_str(&line[..col]);
                ret.push_str("\u{001b}[42m");
                ret.push_str(&line[col..(col + 1)]);
                ret.push_str("\u{001b}[0m");
                ret.push_str(&line[(col + 1)..]);
            } else {
                ret.push_str(line);
            }
            ret.push('\n');
        }
        return ret;
    }
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
    AssignProp(Box<AssignProp>),
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
    pub(crate) globals: HashMap<Identity, Rc<String>>,
}

pub struct AssignProp {
    pub(crate) to: LocatedAction,
    pub(crate) key: LocatedAction,
    pub(crate) what: LocatedAction,
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
    pub(crate) this: Option<LocatedAction>,
    pub(crate) member: LocatedAction, // Used as key if this is Some
    pub(crate) args: LocatedAction,
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

    fn register_local(&self, id: Identity);

    fn get_globals(&self) -> HashMap<Identity, Rc<String>>;
}

impl VarAccessTrait for Rc<RefCell<VarAccess>> {
    fn known_heap_vars(&self) -> Vec<Identity> {
        let globals = self.get_globals(); // TODO being computed twice. Also not pretty
        self.borrow_mut()
            .known_heap_vars
            .iter()
            .map(|i| i.clone())
            .filter(|i| !globals.contains_key(i))
            .collect()
    }

    fn register_local(&self, id: Identity) {
        let mut ref_mut = self.borrow_mut();
        if ref_mut.stack_breaker {
            ref_mut.known_heap_vars.insert(id);
        } else if let Some(prev) = &ref_mut.prev {
            prev.register_local(id);
        }
    }

    fn get_or_global(&mut self, name: &Rc<String>) -> Identity {
        self.get_or_global_internal(name, false)
    }

    fn get_globals(&self) -> HashMap<Identity, Rc<String>> {
        if let Some(prev) = &self.borrow().prev {
            return prev.get_globals();
        } else {
            let mut ret = HashMap::new();

            for (name, id) in &self.borrow().vars {
                ret.insert(id.id, name.clone());
            }

            return ret;
        }
    }

    /// stack_broken means we have crossed a heap boundary and need to capture.
    fn get_or_global_internal(&mut self, name: &Rc<String>, stack_broken: bool) -> Identity {
        let mut ref_mut1 = self.borrow_mut();
        if let Some(ret) = ref_mut1.vars.get_mut(name) {
            if stack_broken {
                ret.is_local = false;
                return ret.id.clone();
            }
            return ret.id.clone();
        } else {
            let mut ref_mut = ref_mut1;
            let is_stack_breaker = ref_mut.stack_breaker;
            if let Some(prev) = &mut ref_mut.prev {
                let ret = prev.get_or_global_internal(name, stack_broken || is_stack_breaker);
                if ref_mut.stack_breaker {
                    ref_mut.known_heap_vars.insert(ret.clone());
                }
                return ret;
            } else {
                let ret = Identity::new();
                ref_mut.vars.insert(
                    name.clone(),
                    VarRef {
                        id: ret.clone(),
                        is_local: false,
                    },
                );
                ref_mut.known_heap_vars.insert(ret.clone());
                return ret;
            }
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
    {($($st:expr)*) @ $loc:expr} => {
        {
            LocatedAction {
                action: Action::Block(Box::from(ScopedBlock {
                    content: vec![
                        $($st,)*
                    ],
                    location: $loc
                })),
                location: $loc
            }
        }
    };
}

#[macro_export]
macro_rules! js_if_else {
    (($cond:expr) {$($ist:expr)*} else {$($est:expr)*} @ $loc:expr) => {
        LocatedAction {
            location: $loc,
            action: Action::IfElse(Box::new(IfElse {
                condition: $cond,
                if_block: js_block! {
                        ($($ist)*)
                        @ $loc
                },
                else_block: js_block! {
                        ($($est)*)
                        @ $loc
                }
            }))
        }
    }
}

#[macro_export]
macro_rules! js_if {
    (($cond:expr) {$($ist:expr)*} @ $loc:expr) => {
        {
            {
                js_if_else!(($cond) {$($ist:expr)*} else {} @ $loc)
            }
        }
    }
}

#[macro_export]
macro_rules! js_while {
    (($cond:expr) {$($body:expr)*} @ $loc:expr) => {
        {
            LocatedAction {
                action: Action::While(Box::from(While {
                    condition: $cond,
                    body: js_block! {($($body)*) @ $loc}
                })),
                location: $loc,
            }
        }
    };
}

#[macro_export]
macro_rules! js_for {
    (($init:expr; $condition:expr; $update:expr) {$($body:expr)*}  @ $loc:expr) => {
        {

            LocatedAction {
                action: Action::For(Box::from(For {
                    initializer: $init,
                    condition: $condition,
                    update: $update,
                    body: (js_block! {($(($body))*) @ $loc } )
                })),
                location: $loc,
            }
        }
    };
}

#[macro_export]
macro_rules! js_bin {
    (($left:expr) == ($right:expr) @ $loc:expr) => {{
        LocatedAction {
            action: Action::FuzzyCompare(Box::new(LeftRight {
                left: $left,
                right: $right,
            })),
            location: $loc,
        }
    }};
    (($left:expr) === ($right:expr) @ $loc:expr) => {{
        LocatedAction {
            action: Action::StrictCompare(Box::new(LeftRight {
                left: $left,
                right: $right,
            })),
            location: $loc,
        }
    }};
    (($left:expr) != ($right:expr) @ $loc:expr) => {{
        LocatedAction {
            action: Action::BoolNot(Box::new(LocatedAction {
                action: Action::FuzzyCompare(Box::new(LeftRight {
                    left: $left,
                    right: $right,
                })),
                location: $loc,
            })),
            location: $loc,
        }
    }};
    (($left:expr) !== ($right:expr) @ $loc:expr) => {{
        LocatedAction {
            action: Action::BoolNot(Box::new(LocatedAction {
                action: Action::StrictCompare(Box::new(LeftRight {
                    left: $left,
                    right: $right,
                })),
                location: $loc,
            })),
            location: $loc,
        }
    }};
    (($left:expr) + ($right:expr) @ $loc:expr) => {{
        LocatedAction {
            action: Action::Arithmetic2(Box::new(Arithmetic2Action {
                left_right: LeftRight {
                    left: $left,
                    right: $right,
                },
                variant: Arithmetic2Op::Add,
            })),
            location: $loc,
        }
    }};
    (($left:expr) - ($right:expr) @ $loc:expr) => {{
        LocatedAction {
            action: Action::Arithmetic2(Box::new(Arithmetic2Action {
                left_right: LeftRight {
                    left: $left,
                    right: $right,
                },
                variant: Arithmetic2Op::Sub,
            })),
            location: $loc,
        }
    }};
    (($left:expr) * ($right:expr) @ $loc:expr) => {{
        LocatedAction {
            action: Action::Arithmetic2(Box::new(Arithmetic2Action {
                left_right: LeftRight {
                    left: $left,
                    right: $right,
                },
                variant: Arithmetic2Op::Multi,
            })),
            location: $loc,
        }
    }};
    (($left:expr) / ($right:expr) @ $loc:expr) => {{
        LocatedAction {
            action: Action::Arithmetic2(Box::new(Arithmetic2Action {
                left_right: LeftRight {
                    left: $left,
                    right: $right,
                },
                variant: Arithmetic2Op::Div,
            })),
            location: $loc,
        }
    }};
    (($left:expr) ** ($right:expr)  @ $loc:expr) => {{
        LocatedAction {
            action: Action::Arithmetic2(Box::new(Arithmetic2Action {
                left_right: LeftRight {
                    left: $left,
                    right: $right,
                },
                variant: Arithmetic2Op::Pow,
            })),
            location: $loc,
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
    (($val:expr) @ $loc:expr) => {{
        LocatedAction {
            action: Action::Literal($val),
            location: $loc,
        }
    }};
}

#[macro_export]
macro_rules! js_var {
    (($name:expr) = ($to:expr) @ $loc:expr) => {{
        LocatedAction {
            action: Action::VariableAssign(Box::new(VarAssign {
                var: $name,
                right_side: Box::new($to),
            })),
            location: $loc,
        }
    }};
    (($name:expr) @ $loc:expr) => {{
        LocatedAction {
            action: Action::ReadVar($name),
            location: $loc,
        }
    }};
}

#[macro_export]
macro_rules! js_prop {
    (($of:expr) $([$index:expr])+[$other:expr] @ $loc:expr) => {
        Action::ReadProp(Box::new(ReadProp {
            from: (js_prop!(($of) $([$index])+  @ $loc)),
            key: $other
        }))
    };
    (($of:expr)[$index:expr] @ $loc:expr) => {
        {
            LocatedAction{ action: Action::ReadProp(Box::new(crate::js::data::intermediate::ReadProp {
                from: $of,
                key: $index
            })), location: $loc}
        }
    }
}

#[macro_export]
macro_rules! js_call {
    (($callee:expr)(args: $args: expr) @ $loc:expr) => {
        LocatedAction {
            action: Action::Call(Box::new(CallFunction {
                this: None,
                member: $callee,
                args: $args,
            })),
            location: $loc,
        }
    };
    (($this:expr)[$callee:expr](args: $args: expr)) => {};
}

macro_rules! js_lit {
    ([$($el:expr)*] -> $arr_var:ident @ $loc:expr) => {{
        let mut counter = 0.0;
        js_block! {
            $(
                {
                    let v = Action::AssignProp(Box::new(AssignProp {
                        to: js_var!($arr_var),
                        key: Action::Literal(JsValue::Number(counter)),
                        what: $el
                    }));
                    counter += 1;
                    v
                }
            )*
             @ $loc
        }
    }};
}

fn temp() {}
