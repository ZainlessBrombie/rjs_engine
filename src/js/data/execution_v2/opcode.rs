use crate::js::data::execution_v2::function::OpFunction;
use crate::js::data::intermediate::CodeLoc;
use crate::js::data::js_execution::JsVar;
use crate::js::data::js_types::JsValue;
use std::rc::Rc;

/// An op, which the function consists of.
/// Target refers to the relative local stack pointer.
#[derive(Debug)]
pub struct Op {
    pub target: Target,
    pub code: OpCode,
    pub loc: CodeLoc,
}

/// A memory target for a value read or write
#[derive(Clone, Debug)]
pub enum Target {
    /// Stack pointer. May be a Value (used as a variable) or a Heap Var
    Stack(usize),
    /// Global object is stored at stack 0
    Global(Rc<String>),
    /// Don't put value anywhere. Undefined read, nop write.
    BlackHole,
}

/// An opcode.
/// If they don't have a target, target is set to BlackHole
#[derive(Debug)]
pub enum OpCode {
    Jump {
        to: usize,
    },
    ConditionalJump {
        to: usize,
    },
    NewObject {
        is_array: bool,
    },
    Not {
        source: Target,
    },
    Assign {
        source: Target,
    },
    Static {
        value: JsValue,
    },
    CreateFunction {
        template: Rc<OpFunction>,
        /// Ordered list of captures to read.
        /// Indexes into our local stack varspace, as it will contain captures itself.
        captures: Vec<Target>,
    },
    Call {
        what: Target,
        this: Target,
        args: Target,
    },
    Throw {
        what: Target,
    },
    Return {
        what: Target,
    },
    ReadProp {
        from: Target,
        key: Target,
    },
    Nop {},
    Transfer {
        from: Target,
    },
    FuzzyCompare {
        left: Target,
        right: Target,
    },
    StrictCompare {
        left: Target,
        right: Target,
    },
    TypeOf {
        what: Target,
    },
    Await {
        what: Target,
    },
    AssignProp {
        of: Target,
        key: Target,
        value: Target,
    },
    /// Fuzzy add, can concatenate strings
    Add {
        left: Target,
        right: Target,
    },
    /// May become NaN
    Arithmetic2 {
        left: Target,
        right: Target,
        variant: Arithmetic2Op,
    },
}

/// Arithmetic op that takes exactly two parameters
#[derive(Clone, Copy, Debug)]
pub enum Arithmetic2Op {
    Add,
    Sub,
    Multi,
    Div,
    Pow,
}
