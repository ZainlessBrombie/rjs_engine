use crate::js::data::execution_v2::opcode::{Op, OpCode};
use crate::js::data::execution_v2::var::JsVar;
use std::rc::Rc;

/// The opcodes belonging to a function
pub struct OpFunction {
    pub instructions: Vec<Op>,
    /// Number of needed local stack variables, not including this, args etc
    pub number_of_vars: usize,
    pub meta: FunctionMeta,
}

/// Stores metadata about an OpFunction.
/// Currently, that's only the debugging information.
/// Later, other fields may be added, like hits or
/// caches for optimization
pub struct FunctionMeta {
    pub(crate) line_map: Vec<usize>,    // opcode to line
    pub(crate) column_map: Vec<usize>,  // Opcode to column in line
    pub(crate) code_source: CodeSource, // Source code file
}

/// Code source that can be displayed.
pub enum CodeSource {
    String(Rc<String>),
    // Later: File
}

/// Belongs to a JS Object. Can be executed but is not being executed right now.
pub struct FunctionInstance {
    pub code: Rc<OpFunction>,
    pub heap_vars: Rc<Vec<JsVar>>,
}

/// A function being executed right now.
/// Sits on the stack
pub struct FunctionExecution {
    pub op_pointer: usize,
    /// Jump here if an exception occurs.
    /// 0 if not in try catch (since 0 is not valid for catch anyways)
    pub catch_pointer: usize,
    pub instance: Rc<FunctionInstance>,
}
