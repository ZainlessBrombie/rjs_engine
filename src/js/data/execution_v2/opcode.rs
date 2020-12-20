use crate::js::data::execution_v2::function::OpFunction;
use crate::js::data::intermediate::CodeLoc;
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
    /// Literal / Static JS value for inlining.
    Static(JsValue),
    /// Don't put value anywhere. Undefined read, nop write.
    BlackHole,
}

impl Target {
    #[inline]
    pub fn write_ineffective(&self) -> bool {
        match self {
            Target::BlackHole => true,
            Target::Static(_) => true,
            _ => false,
        }
    }
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

impl Op {
    pub fn op_stats(&mut self) -> OpStats {
        let mut sources = Vec::new();
        let targets = vec![&mut self.target];
        let mut depends_unknown = false;
        let mut affects_unknown = false;
        match &mut self.code {
            OpCode::Jump { .. } => {}
            OpCode::ConditionalJump { .. } => {
                // sources.push(&mut Target::Stack(JUMP_FLAG_LOCATION));
                depends_unknown = true; // Jump source cannot be changed right now TODO
            }
            OpCode::NewObject { .. } => {}
            OpCode::Not { source } => {
                sources.push(source);
            }
            OpCode::Assign { source } => {
                sources.push(source);
            }
            OpCode::Static { .. } => {}
            OpCode::CreateFunction {
                captures,
                template: _,
            } => {
                sources.append(&mut captures.iter_mut().collect());
            }
            OpCode::Call { what, args, this } => {
                sources.push(what);
                sources.push(args);
                sources.push(this);
                affects_unknown = true;
            }
            OpCode::Throw { what } => {
                sources.push(what);
                affects_unknown = true;
            }
            OpCode::Return { what } => {
                sources.push(what);
                affects_unknown = true;
            }
            OpCode::ReadProp { key, from } => {
                sources.push(key);
                sources.push(from);
                affects_unknown = true; // May throw
            }
            OpCode::Nop { .. } => {}
            OpCode::Transfer { from } => {
                sources.push(from);
            }
            OpCode::FuzzyCompare { right, left } => {
                sources.push(left);
                sources.push(right);
            }
            OpCode::StrictCompare { left, right } => {
                sources.push(left);
                sources.push(right);
            }
            OpCode::TypeOf { what } => {
                sources.push(what);
            }
            OpCode::Await { what } => {
                sources.push(what);
            }
            OpCode::AssignProp { key, value, of } => {
                sources.push(key);
                sources.push(value);
                sources.push(of);
                affects_unknown = true; // Assigning a prop is a side effect, we are not a pure function
                depends_unknown = true; // May throw
            }
            OpCode::Add { right, left } => {
                sources.push(left);
                sources.push(right);
            }
            OpCode::Arithmetic2 {
                left,
                right,
                variant: _,
            } => {
                sources.push(left);
                sources.push(right);
            }
        };

        let mut stack_sources = vec![];
        for source in sources {
            match source {
                Target::Stack(n) => {
                    stack_sources.push(n);
                }
                Target::Global(_) => {
                    depends_unknown = true;
                } // TODO what about heap variables that have a ref on the stack?
                Target::BlackHole => {}
                Target::Static(_) => {}
            }
        }
        let mut stack_targets = vec![];
        for target in targets {
            match target {
                Target::Stack(n) => {
                    stack_targets.push(n);
                }
                Target::Global(_) => {
                    affects_unknown = true;
                } // TODO what about heap variables that have a ref on the stack?
                Target::BlackHole => {}
                Target::Static(_) => {}
            }
        }

        return OpStats {
            side_effects: SideEffects {
                side_source: depends_unknown,
                side_target: affects_unknown,
            },
            sources: stack_sources,
            targets: stack_targets,
        };
    }

    // TODO find better name
    /// Move the jump target by x.
    pub fn move_op_target(&mut self, offset: i32) {
        match &mut self.code {
            OpCode::Jump { to } => {
                *to = (*to as i32 + offset) as usize;
            }
            OpCode::ConditionalJump { to } => {
                *to = (*to as i32 + offset) as usize;
            }
            OpCode::NewObject { .. } => {}
            OpCode::Not { .. } => {}
            OpCode::Assign { .. } => {}
            OpCode::Static { .. } => {}
            OpCode::CreateFunction { .. } => {}
            OpCode::Call { .. } => {}
            OpCode::Throw { .. } => {}
            OpCode::Return { .. } => {}
            OpCode::ReadProp { .. } => {}
            OpCode::Nop { .. } => {}
            OpCode::Transfer { .. } => {}
            OpCode::FuzzyCompare { .. } => {}
            OpCode::StrictCompare { .. } => {}
            OpCode::TypeOf { .. } => {}
            OpCode::Await { .. } => {}
            OpCode::AssignProp { .. } => {}
            OpCode::Add { .. } => {}
            OpCode::Arithmetic2 { .. } => {}
        }
    }
}

pub struct OpStats<'a> {
    /// Don't optimize if it does
    side_effects: SideEffects,
    sources: Vec<&'a mut usize>,
    targets: Vec<&'a mut usize>,
}

impl<'a> OpStats<'a> {
    pub fn is_pure(&self) -> bool {
        return !self.side_effects.side_source && !self.side_effects.side_target;
    }
}

pub struct SideEffects {
    side_source: bool,
    side_target: bool,
}
