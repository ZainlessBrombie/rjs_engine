use crate::js::data::execution_v2::constants::{BEGIN_VARS, JUMP_FLAG_LOCATION};
use crate::js::data::execution_v2::function::{
    CodeSource, FunctionInstance, FunctionMeta, OpFunction,
};
use crate::js::data::execution_v2::opcode::{Op, OpCode, Target};
use crate::js::data::intermediate::Action;
use crate::js::data::js_types::{Identity, JSCallable, JsValue};
use crate::js::data::util::JsObjectBuilder;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub fn build_function(action: Action) -> JsValue {
    let mut repo = StackVarRepo::new(Default::default());
    let ops = build_opcode_parts(action, Target::BlackHole, &mut repo, 0);
    return JsObjectBuilder::new(None)
        .with_callable(JSCallable::Js {
            content: Rc::new("".to_string()),
            creator: Rc::new(FunctionInstance {
                code: Rc::new(OpFunction {
                    instructions: ops,
                    number_of_vars: repo.cur,
                    meta: FunctionMeta {
                        line_map: vec![],
                        column_map: vec![],
                        code_source: CodeSource::String(Rc::new("".into_string())),
                    },
                }),
                heap_vars: Rc::new(vec![]),
            }),
        })
        .build();
}

struct StackVarRepo {
    cur: usize,                      // Initialized to be after the captured heap vars
    by_id: HashMap<Identity, usize>, // Offsets from "base" - not var region
}

// TODO naming
impl StackVarRepo {
    fn new(captures: HashMap<Identity, usize>) -> StackVarRepo {
        StackVarRepo {
            cur: BEGIN_VARS + captures.len(),
            by_id: captures,
        }
    }

    fn allocate(&mut self) -> usize {
        self.cur += 1;
        return self.cur - 1;
    }

    fn get(&self, id: &Identity) -> usize {
        *self.by_id.get(&id).expect("Foreign/Unknown variable id")
    }
}

fn build_opcode_parts(
    action: Action,
    target: Target,
    vars: &mut StackVarRepo,
    offset: usize,
) -> Vec<Op> {
    match action {
        Action::LoadGlobal(global) => {
            return vec![Op {
                target,
                code: OpCode::Transfer {
                    from: Target::Global(global),
                },
            }];
        }
        Action::VariableDeclare(decl) => {
            let into = vars.allocate();
            let mut ret = build_opcode_parts(
                decl.assign.right_side.action,
                Target::Stack(into),
                vars,
                offset,
            );
            ret.push(Op {
                target,
                code: OpCode::Transfer {
                    from: Target::Stack(into),
                },
            });
            return ret;
        }
        Action::VariableAssign(assign) => {
            let into = vars.get(&assign.var);
            let mut ret =
                build_opcode_parts(assign.right_side.action, Target::Stack(into), vars, offset);
            ret.push(Op {
                target,
                code: OpCode::Transfer {
                    from: Target::Stack(into),
                },
            })
        }
        Action::Literal(lit) => {
            return vec![Op {
                target,
                code: OpCode::Static { value: lit },
            }];
        }
        Action::ReadVar(_) => {}
        Action::Call(_) => {}
        Action::Throw(_) => {}
        Action::Return(_) => {}
        Action::Break(_) => {}
        Action::Labeled(_) => {}
        Action::ReadProp(_) => {}
        Action::IfElse(_) => {}
        Action::While(wh) => {
            let mut ret = Vec::new();
            ret.append(&mut build_opcode_parts(
                wh.condition.action,
                Target::Stack(JUMP_FLAG_LOCATION),
                vars,
                offset,
            ));
            let mut body = build_opcode_parts(
                wh.body.action,
                Target::BlackHole,
                vars,
                offset + ret.len() + 1,
            );
            ret.push(Op {
                target: Target::BlackHole,
                code: OpCode::ConditionalJump {
                    to: offset + ret.len() + body.len() + 1,
                },
            });
            ret.append(&mut body);
            return ret;
        }
        Action::For(_) => {}
        Action::Nop => {
            return vec![];
        }
        Action::BoolAnd(_) => {}
        Action::BoolOr(_) => {}
        Action::BoolNot(_) => {}
        Action::FuzzyCompare(_) => {}
        Action::StrictCompare(comp) => {
            let left = vars.allocate();
            let right = vars.allocate();
            let mut ret = build_opcode_parts(comp.left.action, Target::Stack(left), vars, offset);
            ret.append(&mut build_opcode_parts(
                comp.right.action,
                Target::Stack(right),
                vars,
                offset + ret.len(),
            ));
            ret.push(Op {
                target,
                code: OpCode::StrictCompare {
                    left: Target::Stack(left),
                    right: Target::Stack(right),
                },
            });
            return ret;
        }
        Action::TypeOf(_) => {}
        Action::Await(_) => {}
        Action::Add(add) => {
            let left = vars.allocate();
            let right = vars.allocate();
            let mut ret = build_opcode_parts(add.left.action, Target::Stack(left), vars, offset);
            ret.append(&mut build_opcode_parts(
                add.right.action,
                Target::Stack(right),
                vars,
                offset + ret.len(),
            ));
            ret.push(Op {
                target,
                code: OpCode::Add {
                    left: Target::Stack(left),
                    right: Target::Stack(right),
                },
            });
            return ret;
        }
        Action::Arithmetic2(arith) => {
            let left = vars.allocate();
            let right = vars.allocate();
            let mut ret = build_opcode_parts(
                arith.left_right.left.action,
                Target::Stack(left),
                vars,
                offset,
            );
            ret.append(&mut build_opcode_parts(
                arith.left_right.right.action,
                Target::Stack(right),
                vars,
                offset + ret.len(),
            ));
            ret.push(Op {
                target,
                code: OpCode::Arithmetic2 {
                    left: Target::Stack(left),
                    right: Target::Stack(right),
                    variant: arith.variant,
                },
            });
            return ret;
        }
        Action::NewObject(_) => {}
        Action::InstantiateFunction(f) => {
            let mut captures = HashMap::new();

            for (i, id) in f.captures.iter().enumerate() {
                captures.insert(id.clone(), i);
            }

            let mut repo = StackVarRepo::new(captures);
            let mut instructions = build_opcode_parts(
                f.content.action,
                Target::BlackHole,
                &mut repo,
                BEGIN_VARS + f.captures.len(),
            );
            return vec![Op {
                target,
                code: OpCode::CreateFunction {
                    // TODO move over heap vars
                    template: Rc::new(OpFunction {
                        instructions,
                        number_of_vars: repo.cur,
                        meta: FunctionMeta {
                            line_map: vec![],
                            column_map: vec![],
                            code_source: CodeSource::String(Rc::new("".to_string())),
                        },
                    }),
                    captures: f
                        .captures
                        .iter()
                        .map(|id| vars.get(id))
                        .map(|index| Target::Stack(index))
                        .collect(),
                },
            }];
        }
        Action::Block(block) => {
            let mut offset = offset;
            let mut result = Vec::new();
            for action in block.content {
                result.append(&mut build_opcode_parts(
                    action.action,
                    Target::BlackHole,
                    vars,
                    offset,
                ));
            }
            return result;
        }
    }
    unimplemented!()
}
