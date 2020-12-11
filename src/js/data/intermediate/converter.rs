use crate::js::data::execution_v2::constants::{BEGIN_VARS, JUMP_FLAG_LOCATION};
use crate::js::data::execution_v2::function::{
    CodeSource, FunctionInstance, FunctionMeta, OpFunction,
};
use crate::js::data::execution_v2::opcode::{Op, OpCode, Target};
use crate::js::data::execution_v2::var::JsVar;
use crate::js::data::intermediate::Action;
use crate::js::data::js_execution::native_from;
use crate::js::data::js_types::{Identity, JSCallable, JsValue};
use crate::js::data::util::{s_pool, JsObjectBuilder};
use safe_gc::{Gc, GcCell};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub fn build_function(action: Action, console: Identity) -> JsValue {
    let console_obj = JsObjectBuilder::new(None)
        .with_prop(
            s_pool("log"),
            JsObjectBuilder::new(None)
                .with_callable(JSCallable::Native {
                    op: native_from(|_this, args| {
                        return {
                            println!("Print: {}", args.to_system_string());
                            Ok(JsValue::Undefined)
                        };
                    }),
                })
                .build(),
        )
        .build();
    let console_var = JsVar {
        name: Rc::new("console".to_string()),
        value: Gc::new(GcCell::new(console_obj)),
    };
    let mut repo = StackVarRepo::new(Default::default());
    repo.by_id.insert(console, BEGIN_VARS);
    let mut ops = build_opcode_parts(action, Target::BlackHole, &mut repo, 0);
    ops.push(Op {
        target: Target::BlackHole,
        code: OpCode::Return {
            what: Target::BlackHole,
        },
    });
    return JsObjectBuilder::new(None)
        .with_callable(JSCallable::Js {
            content: Rc::new("".to_string()),
            creator: Rc::new(FunctionInstance {
                code: Rc::new(OpFunction {
                    instructions: ops,
                    number_of_vars: repo.cur + 10,
                    meta: FunctionMeta {
                        line_map: vec![],
                        column_map: vec![],
                        code_source: CodeSource::String(Rc::new("".into())),
                    },
                }),
                heap_vars: Rc::new(vec![console_var.clone(), console_var.clone(), console_var]),
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

    fn get(&mut self, id: &Identity) -> usize {
        if let Some(found) = self.by_id.get(&id) {
            return *found;
        } else {
            let ret = self.allocate();
            self.by_id.insert(id.clone(), ret);
            return ret;
        }
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
            });
            return ret;
        }
        Action::Literal(lit) => {
            return vec![Op {
                target,
                code: OpCode::Static { value: lit },
            }];
        }
        Action::ReadVar(read_var) => {
            return vec![Op {
                target,
                code: OpCode::Transfer {
                    from: Target::Stack(vars.get(&read_var)),
                },
            }];
        }
        Action::Call(call) => {
            // TODO does not handle "this" right now
            let callee_var = vars.allocate();
            let args_var = vars.allocate();

            let mut result = Vec::new();

            result.append(&mut build_opcode_parts(
                call.member.action,
                Target::Stack(callee_var),
                vars,
                offset,
            ));
            result.append(&mut build_opcode_parts(
                call.args.action,
                Target::Stack(args_var),
                vars,
                offset + result.len(),
            ));
            result.push(Op {
                target,
                code: OpCode::Call {
                    args: Target::Stack(args_var),
                    what: Target::Stack(callee_var),
                    this: Target::BlackHole,
                },
            });
            return result;
        }
        Action::Throw(_) => {}
        Action::Return(_) => {}
        Action::Break(_) => {}
        Action::Labeled(_) => {}
        Action::ReadProp(read_prop) => {
            let mut result = Vec::new();
            let from_var = vars.allocate();
            let to_var = vars.allocate();

            result.append(&mut build_opcode_parts(
                read_prop.from.action,
                Target::Stack(from_var),
                vars,
                offset,
            ));
            result.append(&mut build_opcode_parts(
                read_prop.key.action,
                Target::Stack(to_var),
                vars,
                offset + result.len(),
            ));

            result.push(Op {
                target,
                code: OpCode::ReadProp {
                    from: Target::Stack(from_var),
                    key: Target::Stack(to_var),
                },
            });
            return result;
        }
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
        Action::NewObject(new_obj) => {
            return vec![Op {
                target,
                code: OpCode::NewObject {
                    is_array: new_obj.is_array,
                },
            }];
        }
        Action::InstantiateFunction(f) => {
            let mut captures = HashMap::new();

            for (i, id) in f.captures.iter().enumerate() {
                captures.insert(id.clone(), i + BEGIN_VARS);
            }

            let mut repo = StackVarRepo::new(captures);
            let mut instructions = build_opcode_parts(
                f.content.action,
                Target::BlackHole,
                &mut repo,
                BEGIN_VARS + f.captures.len(),
            );
            instructions.push(Op {
                target: Target::BlackHole,
                code: OpCode::Return {
                    what: Target::BlackHole,
                },
            });
            // TODO return here
            return vec![Op {
                target,
                code: OpCode::CreateFunction {
                    template: Rc::new(OpFunction {
                        instructions,
                        number_of_vars: repo.cur + f.captures.len() + 10,
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
        Action::Block(mut block) => {
            let mut offset = offset;
            let mut result = Vec::new();
            let original_size = block.content.len();
            for (i, action) in block.content.drain(..).enumerate() {
                result.append(&mut build_opcode_parts(
                    action.action,
                    if (original_size - 1) == i {
                        target.clone()
                    } else {
                        Target::BlackHole
                    },
                    vars,
                    offset,
                ));
            }
            return result;
        }
        Action::AssignProp(assign) => {
            let mut result = Vec::new();
            let to_var = vars.allocate();
            let key_var = vars.allocate();
            let what_var = vars.allocate();
            result.append(&mut build_opcode_parts(
                assign.to.action,
                Target::Stack(to_var.clone()),
                vars,
                offset,
            ));

            result.append(&mut build_opcode_parts(
                assign.key.action,
                Target::Stack(key_var.clone()),
                vars,
                offset + result.len(),
            ));

            result.append(&mut build_opcode_parts(
                assign.what.action,
                Target::Stack(what_var.clone()),
                vars,
                offset + result.len(),
            ));

            result.push(Op {
                target,
                code: OpCode::AssignProp {
                    of: Target::Stack(to_var),
                    key: Target::Stack(key_var),
                    value: Target::Stack(what_var),
                },
            });
            return result;
        }
    }
    unimplemented!()
}
