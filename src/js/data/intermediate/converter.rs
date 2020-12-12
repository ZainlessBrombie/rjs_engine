use crate::js::data::execution_v2::constants::{BEGIN_VARS, JUMP_FLAG_LOCATION};
use crate::js::data::execution_v2::function::{
    CodeSource, FunctionInstance, FunctionMeta, OpFunction,
};
use crate::js::data::execution_v2::opcode::{Op, OpCode, Target};
use crate::js::data::execution_v2::var::JsVar;
use crate::js::data::intermediate::{Action, CodeLoc, LocatedAction};
use crate::js::data::js_execution::native_from;
use crate::js::data::js_types::{Identity, JSCallable, JsValue};
use crate::js::data::util::{s_pool, JsObjectBuilder};
use safe_gc::{Gc, GcCell};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub fn build_function(action: Action, console: Identity, source: Rc<String>) -> JsValue {
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
    let mut map = HashMap::new();
    map.insert(console, BEGIN_VARS);
    let mut repo = StackVarRepo::new(map);

    let mut ops = build_opcode_parts(
        LocatedAction {
            action,
            location: CodeLoc { line: 1, column: 1 },
        },
        Target::BlackHole,
        &mut repo,
        0,
        source.clone(),
    );
    ops.push(Op {
        target: Target::BlackHole,
        code: OpCode::Return {
            what: Target::BlackHole,
        },
        loc: CodeLoc { line: 1, column: 1 },
    });
    return JsObjectBuilder::new(None)
        .with_callable(JSCallable::Js {
            content: Rc::new("".to_string()),
            creator: Rc::new(FunctionInstance {
                code: Rc::new(OpFunction {
                    instructions: ops,
                    number_of_vars: repo.cur + 10,
                    meta: FunctionMeta {
                        code_source: CodeSource::String(source.clone()),
                    },
                }),
                heap_vars: Rc::new(vec![console_var.clone()]),
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
    action: LocatedAction,
    target: Target,
    vars: &mut StackVarRepo,
    offset: usize,
    source: Rc<String>,
) -> Vec<Op> {
    match action.action {
        Action::LoadGlobal(global) => {
            return vec![Op {
                target,
                code: OpCode::Transfer {
                    from: Target::Global(global),
                },
                loc: action.location,
            }];
        }
        Action::VariableDeclare(decl) => {
            let into = vars.allocate();
            let mut ret = build_opcode_parts(
                *decl.assign.right_side,
                Target::Stack(into),
                vars,
                offset,
                source,
            );
            ret.push(Op {
                target,
                code: OpCode::Transfer {
                    from: Target::Stack(into),
                },
                loc: action.location,
            });
            return ret;
        }
        Action::VariableAssign(assign) => {
            let into = vars.get(&assign.var);
            let mut ret = build_opcode_parts(
                *assign.right_side,
                Target::Stack(into),
                vars,
                offset,
                source,
            );
            ret.push(Op {
                target,
                code: OpCode::Transfer {
                    from: Target::Stack(into),
                },
                loc: action.location,
            });
            return ret;
        }
        Action::Literal(lit) => {
            return vec![Op {
                target,
                code: OpCode::Static { value: lit },
                loc: action.location,
            }];
        }
        Action::ReadVar(read_var) => {
            return vec![Op {
                target,
                code: OpCode::Transfer {
                    from: Target::Stack(vars.get(&read_var)),
                },
                loc: action.location,
            }];
        }
        Action::Call(call) => {
            // TODO does not handle "this" right now
            let callee_var = vars.allocate();
            let args_var = vars.allocate();

            let mut result = Vec::new();

            result.append(&mut build_opcode_parts(
                call.member,
                Target::Stack(callee_var),
                vars,
                offset,
                source.clone(),
            ));
            result.append(&mut build_opcode_parts(
                call.args,
                Target::Stack(args_var),
                vars,
                offset + result.len(),
                source,
            ));
            result.push(Op {
                target,
                code: OpCode::Call {
                    args: Target::Stack(args_var),
                    what: Target::Stack(callee_var),
                    this: Target::BlackHole,
                },
                loc: action.location,
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
                read_prop.from,
                Target::Stack(from_var),
                vars,
                offset,
                source.clone(),
            ));
            result.append(&mut build_opcode_parts(
                read_prop.key,
                Target::Stack(to_var),
                vars,
                offset + result.len(),
                source,
            ));

            result.push(Op {
                target,
                code: OpCode::ReadProp {
                    from: Target::Stack(from_var),
                    key: Target::Stack(to_var),
                },
                loc: action.location,
            });
            return result;
        }
        Action::IfElse(_) => {}
        Action::While(wh) => {
            let mut ret = Vec::new();
            let mut body =
                build_opcode_parts(wh.body, Target::BlackHole, vars, offset + 1, source.clone());
            ret.push(Op {
                target,
                code: OpCode::Jump {
                    to: offset + body.len() + 1,
                },
                loc: action.location.clone(),
            });
            ret.append(&mut body);

            ret.append(&mut build_opcode_parts(
                wh.condition,
                Target::Stack(JUMP_FLAG_LOCATION),
                vars,
                offset + ret.len(),
                source.clone(),
            ));

            ret.push(Op {
                target: Target::BlackHole,
                code: OpCode::ConditionalJump {
                    to: offset + 1, // body pos 0
                },
                loc: action.location,
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
        Action::BoolNot(not) => {
            let mut ret = build_opcode_parts(*not, target.clone(), vars, offset, source.clone());
            ret.push(Op {
                target: target.clone(),
                code: OpCode::Not { source: target },
                loc: action.location,
            });
            return ret;
        }
        Action::FuzzyCompare(_) => {}
        Action::StrictCompare(comp) => {
            let left = vars.allocate();
            let right = vars.allocate();
            let mut ret =
                build_opcode_parts(comp.left, Target::Stack(left), vars, offset, source.clone());
            ret.append(&mut build_opcode_parts(
                comp.right,
                Target::Stack(right),
                vars,
                offset + ret.len(),
                source,
            ));
            ret.push(Op {
                target,
                code: OpCode::StrictCompare {
                    left: Target::Stack(left),
                    right: Target::Stack(right),
                },
                loc: action.location,
            });
            return ret;
        }
        Action::TypeOf(_) => {}
        Action::Await(_) => {}
        Action::Add(add) => {
            let left = vars.allocate();
            let right = vars.allocate();
            let mut ret =
                build_opcode_parts(add.left, Target::Stack(left), vars, offset, source.clone());
            ret.append(&mut build_opcode_parts(
                add.right,
                Target::Stack(right),
                vars,
                offset + ret.len(),
                source,
            ));
            ret.push(Op {
                target,
                code: OpCode::Add {
                    left: Target::Stack(left),
                    right: Target::Stack(right),
                },
                loc: action.location,
            });
            return ret;
        }
        Action::Arithmetic2(arith) => {
            let left = vars.allocate();
            let right = vars.allocate();
            let mut ret = build_opcode_parts(
                arith.left_right.left,
                Target::Stack(left),
                vars,
                offset,
                source.clone(),
            );
            ret.append(&mut build_opcode_parts(
                arith.left_right.right,
                Target::Stack(right),
                vars,
                offset + ret.len(),
                source.clone(),
            ));
            ret.push(Op {
                target,
                code: OpCode::Arithmetic2 {
                    left: Target::Stack(left),
                    right: Target::Stack(right),
                    variant: arith.variant,
                },
                loc: action.location,
            });
            return ret;
        }
        Action::NewObject(new_obj) => {
            return vec![Op {
                target,
                code: OpCode::NewObject {
                    is_array: new_obj.is_array,
                },
                loc: action.location,
            }];
        }
        Action::InstantiateFunction(f) => {
            let mut captures = HashMap::new();

            for (i, id) in f.captures.iter().enumerate() {
                captures.insert(id.clone(), i + BEGIN_VARS);
            }

            let mut repo = StackVarRepo::new(captures);
            let mut instructions = build_opcode_parts(
                f.content,
                Target::BlackHole,
                &mut repo,
                BEGIN_VARS + f.captures.len(),
                source.clone(),
            );
            instructions.push(Op {
                target: Target::BlackHole,
                code: OpCode::Return {
                    what: Target::BlackHole,
                },
                loc: action.location.clone(),
            });
            // TODO return here
            return vec![Op {
                target,
                code: OpCode::CreateFunction {
                    template: Rc::new(OpFunction {
                        instructions,
                        number_of_vars: repo.cur + f.captures.len() + 10,
                        meta: FunctionMeta {
                            code_source: CodeSource::String(source),
                        },
                    }),
                    captures: f
                        .captures
                        .iter()
                        .map(|id| vars.get(id))
                        .map(|index| Target::Stack(index))
                        .collect(),
                },
                loc: action.location.clone(),
            }];
        }
        Action::Block(mut block) => {
            let mut offset = offset;
            let mut result = Vec::new();
            let original_size = block.content.len();
            for (i, action) in block.content.drain(..).enumerate() {
                result.append(&mut build_opcode_parts(
                    action,
                    if (original_size - 1) == i {
                        target.clone()
                    } else {
                        Target::BlackHole
                    },
                    vars,
                    offset + result.len(),
                    source.clone(),
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
                assign.to,
                Target::Stack(to_var.clone()),
                vars,
                offset,
                source.clone(),
            ));

            result.append(&mut build_opcode_parts(
                assign.key,
                Target::Stack(key_var.clone()),
                vars,
                offset + result.len(),
                source.clone(),
            ));

            result.append(&mut build_opcode_parts(
                assign.what,
                Target::Stack(what_var.clone()),
                vars,
                offset + result.len(),
                source,
            ));

            result.push(Op {
                target,
                code: OpCode::AssignProp {
                    of: Target::Stack(to_var),
                    key: Target::Stack(key_var),
                    value: Target::Stack(what_var),
                },
                loc: action.location,
            });
            return result;
        }
    }
    unimplemented!()
}
