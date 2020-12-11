use crate::js::data::execution_v2::constants::THIS_LOCATION;
use crate::js::data::execution_v2::opcode::Arithmetic2Op;
use crate::js::data::execution_v2::opcode::OpCode::ReadProp;
use crate::js::data::intermediate::Action::VariableAssign;
use crate::js::data::intermediate::{
    empty_var_access, Action, Arithmetic2Action, AssignProp, CallFunction, CodeLoc, For, IfElse,
    InstantiateFunction, LeftRight, LocatedAction, Module, NewObject, ScopedBlock, VarAccess,
    VarAccessTrait, VarAssign, VarDecl, While,
};
use crate::js::data::js_types::{Identity, JsValue};
use crate::js::data::util::{s_pool, u_bool, u_string, u_true};
use crate::*;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::process::exit;
use std::rc::Rc;
use swc_ecma_ast::{
    BinaryOp, Decl, EmptyStmt, Expr, ExprOrSuper, Function, Lit, ModuleItem, Pat, Stmt,
    VarDeclOrExpr,
};

thread_local! {
    static THIS_ID: Identity = Identity::new();
    static ARGS_ID: Identity = Identity::new();
}
type RcVarAccess = Rc<RefCell<VarAccess>>;

pub fn parse_module(module: swc_ecma_ast::Module, vars: RcVarAccess) -> Action {
    let mut ret = Vec::new();
    for item in module.body {
        match item {
            ModuleItem::ModuleDecl(_) => {
                println!("Module Decl not supported");
            }
            ModuleItem::Stmt(stmt) => {
                ret.push(parse_stmt(stmt, vars.clone()));
            }
        }
    }
    return Action::Block(Box::from(ScopedBlock {
        content: ret,
        location: CodeLoc { line: 0, column: 0 },
    }));
}

fn parse_stmt(stmt: Stmt, access: RcVarAccess) -> LocatedAction {
    match stmt {
        Stmt::Block(block) => {
            let mut action = Vec::new();
            for stmt in block.stmts {
                let action1 = parse_stmt(stmt, access.clone());
                action.push(action1);
            }
            return LocatedAction {
                action: Action::Block(Box::new(ScopedBlock {
                    content: action,
                    location: CodeLoc { line: 0, column: 0 },
                })),
                location: CodeLoc { line: 0, column: 0 },
            };
        }
        Stmt::Empty(_) => {
            return LocatedAction {
                action: Action::Nop,
                location: CodeLoc { line: 0, column: 0 },
            }
        }
        Stmt::Debugger(_) => {}
        Stmt::With(_) => {}
        Stmt::Return(ret) => {
            if ret.arg.is_none() {
                return LocatedAction {
                    action: Action::Return(Box::from(Action::Literal(JsValue::Undefined))),
                    location: CodeLoc { line: 0, column: 0 },
                };
            }
        }
        Stmt::Labeled(_) => {}
        Stmt::Break(_) => {}
        Stmt::Continue(_) => {}
        Stmt::If(if_block) => {
            let test = *if_block.test;
            let cons = *if_block.cons;
            let alt = *if_block.alt.unwrap_or(Box::new(Stmt::Empty(EmptyStmt {
                span: Default::default(),
            })));
            return js_if_else!((parse_expr(test, access.clone())) {
                parse_stmt(cons, access.clone())
            } else {
                parse_stmt(alt, access)
            });
        }
        Stmt::Switch(_) => {}
        Stmt::Throw(thr) => {
            return LocatedAction {
                action: Action::Throw(Box::new(parse_expr(*thr.arg, access))),
                location: CodeLoc { line: 0, column: 0 },
            }
        }
        Stmt::Try(_) => {}
        Stmt::While(while_loop) => {
            let mut access: RcVarAccess = empty_var_access(Some(access), false);
            js_while!((parse_expr(*while_loop.test, access.clone())) {
                parse_stmt(*while_loop.body, access.clone())
            });
        }
        Stmt::DoWhile(_) => {}
        Stmt::For(for_loop) => {
            let mut access = empty_var_access(Some(access), false);
            return js_for!((
            if for_loop.init.is_some() {
                match for_loop.init.unwrap() {
                    VarDeclOrExpr::VarDecl(decl) => {
                        parse_var_decl(decl, access.clone())
                    }
                    VarDeclOrExpr::Expr(expr) => {
                        parse_expr(*expr, access.clone())
                    }
                }
            } else {
                js_block! {}
            }; {
                if for_loop.test.is_some() {
                    parse_expr(*for_loop.test.unwrap(), access.clone())
                } else {
                    js_block! {}
                }
            }; {
                if let Some(update) = for_loop.update {
                    parse_expr(*update, access.clone())
                } else {
                    js_block!()
                }
            }) {
                parse_stmt(*for_loop.body, access.clone())
            });
        }
        Stmt::ForIn(_) => {}
        Stmt::ForOf(_) => {}
        Stmt::Decl(decl) => match decl {
            Decl::Class(_) => {}
            Decl::Fn(f) => {
                let mut access = empty_var_access(Some(access), true);
                return parse_function(f.function, access);
            }
            Decl::Var(var_decl) => {
                return parse_var_decl(var_decl, access);
            }
            Decl::TsInterface(_) => {}
            Decl::TsTypeAlias(_) => {}
            Decl::TsEnum(_) => {}
            Decl::TsModule(_) => {}
        },
        Stmt::Expr(expr) => {
            return parse_expr(*expr.expr, access);
        }
    };
    unimplemented!()
}

fn parse_expr(expr: Expr, mut access: RcVarAccess) -> LocatedAction {
    match expr {
        Expr::This(_) => {
            return LocatedAction {
                action: Action::ReadVar(THIS_ID.with(|id| id.clone())),
                location: CodeLoc { line: 0, column: 0 },
            }
        }
        Expr::Array(arr_lit) => {}
        Expr::Object(_) => {}
        Expr::Fn(function) => {
            return parse_function(function.function, access);
        }
        Expr::Unary(_) => {}
        Expr::Update(_) => {}
        Expr::Bin(bin) => match bin.op {
            BinaryOp::EqEq => {
                return js_bin!(
                    (parse_expr(*bin.left, access.clone()))
                        == (parse_expr(*bin.right, access.clone()))
                );
            }
            BinaryOp::NotEq => {
                return js_bin!(
                    (parse_expr(*bin.left, access.clone()))
                        != (parse_expr(*bin.right, access.clone()))
                );
            }
            BinaryOp::EqEqEq => {
                return js_bin!(
                    (parse_expr(*bin.left, access.clone()))
                        === (parse_expr(*bin.right, access))
                );
            }
            BinaryOp::NotEqEq => {
                return js_bin!(
                    (parse_expr(*bin.left, access.clone()))
                        !== (parse_expr(*bin.right, access))
                );
            }
            BinaryOp::Lt => {}
            BinaryOp::LtEq => {}
            BinaryOp::Gt => {}
            BinaryOp::GtEq => {}
            BinaryOp::LShift => {}
            BinaryOp::RShift => {}
            BinaryOp::ZeroFillRShift => {}
            BinaryOp::Add => {
                return js_bin!(
                    (parse_expr(*bin.left, access.clone())) + (parse_expr(*bin.right, access))
                );
            }
            BinaryOp::Sub => {
                return js_bin!(
                    (parse_expr(*bin.left, access.clone())) - (parse_expr(*bin.right, access))
                );
            }
            BinaryOp::Mul => {
                return js_bin!(
                    (parse_expr(*bin.left, access.clone())) * (parse_expr(*bin.right, access))
                );
            }
            BinaryOp::Div => {
                return js_bin!(
                    (parse_expr(*bin.left, access.clone())) / (parse_expr(*bin.right, access))
                );
            }
            BinaryOp::Mod => {}
            BinaryOp::BitOr => {}
            BinaryOp::BitXor => {}
            BinaryOp::BitAnd => {}
            BinaryOp::LogicalOr => {}
            BinaryOp::LogicalAnd => {}
            BinaryOp::In => {}
            BinaryOp::InstanceOf => {}
            BinaryOp::Exp => {
                return js_bin!(
                    (parse_expr(*bin.left, access.clone())) * *(parse_expr(*bin.right, access))
                );
            }
            BinaryOp::NullishCoalescing => {}
        },
        Expr::Assign(_) => {}
        Expr::Member(member) => {
            let obj = match member.obj {
                ExprOrSuper::Super(_) => {
                    unimplemented!()
                }
                ExprOrSuper::Expr(expr) => expr,
            };
            return js_prop!((parse_expr(*obj, access.clone()))[parse_expr(*member.prop, access)]);
        }
        Expr::Cond(_) => {}
        Expr::Call(mut call) => {
            let callee = match call.callee {
                ExprOrSuper::Super(_) => {
                    unimplemented!()
                }
                ExprOrSuper::Expr(expr) => parse_expr(*expr, access.clone()),
            };

            let args_var = Identity::new();

            let mut args_assigns = Vec::new();

            args_assigns.push(LocatedAction {
                action: Action::AssignProp(Box::new(AssignProp {
                    to: LocatedAction {
                        action: Action::ReadVar(args_var.clone()),
                        location: CodeLoc { line: 0, column: 0 },
                    },
                    key: LocatedAction {
                        action: Action::Literal(u_string("length")),
                        location: CodeLoc { line: 0, column: 0 },
                    },
                    what: LocatedAction {
                        action: Action::Literal(JsValue::Number(call.args.len() as f64)),
                        location: CodeLoc { line: 0, column: 0 },
                    },
                })),
                location: CodeLoc { line: 0, column: 0 },
            });

            for (i, expr) in call.args.drain(..).enumerate() {
                args_assigns.push(LocatedAction {
                    action: Action::AssignProp(Box::new(AssignProp {
                        to: LocatedAction {
                            action: Action::ReadVar(args_var.clone()),
                            location: CodeLoc { line: 0, column: 0 },
                        },
                        key: LocatedAction {
                            action: Action::Literal(JsValue::Number(i as f64)),
                            location: CodeLoc { line: 0, column: 0 },
                        },
                        what: parse_expr(*expr.expr, access.clone()),
                    })),
                    location: CodeLoc { line: 0, column: 0 },
                });
            }

            return js_block! {
                {LocatedAction {
                    action: Action::VariableAssign(Box::new(VarAssign {
                        var: args_var.clone(),
                        right_side: Box::new(
                            LocatedAction {
                                action: Action::NewObject(Box::new(NewObject {
                                    is_array: true
                                })),
                                location: CodeLoc { line: 0, column: 0 }
                            }
                        )
                    })),
                    location: CodeLoc {
                        line: 0,
                        column: 0
                    }
                }}
                {LocatedAction {
                    action: Action::Block(Box::new(ScopedBlock {
                        location: CodeLoc {
                            line: 0,
                            column: 0
                        },
                        content: args_assigns
                    })),
                    location: CodeLoc {
                        line: 0,
                        column: 0
                    }
                }}

                {js_call!((callee)(args: LocatedAction {
                    action: Action::ReadVar(args_var),
                    location: CodeLoc {
                        line: 0,
                        column: 0
                    }
                }))}
            };
        }
        Expr::New(_) => {}
        Expr::Seq(_) => {}
        Expr::Ident(ident) => {
            return LocatedAction {
                action: Action::ReadVar(access.get_or_global(&Rc::new(ident.sym.to_string()))),
                location: CodeLoc { line: 0, column: 0 },
            }
        }
        Expr::Lit(literal) => match literal {
            Lit::Str(s) => {
                return LocatedAction {
                    action: Action::Literal(JsValue::String(Rc::new(s.value.to_string()))),
                    location: CodeLoc { line: 0, column: 0 },
                }
            }
            Lit::Bool(b) => {
                return LocatedAction {
                    action: Action::Literal(JsValue::Boolean(b.value)),
                    location: CodeLoc { line: 0, column: 0 },
                }
            }
            Lit::Null(_) => {
                return LocatedAction {
                    action: Action::Literal(JsValue::Null),
                    location: CodeLoc { line: 0, column: 0 },
                }
            }
            Lit::Num(n) => {
                return LocatedAction {
                    action: Action::Literal(JsValue::Number(n.value)),
                    location: CodeLoc { line: 0, column: 0 },
                }
            }
            Lit::BigInt(_) => {}
            Lit::Regex(_) => {}
            Lit::JSXText(_) => {}
        },
        Expr::Tpl(_) => {}
        Expr::TaggedTpl(_) => {}
        Expr::Arrow(_) => {}
        Expr::Class(_) => {}
        Expr::Yield(_) => {}
        Expr::MetaProp(_) => {}
        Expr::Await(_) => {}
        Expr::Paren(_) => {}
        Expr::JSXMember(_) => {}
        Expr::JSXNamespacedName(_) => {}
        Expr::JSXEmpty(_) => {}
        Expr::JSXElement(_) => {}
        Expr::JSXFragment(_) => {}
        Expr::TsTypeAssertion(_) => {}
        Expr::TsConstAssertion(_) => {}
        Expr::TsNonNull(_) => {}
        Expr::TsTypeCast(_) => {}
        Expr::TsAs(_) => {}
        Expr::PrivateName(_) => {}
        Expr::OptChain(_) => {}
        Expr::Invalid(_) => {}
    }
    unimplemented!()
}

pub fn this() -> Identity {
    return THIS_ID.with(|a| a.clone());
}

pub fn args() -> Identity {
    return ARGS_ID.with(|a| a.clone());
}

fn parse_function(f: Function, access: RcVarAccess) -> LocatedAction {
    let mut actions = Vec::new();

    for (i, param) in f.params.iter().enumerate() {
        match &param.pat {
            Pat::Ident(ident) => {
                let v = access.clone().local_declare(Rc::new(ident.sym.to_string()));
                actions.push(js_var!(
                    (v) = js_prop!((js_var!(args()))[js_static!(js_primitive!(i))])
                ));
            }
            _ => unimplemented!(),
        }
    }

    if let Some(body) = f.body {
        for stmt in body.stmts {
            actions.push(parse_stmt(stmt, access.clone()));
        }
    }

    return LocatedAction {
        action: Action::InstantiateFunction(Box::new(InstantiateFunction {
            name: None,
            content: LocatedAction {
                action: Action::Block(Box::new(ScopedBlock {
                    content: actions,
                    location: CodeLoc { line: 0, column: 0 },
                })),
                location: CodeLoc { line: 0, column: 0 },
            },
            captures: access.known_heap_vars(),
        })),
        location: CodeLoc { line: 0, column: 0 },
    };
}

fn parse_var_decl(mut decl: swc_ecma_ast::VarDecl, mut access: RcVarAccess) -> LocatedAction {
    let one = decl.decls.drain(..).next().unwrap();
    let name = match &one.name {
        Pat::Ident(ident) => ident.sym.to_string(),
        _ => unimplemented!(),
    };
    let var = access.local_declare(Rc::new(name));
    return js_var!(
        (var) = one
            .init
            .map(|init| parse_expr(*init, access))
            .unwrap_or(LocatedAction {
                action: Action::Literal(JsValue::Undefined),
                location: CodeLoc { column: 0, line: 0 }
            })
    );
}
