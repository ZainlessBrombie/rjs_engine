use crate::js::data::execution_v2::opcode::Arithmetic2Op;
use crate::js::data::intermediate::{
    empty_var_access, Action, Arithmetic2Action, AssignProp, CallFunction, CodeLoc, For, IfElse,
    InstantiateFunction, LeftRight, LocatedAction, Module, NewObject, ScopedBlock, VarAccess,
    VarAccessTrait, VarAssign, VarDecl, While,
};
use crate::js::data::js_types::{Identity, JsValue};
use crate::js::data::util::col_line_map::ColLineMap;
use crate::js::data::util::{s_pool, u_bool, u_string, u_true};
use crate::*;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::process::exit;
use std::rc::Rc;
use swc_common::Spanned;
use swc_ecma_ast::{
    BinaryOp, Decl, EmptyStmt, Expr, ExprOrSuper, Function, Lit, ModuleItem, Pat, PatOrExpr, Stmt,
    VarDeclOrExpr,
};
use swc_ecma_parser::token::Token::AssignOp;

thread_local! {
    static THIS_ID: Identity = Identity::new();
    static ARGS_ID: Identity = Identity::new();
}
type RcVarAccess = Rc<RefCell<VarAccess>>;

pub fn parse_module(module: swc_ecma_ast::Module, vars: RcVarAccess, source: Rc<String>) -> Action {
    let line_map = Rc::new(ColLineMap::new(source.as_str()));
    let mut ret = Vec::new();
    for item in module.body {
        match item {
            ModuleItem::ModuleDecl(_) => {
                println!("Module Decl not supported");
            }
            ModuleItem::Stmt(stmt) => {
                ret.push(parse_stmt(stmt, vars.clone(), line_map.clone()));
            }
        }
    }
    return Action::Block(Box::from(ScopedBlock {
        content: ret,
        location: CodeLoc { line: 1, column: 1 },
    }));
}

fn parse_stmt(stmt: Stmt, access: RcVarAccess, line_map: Rc<ColLineMap>) -> LocatedAction {
    match stmt {
        Stmt::Block(block) => {
            let lo = block.span.lo.0;
            let mut action = Vec::new();
            for stmt in block.stmts {
                let action1 = parse_stmt(stmt, access.clone(), line_map.clone());
                action.push(action1);
            }
            return LocatedAction {
                action: Action::Block(Box::new(ScopedBlock {
                    content: action,
                    location: line_map.loc_for(lo as usize),
                })),
                location: line_map.loc_for(lo as usize),
            };
        }
        Stmt::Empty(empty) => {
            return LocatedAction {
                action: Action::Nop,
                location: line_map.loc_for(empty.span.lo.0 as usize),
            }
        }
        Stmt::Debugger(_) => {}
        Stmt::With(_) => {}
        Stmt::Return(ret) => {
            if ret.arg.is_none() {
                return LocatedAction {
                    action: Action::Return(Box::from(Action::Literal(JsValue::Undefined))),
                    location: line_map.loc_for(ret.span.lo.0 as usize),
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
            return js_if_else!((parse_expr(test, access.clone(), line_map.clone())) {
                parse_stmt(cons, access.clone(), line_map.clone())
            } else {
                parse_stmt(alt, access, line_map.clone())
            } @ line_map.loc_for(if_block.span.lo.0 as usize));
        }
        Stmt::Switch(_) => {}
        Stmt::Throw(thr) => {
            return LocatedAction {
                action: Action::Throw(Box::new(parse_expr(*thr.arg, access, line_map.clone()))),
                location: line_map.loc_for(thr.span.lo.0 as usize),
            }
        }
        Stmt::Try(_) => {}
        Stmt::While(while_loop) => {
            let mut access: RcVarAccess = empty_var_access(Some(access), false);
            return js_while!((parse_expr(*while_loop.test, access.clone(), line_map.clone())) {
                parse_stmt(*while_loop.body, access.clone(), line_map.clone())
            } @ line_map.loc_for(while_loop.span.lo.0 as usize));
        }
        Stmt::DoWhile(_) => {}
        Stmt::For(for_loop) => {
            let mut access = empty_var_access(Some(access), false);
            return js_for!((
            if for_loop.init.is_some() {
                match for_loop.init.unwrap() {
                    VarDeclOrExpr::VarDecl(decl) => {
                        parse_var_decl(decl, access.clone(), line_map.clone())
                    }
                    VarDeclOrExpr::Expr(expr) => {
                        parse_expr(*expr, access.clone(), line_map.clone())
                    }
                }
            } else {
                js_block! {() @ line_map.loc_for(for_loop.span.lo.0 as usize)}
            }; {
                if for_loop.test.is_some() {
                    parse_expr(*for_loop.test.unwrap(), access.clone(), line_map.clone())
                } else {
                    js_block! {() @ line_map.loc_for(for_loop.span.lo.0 as usize)}
                }
            }; {
                if let Some(update) = for_loop.update {
                    parse_expr(*update, access.clone(), line_map.clone())
                } else {
                    js_block!(() @ line_map.loc_for(for_loop.span.lo.0 as usize))
                }
            }) {
                parse_stmt(*for_loop.body, access.clone(), line_map.clone())
            } @ line_map.loc_for(for_loop.span.lo.0 as usize));
        }
        Stmt::ForIn(_) => {}
        Stmt::ForOf(_) => {}
        Stmt::Decl(decl) => match decl {
            Decl::Class(_) => {}
            Decl::Fn(f) => {
                let mut access = empty_var_access(Some(access), true);
                return parse_function(f.function, access, line_map.clone());
            }
            Decl::Var(var_decl) => {
                return parse_var_decl(var_decl, access, line_map.clone());
            }
            Decl::TsInterface(_) => {}
            Decl::TsTypeAlias(_) => {}
            Decl::TsEnum(_) => {}
            Decl::TsModule(_) => {}
        },
        Stmt::Expr(expr) => {
            return parse_expr(*expr.expr, access, line_map.clone());
        }
    };
    unimplemented!()
}

fn parse_expr(expr: Expr, mut access: RcVarAccess, line_map: Rc<ColLineMap>) -> LocatedAction {
    match expr {
        Expr::This(this) => {
            return LocatedAction {
                action: Action::ReadVar(THIS_ID.with(|id| id.clone())),
                location: line_map.loc_for(this.span.lo.0 as usize),
            }
        }
        Expr::Array(arr_lit) => {}
        Expr::Object(_) => {}
        Expr::Fn(function) => {
            return parse_function(function.function, access, line_map.clone());
        }
        Expr::Unary(_) => {}
        Expr::Update(_) => {}
        Expr::Bin(bin) => match bin.op {
            BinaryOp::EqEq => {
                return js_bin!(
                    (parse_expr(*bin.left, access.clone(), line_map.clone()))
                        == (parse_expr(*bin.right, access.clone(), line_map.clone()))
                     @ line_map.loc_for(bin.span.lo.0 as usize)
                );
            }
            BinaryOp::NotEq => {
                return js_bin!(
                    (parse_expr(*bin.left, access.clone(), line_map.clone()))
                        != (parse_expr(*bin.right, access.clone(), line_map.clone()))
                    @ line_map.loc_for(bin.span.lo.0 as usize)
                );
            }
            BinaryOp::EqEqEq => {
                return js_bin!(
                    (parse_expr(*bin.left, access.clone(), line_map.clone()))
                        === (parse_expr(*bin.right, access, line_map.clone()))
                    @ line_map.loc_for(bin.span.lo.0 as usize)
                );
            }
            BinaryOp::NotEqEq => {
                return js_bin!(
                    (parse_expr(*bin.left, access.clone(), line_map.clone()))
                        !== (parse_expr(*bin.right, access, line_map.clone()))
                    @ line_map.loc_for(bin.span.lo.0 as usize)
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
                    (parse_expr(*bin.left, access.clone(), line_map.clone()))
                        + (parse_expr(*bin.right, access, line_map.clone()))
                    @ line_map.loc_for(bin.span.lo.0 as usize)
                );
            }
            BinaryOp::Sub => {
                return js_bin!(
                    (parse_expr(*bin.left, access.clone(), line_map.clone()))
                        - (parse_expr(*bin.right, access, line_map.clone()))
                    @ line_map.loc_for(bin.span.lo.0 as usize)
                );
            }
            BinaryOp::Mul => {
                return js_bin!(
                    (parse_expr(*bin.left, access.clone(), line_map.clone()))
                        * (parse_expr(*bin.right, access, line_map.clone()))
                    @ line_map.loc_for(bin.span.lo.0 as usize)
                );
            }
            BinaryOp::Div => {
                return js_bin!(
                    (parse_expr(*bin.left, access.clone(), line_map.clone()))
                        / (parse_expr(*bin.right, access, line_map.clone()))
                    @ line_map.loc_for(bin.span.lo.0 as usize)
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
                    (parse_expr(*bin.left, access.clone(), line_map.clone()))
                        * *(parse_expr(*bin.right, access, line_map.clone()))
                    @ line_map.loc_for(bin.span.lo.0 as usize)
                );
            }
            BinaryOp::NullishCoalescing => {}
        },
        Expr::Assign(assign) => {
            let name = match assign.left {
                PatOrExpr::Expr(_) => {
                    unimplemented!()
                }
                PatOrExpr::Pat(pat) => match *pat {
                    Pat::Ident(id) => id.sym.to_string(),
                    _ => {
                        unimplemented!()
                    }
                },
            };
            return js_var!((access.get_or_global(&Rc::new(name))) = (parse_expr(*assign.right, access, line_map.clone())) @ line_map.loc_for(assign.span.lo.0 as usize));
        }
        Expr::Member(member) => {
            let obj = match member.obj {
                ExprOrSuper::Super(_) => {
                    unimplemented!()
                }
                ExprOrSuper::Expr(expr) => expr,
            };
            if !member.computed {
                let (val, loc) = match *member.prop {
                    Expr::Ident(ident) => (ident.sym.to_string(), ident.span.lo.0 as usize),
                    _ => unimplemented!(),
                };
                return js_prop!(
                    (parse_expr(*obj, access.clone(), line_map.clone()))
                        [LocatedAction {
                            action: Action::Literal(JsValue::String(Rc::new(val))),
                            location: line_map.loc_for(loc)
                        }]
                    @ line_map.loc_for(member.span.lo.0 as usize)
                );
            }
            return js_prop!(
                (parse_expr(*obj, access.clone(), line_map.clone()))
                    [parse_expr(*member.prop, access, line_map.clone())]
                @ line_map.loc_for(member.span.lo.0 as usize)
            );
        }
        Expr::Cond(_) => {}
        Expr::Call(mut call) => {
            let callee = match call.callee {
                ExprOrSuper::Super(_) => {
                    unimplemented!()
                }
                ExprOrSuper::Expr(expr) => parse_expr(*expr, access.clone(), line_map.clone()),
            };

            let args_var = Identity::new();

            let mut args_assigns = Vec::new();

            let args_pos = call.span.lo.0 as usize;

            args_assigns.push(LocatedAction {
                action: Action::AssignProp(Box::new(AssignProp {
                    to: LocatedAction {
                        action: Action::ReadVar(args_var.clone()),
                        location: line_map.loc_for(args_pos),
                    },
                    key: LocatedAction {
                        action: Action::Literal(u_string("length")),
                        location: line_map.loc_for(args_pos),
                    },
                    what: LocatedAction {
                        action: Action::Literal(JsValue::Number(call.args.len() as f64)),
                        location: line_map.loc_for(args_pos),
                    },
                })),
                location: line_map.loc_for(args_pos),
            });

            for (i, expr) in call.args.drain(..).enumerate() {
                args_assigns.push(LocatedAction {
                    action: Action::AssignProp(Box::new(AssignProp {
                        to: LocatedAction {
                            action: Action::ReadVar(args_var.clone()),
                            location: line_map.loc_for(expr.span().lo.0 as usize),
                        },
                        key: LocatedAction {
                            action: Action::Literal(JsValue::Number(i as f64)),
                            location: line_map.loc_for(expr.span().lo.0 as usize),
                        },
                        what: parse_expr(*expr.expr, access.clone(), line_map.clone()),
                    })),
                    location: line_map.loc_for(args_pos),
                });
            }

            return js_block! {
                ({LocatedAction {
                    action: Action::VariableAssign(Box::new(VarAssign {
                        var: args_var.clone(),
                        right_side: Box::new(
                            LocatedAction {
                                action: Action::NewObject(Box::new(NewObject {
                                    is_array: true
                                })),
                                location: line_map.loc_for(args_pos)
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
                        location: line_map.loc_for(args_pos),
                        content: args_assigns
                    })),
                    location: line_map.loc_for(args_pos)
                }}

                {js_call!((callee)(args: LocatedAction {
                    action: Action::ReadVar(args_var),
                    location: line_map.loc_for(args_pos)
                }) @ line_map.loc_for(call.span.lo.0 as usize))}) @ line_map.loc_for(call.span.lo.0 as usize)
            };
        }
        Expr::New(_) => {}
        Expr::Seq(_) => {}
        Expr::Ident(ident) => {
            return LocatedAction {
                action: Action::ReadVar(access.get_or_global(&Rc::new(ident.sym.to_string()))),
                location: line_map.loc_for(ident.span.lo.0 as usize),
            }
        }
        Expr::Lit(literal) => match literal {
            Lit::Str(s) => {
                return LocatedAction {
                    action: Action::Literal(JsValue::String(Rc::new(s.value.to_string()))),
                    location: line_map.loc_for(s.span.lo.0 as usize),
                }
            }
            Lit::Bool(b) => {
                return LocatedAction {
                    action: Action::Literal(JsValue::Boolean(b.value)),
                    location: line_map.loc_for(b.span.lo.0 as usize),
                }
            }
            Lit::Null(null) => {
                return LocatedAction {
                    action: Action::Literal(JsValue::Null),
                    location: line_map.loc_for(null.span.lo.0 as usize),
                }
            }
            Lit::Num(n) => {
                return LocatedAction {
                    action: Action::Literal(JsValue::Number(n.value)),
                    location: line_map.loc_for(n.span.lo.0 as usize),
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

fn parse_function(f: Function, access: RcVarAccess, line_map: Rc<ColLineMap>) -> LocatedAction {
    let mut actions = Vec::new();

    for (i, param) in f.params.iter().enumerate() {
        match &param.pat {
            Pat::Ident(ident) => {
                let v = access.clone().local_declare(Rc::new(ident.sym.to_string()));
                actions.push(js_var!(
                    (v) = (js_prop!(
                        (js_var!((args()) @ line_map.loc_for(ident.span.lo.0 as usize)))
                            [js_static!((js_primitive!(i)) @ line_map.loc_for(ident.span.lo.0 as usize))]
                            @ line_map.loc_for(ident.span.lo.0 as usize)
                    )) @ line_map.loc_for(ident.span.lo.0 as usize)
                ));
            }
            _ => unimplemented!(),
        }
    }

    if let Some(body) = f.body {
        for stmt in body.stmts {
            actions.push(parse_stmt(stmt, access.clone(), line_map.clone()));
        }
    }

    return LocatedAction {
        action: Action::InstantiateFunction(Box::new(InstantiateFunction {
            name: None,
            content: LocatedAction {
                action: Action::Block(Box::new(ScopedBlock {
                    content: actions,
                    location: line_map.loc_for(f.span.lo.0 as usize),
                })),
                location: line_map.loc_for(f.span.lo.0 as usize),
            },
            captures: access.known_heap_vars(),
        })),
        location: line_map.loc_for(f.span.lo.0 as usize),
    };
}

fn parse_var_decl(
    mut decl: swc_ecma_ast::VarDecl,
    mut access: RcVarAccess,
    line_map: Rc<ColLineMap>,
) -> LocatedAction {
    let one = decl.decls.drain(..).next().unwrap();
    let name = match &one.name {
        Pat::Ident(ident) => ident.sym.to_string(),
        _ => unimplemented!(),
    };
    let var = access.local_declare(Rc::new(name));
    return js_var!(
        (var) = (one
            .init
            .map(|init| parse_expr(*init, access, line_map.clone()))
            .unwrap_or(LocatedAction {
                action: Action::Literal(JsValue::Undefined),
                location: line_map.loc_for(decl.span.lo.0 as usize)
            })) @ line_map.loc_for(one.span.lo.0 as usize)
    );
}
