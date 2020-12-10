use crate::js::data::execution_v2::constants::THIS_LOCATION;
use crate::js::data::execution_v2::opcode::Arithmetic2Op;
use crate::js::data::execution_v2::opcode::OpCode::ReadProp;
use crate::js::data::intermediate::Action::VariableAssign;
use crate::js::data::intermediate::{
    Action, Arithmetic2Action, CodeLoc, For, IfElse, InstantiateFunction, LeftRight, LocatedAction,
    Module, NewObject, ScopedBlock, VarAccess, VarAssign, VarDecl, While,
};
use crate::js::data::js_types::{Identity, JsValue};
use crate::js::data::util::{s_pool, u_bool, u_true};
use crate::*;
use std::collections::{HashMap, HashSet};
use std::process::exit;
use std::rc::Rc;
use swc_ecma_ast::{BinaryOp, Decl, Expr, Function, ModuleItem, Pat, Stmt, VarDeclOrExpr};

thread_local! {
    static THIS_ID: Identity = Identity::new();
    static ARGS_ID: Identity = Identity::new();
}

pub fn parse_module(module: swc_ecma_ast::Module, vars: &mut VarAccess) -> Module {
    for item in module.body {
        match item {
            ModuleItem::ModuleDecl(_) => {
                println!("Module Decl not supported");
            }
            ModuleItem::Stmt(stmt) => {}
        }
    }
    unimplemented!()
}

fn parse_stmt(stmt: Stmt, access: &'a mut VarAccess<'a>) -> LocatedAction {
    match stmt {
        Stmt::Block(block) => {
            let mut action = Vec::new();
            for stmt in block.stmts {
                action.push(parse_stmt(stmt, &mut VarAccess::empty(Some(access), false)));
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
            return js_if_else!((js_action_lit!(a, parse_expr(*if_block.test, a))) {
                js_action_lit!(a, parse_stmt(*if_block.cons, a))
            } else {
                js_action_lit!(a, parse_stmt(*if_block.cons, a))
            })(access);
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
            js_while!((js_action_lit!(a, parse_expr(*while_loop.test, a))) {
                js_action_lit!(a, parse_stmt(*while_loop.body, a))
            });
        }
        Stmt::DoWhile(_) => {}
        Stmt::For(for_loop) => {
            return js_for!((js_action_lit!(a, {
                if for_loop.init.is_some() {
                    match for_loop.init.unwrap() {
                        VarDeclOrExpr::VarDecl(decl) => {
                            parse_var_decl(decl, a)
                        }
                        VarDeclOrExpr::Expr(expr) => {
                            parse_expr(*expr, a)
                        }
                    }
                } else {
                    (js_block! {})(a)
                }
            }); js_action_lit!(a, {
                if for_loop.test.is_some() {
                    parse_expr(*for_loop.test.unwrap(), a)
                } else {
                    (js_block! {})(a)
                }
            }); js_action_lit!(a, {
                if let Some(update) = for_loop.update {
                    parse_expr(*update, a)
                } else {
                    js_block!()(a)
                }
            })) {
                js_action_lit!(a, parse_stmt(*for_loop.body, a))
            })(access);
        }
        Stmt::ForIn(_) => {}
        Stmt::ForOf(_) => {}
        Stmt::Decl(decl) => match decl {
            Decl::Class(_) => {}
            Decl::Fn(f) => {
                let mut access = VarAccess::empty(Some(access), true);
                return parse_function(f.function, &mut access);
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

fn parse_expr(expr: Expr, access: &'a mut VarAccess<'a>) -> LocatedAction {
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
                    (js_action_lit!(a, parse_expr(*bin.left, a)))
                        == (js_action_lit!(a, parse_expr(*bin.left, a)))
                )(access);
            }
            BinaryOp::NotEq => {
                return js_bin!(
                    (js_action_lit!(a, parse_expr(*bin.left, a)))
                        != (js_action_lit!(a, parse_expr(*bin.left, a)))
                )(access);
            }
            BinaryOp::EqEqEq => {
                return js_bin!(
                    (js_action_lit!(a, parse_expr(*bin.left, a)))
                        === (js_action_lit!(a, parse_expr(*bin.left, a)))
                )(access);
            }
            BinaryOp::NotEqEq => {
                return js_bin!(
                    (js_action_lit!(a, parse_expr(*bin.left, a)))
                        !== (js_action_lit!(a, parse_expr(*bin.left, a)))
                )(access);
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
                    (js_action_lit!(a, parse_expr(*bin.left, a)))
                        + (js_action_lit!(a, parse_expr(*bin.left, a)))
                )(access);
            }
            BinaryOp::Sub => {
                return js_bin!(
                    (js_action_lit!(a, parse_expr(*bin.left, a)))
                        - (js_action_lit!(a, parse_expr(*bin.left, a)))
                )(access);
            }
            BinaryOp::Mul => {
                return js_bin!(
                    (js_action_lit!(a, parse_expr(*bin.left, a)))
                        * (js_action_lit!(a, parse_expr(*bin.left, a)))
                )(access);
            }
            BinaryOp::Div => {
                return js_bin!(
                    (js_action_lit!(a, parse_expr(*bin.left, a)))
                        / (js_action_lit!(a, parse_expr(*bin.left, a)))
                )(access);
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
                    (js_action_lit!(a, parse_expr(*bin.left, a)))
                        * *(js_action_lit!(a, parse_expr(*bin.left, a)))
                )(access);
            }
            BinaryOp::NullishCoalescing => {}
        },
        Expr::Assign(_) => {}
        Expr::Member(_) => {}
        Expr::Cond(_) => {}
        Expr::Call(_) => {}
        Expr::New(_) => {}
        Expr::Seq(_) => {}
        Expr::Ident(_) => {}
        Expr::Lit(_) => {}
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

fn this() -> Identity {
    return THIS_ID.with(|a| a.clone());
}
fn args() -> Identity {
    return ARGS_ID.with(|a| a.clone());
}

fn parse_function(f: Function, access: &'a mut VarAccess<'a>) -> LocatedAction {
    let mut actions = Vec::new();

    for (i, param) in f.params.iter().enumerate() {
        match &param.pat {
            Pat::Ident(ident) => {
                let v = access.local_declare(Rc::new(ident.sym.to_string()));
                actions.push(js_var!(
                    (v) = js_prop!((js_var!(args()))[js_static!(js_primitive!(i))])
                )(access));
            }
            _ => unimplemented!(),
        }
    }

    if let Some(body) = f.body {
        for stmt in body.stmts {
            actions.push(parse_stmt(stmt, access));
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
        })),
        location: CodeLoc { line: 0, column: 0 },
    };
}

fn parse_var_decl(mut decl: swc_ecma_ast::VarDecl, access: &'a mut VarAccess<'a>) -> LocatedAction {
    let one = decl.decls.drain(..).next().unwrap();
    let name = match &one.name {
        Pat::Ident(ident) => ident.sym.to_string(),
        _ => unimplemented!(),
    };
    let var = access.local_declare(Rc::new(name));
    return js_var!(
        (var) = js_action_lit!(
            a,
            one.init
                .map(|init| parse_expr(*init, a))
                .unwrap_or(LocatedAction {
                    action: Action::Literal(JsValue::Undefined),
                    location: CodeLoc {
                        column: 0,
                        line: 0
                    }
                })
        )
    )(access);
}
