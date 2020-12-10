use crate::js::data::execution_v2::constants::THIS_LOCATION;
use crate::js::data::intermediate::Action::VariableAssign;
use crate::js::data::intermediate::{
    Action, CodeLoc, For, IfElse, LocatedAction, Module, NewObject, ScopedBlock, VarAccess,
    VarAssign, VarDecl, While,
};
use crate::js::data::js_types::{Identity, JsValue};
use crate::js::data::util::{s_pool, u_bool, u_true};
use crate::*;
use std::collections::{HashMap, HashSet};
use std::process::exit;
use std::rc::Rc;
use swc_ecma_ast::{Decl, Expr, Function, ModuleItem, Stmt, VarDeclOrExpr};

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
            let mut access = VarAccess::empty(Some(access), false);
            unimplemented!()
        }
        Stmt::DoWhile(_) => {}
        Stmt::For(for_loop) => {
            /*return js_for!((js_action_lit!(a, {
                if for_loop.test.is_some() {
                    parse_expr(*for_loop.test.unwrap(), a)
                } else {
                    (js_block! {})(a)
                }
            }); js_action_lit!(a, {js_block!()(a)});js_action_lit!(a, {js_block!()(a)})) {

            })(access);*/
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

fn parse_expr(expr: Expr, access: &mut VarAccess) -> LocatedAction {
    match expr {
        Expr::This(_) => {
            return LocatedAction {
                action: Action::ReadVar(THIS_ID.with(|id| id.clone())),
                location: CodeLoc { line: 0, column: 0 },
            }
        }
        Expr::Array(arr_lit) => {
            let temp = access.local_declare(s_pool("temp(array literal)"));
        }
        Expr::Object(_) => {}
        Expr::Fn(_) => {}
        Expr::Unary(_) => {}
        Expr::Update(_) => {}
        Expr::Bin(_) => {}
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

fn parse_function(f: Function, access: &mut VarAccess) -> LocatedAction {}

fn parse_var_decl(decl: swc_ecma_ast::VarDecl, access: &mut VarAccess) -> LocatedAction {}
