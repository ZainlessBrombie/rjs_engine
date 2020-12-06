extern crate swc_ecma_parser;
use self::swc_ecma_parser::JscTarget;
use crate::js::data::gc_util::GcDestr;
use crate::js::data::js_execution::{build_demo_fn, EngineState, FnOpRepr, JsVar, VarAlloc};
use crate::js::data::js_types;
use crate::js::data::js_types::{JSCallable, JsFn, JsNext, JsObj, JsValue};
use crate::js::data::util::{
    s_pool, u_array, u_array_e, u_assign, u_block, u_bool, u_cached, u_call, u_call_simple,
    u_deref, u_function, u_if, u_if_else, u_it_next, u_literal, u_load_global, u_not, u_null,
    u_number, u_obj, u_plus_num, u_read_var, u_return, u_string, u_this, u_true, u_undefined,
    u_while, u_write_var, JsObjectBuilder,
};
use gc::{Finalize, Gc, GcCell, Trace};
use std::any::Any;
use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::marker::PhantomData;
use std::ops::Deref;
use std::pin::Pin;
use std::rc::Rc;
use std::sync::atomic::AtomicU64;
use std::sync::{Arc, Mutex};
use swc_common::errors::{DiagnosticBuilder, Emitter};
use swc_common::sync::Lrc;
use swc_common::{errors::Handler, FileName, SourceMap};
use swc_ecma_ast::{
    ArrowExpr, BlockStmtOrExpr, Decl, Expr, ExprOrSpread, ExprOrSuper, Function, Lit, Module,
    ModuleItem, ObjectPatProp, Pat, Prop, PropName, PropOrSpread, Stmt, VarDecl, VarDeclOrExpr,
};
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax};

struct Empty {}

impl Emitter for Empty {
    fn emit(&mut self, db: &DiagnosticBuilder<'_>) {
        for (str, _) in db.styled_message() {
            println!("{}", str)
        }
    }

    fn should_show_explain(&self) -> bool {
        true
    }
}

pub struct JsEngine {
    eng: Rc<JsEngineInternal>,
}

struct JsEngineInternal {
    max_mem: u64,
    cur_mem: AtomicU64,
    state: EngineState,
}

#[derive(Trace, Finalize)]
pub struct ScopeLookup<'a> {
    cur: HashMap<Rc<String>, VarAlloc>,
    prev: Option<&'a mut ScopeLookup<'a>>,
    local_counter: usize,
    heap_break: bool,
}

impl ScopeLookup {
    pub fn new(
        prev: Option<&mut ScopeLookup>,
        heap_break: bool,
    ) -> ScopeLookup {
        ScopeLookup {
            cur: Default::default(),
            prev: prev.map_or(None, |r| Some(r.clone())),
            local_counter: 2,
            heap_break,
        }
    }

    pub fn get(&mut self, name: &Rc<String>, heaped: bool) -> Option<VarAlloc> {
        let mut b = 1;
        let a = Rc::new(&mut b);
        *a += 1; // TODO
        let ret = self
            .cur
            .get(name)
            .map(|r| {
                if heaped {
                    return VarAlloc::CapturedAt {
                        name: name.clone(),
                        from: r.clone(),
                        target: 0
                    };
                }
                r.clone()
            })
            .or_else(|| {
                self.prev
                    .as_ref()
                    .map_or(None, |mut p| {
                        if heaped {
                            p.get(&name, false).map(|source| {
                                return VarAlloc::CapturedAt {
                                    name: name.clone(),
                                    from: source,
                                    target: 0
                                };
                            })
                        } else {
                            p.get(&name, self.heap_break) // TODO possibly incorrect
                        }
                    })
            });
    }

    pub fn insert_here(&mut self, name: Rc<String>) -> VarAlloc {
        return self.get(&name, false).unwrap_or_else(|| {
            let ret = VarAlloc::LocalAt(name.clone(), self.local_counter);
            self.local_counter += 1;
            self.cur.insert(name.clone(), ret.clone());
            ret
        });
    }

    fn get_incr_local_counter(&mut self) -> Option<usize> {
        if self.heap_break {
            self.local_counter += 1;
            return Option::from(self.local_counter - 1);
        } else if let Some(prev) = &self.prev {
            return RefCell::borrow_mut(prev).get_incr_local_counter();
        } else {
            return None; // We are global
        }
    }

    pub fn get_or_global(&mut self, name: Rc<String>) -> VarAlloc {
        return self
            .get(&name, false)
            .unwrap_or_else(|| self.insert_top(name));
    }

    pub fn insert_top(&mut self, name: Rc<String>) -> VarAlloc {
        if let Some(prev) = self.prev {
            return prev.insert_top(name);
        } else {
            let v = VarAlloc::Static(name, JsVar::new_t());
            self.cur.insert(name.clone(), v.clone());
            return v;
        }
    }
}

impl JsEngine {
    pub fn new() -> JsEngine {
        return JsEngine {
            eng: Rc::new(JsEngineInternal {
                max_mem: 1_000_000,
                cur_mem: AtomicU64::new(0),
                state: EngineState {
                    tick_queue: vec![],
                    external_calls: Arc::new(Mutex::new(vec![])),
                },
            }),
        };
    }

    /// Returns a JsValue that represents a function. when it is called, the module is executed and returned by the function.
    pub fn ingest_code(&self, mut module: Module) -> JsValue {
        let mut scope = ScopeLookup::new(None, false);
        let ret =module
            .body
            .drain(..)
            .map(|mod_item| match &mod_item {
                ModuleItem::ModuleDecl(_declr) => {
                    println!("Skipping module item!");
                    return FnOpRepr::Nop {};
                }
                ModuleItem::Stmt(stmt) => {
                    return self.ingest_statement(&mut scope, stmt);
                }
            })
            .collect::<Vec<_>>();
        return JsObjectBuilder::new(None)
            .with_callable(JSCallable::Js { content: Rc::new("".into()), creator: Gc::new(JsFn { ops: Rc::new(FnOpRepr::Multi { block: ret }), captures: Rc::new(vec![]) })})
            .build();
    }

    fn ingest_statement(&self, scopes: &mut ScopeLookup, stmt: &Stmt) -> FnOpRepr {
        // TODO make scopelookup a &mut
        match &stmt {
            Stmt::Block(block) => {
                let block_scope = ScopeLookup::new(Some(scopes), false);
                return FnOpRepr::Multi {
                    block: block
                        .stmts
                        .iter()
                        .map(|stmt| self.ingest_statement(block_scope.clone(), stmt))
                        .collect(),
                };
            }
            Stmt::Empty(_stmt) => return FnOpRepr::Nop {},
            Stmt::Debugger(_stmt) => {
                println!("Note: skipping debugger statement");
                return FnOpRepr::Nop {};
            }
            Stmt::With(_with_stmt) => {
                println!("Note: skipping with statement");
                return FnOpRepr::Nop {};
            }
            Stmt::Return(ret_stmt) => {
                if let Some(ret_stmt) = &ret_stmt.arg {
                    return FnOpRepr::Return {
                        what: Rc::new(self.ingest_expression(scopes, ret_stmt)),
                    };
                } else {
                    return FnOpRepr::Return {
                        what: Rc::from(FnOpRepr::LoadStatic {
                            value: JsValue::Undefined,
                        }),
                    };
                }
            }
            Stmt::Labeled(_lbl) => {
                println!("Note: label not supported");
                return FnOpRepr::Nop {};
            }
            Stmt::Break(_break_stmt) => {
                println!("Note: break not supported");
                return FnOpRepr::Nop {};
            }
            Stmt::Continue(_) => {
                println!("Note: continue not supported");
                return FnOpRepr::Nop {};
            }
            Stmt::If(if_stmt) => {
                return FnOpRepr::IfElse {
                    condition: Rc::new(
                        self.ingest_expression(
                            &mut ScopeLookup::new(Some(scopes), false),
                            &if_stmt.test,
                        ),
                    ),
                    if_block: Rc::new(
                        self.ingest_statement(
                            &mut ScopeLookup::new(Some(scopes), false),
                            &if_stmt.cons,
                        ),
                    ),
                    else_block: Rc::new(
                        (&if_stmt.alt)
                            .as_ref()
                            .map(|stmt| self.ingest_statement(scopes, &stmt))
                            .unwrap_or(FnOpRepr::Nop {}),
                    ),
                }
            }
            Stmt::Switch(_) => {
                println!("Note: switch not supported");
                return FnOpRepr::Nop {};
            }
            Stmt::Throw(throw_stmt) => {
                return FnOpRepr::Throw {
                    what: Rc::new(this.ingest_expression(scopes, &throw_stmt.arg)),
                }
            }
            Stmt::Try(_) => {
                println!("Note: try not supported");
                return FnOpRepr::Nop {};
            }
            Stmt::While(while_stmt) => {
                return FnOpRepr::While {
                    condition: Rc::new(self.ingest_expression(scopes, &while_stmt.test)),
                    block: Rc::new(self.ingest_statement(
                        &mut ScopeLookup::new(Some(scopes), false),
                        &while_stmt.body,
                    )),
                }
            }
            Stmt::DoWhile(_) => {
                println!("Note: do while not supported");
                return FnOpRepr::Nop {};
            }
            Stmt::For(for_stmt) => {
                return FnOpRepr::For {
                    initial: Rc::new(
                        for_stmt
                            .init
                            .map(|var_or_expr| match var_or_expr {
                                VarDeclOrExpr::VarDecl(var_decl) => {
                                    self.ingest_var_decl(scopes, &var_decl)
                                }
                                VarDeclOrExpr::Expr(expr) => {
                                    self.ingest_expression(scopes, expr.as_ref())
                                }
                            })
                            .unwrap_or(FnOpRepr::Nop {}),
                    ),
                    condition: Rc::new(
                        for_stmt
                            .test
                            .map(|test| self.ingest_expression(scopes, &test))
                            .unwrap_or(FnOpRepr::Nop {}),
                    ),
                    each: Rc::new(
                        for_stmt
                            .update
                            .map(|test| self.ingest_expression(scopes, &test))
                            .unwrap_or(FnOpRepr::Nop {}),
                    ),
                    block: Rc::new(
                        self.ingest_statement(
                            &mut ScopeLookup::new(Some(scopes), false),
                            &for_stmt.body,
                        ),
                    ),
                };
            }
            Stmt::ForIn(_) => {
                unimplemented!()
            }
            Stmt::ForOf(_) => {
                unimplemented!()
            }
            Stmt::Decl(decl) => match decl {
                Decl::Class(_) => {
                    unimplemented!()
                }
                Decl::Fn(f) => {
                    return self.ingest_function(scopes, &f.function);
                }
                Decl::Var(v) => {
                    return self.ingest_var_decl(scopes, v);
                }
                Decl::TsInterface(_) => {
                    unimplemented!()
                }
                Decl::TsTypeAlias(_) => {
                    unimplemented!()
                }
                Decl::TsEnum(_) => {
                    unimplemented!()
                }
                Decl::TsModule(_) => {
                    unimplemented!()
                }
            },
            Stmt::Expr(expr) => {
                return self.ingest_expression(scopes, &expr.expr);
            }
        }
    }

    fn ingest_var_decl(&self, scopes: &mut ScopeLookup, decl: &VarDecl) -> FnOpRepr {
        return FnOpRepr::Multi {
            block: decl
                .decls
                .iter()
                .map(|decl_single| {
                    // GcCell::borrow_mut(&scopes).insert_here(Rc::new(unimplemented!()));
                    let init = ScopeLookup::insert_here(
                        scopes,
                        Rc::new(unimplemented!()),
                    );
                    return unimplemented!();
                })
                .collect(),
        };
    }

    fn ingest_assignment(
        &'a self,
        scopes: &mut ScopeLookup,
        pat: &Pat,
        source: FnOpRepr,
        declare: bool,
    ) -> FnOpRepr {
        // TODO dup code
        let scopes_copy = scopes;
        let prop_name = move |prop: &PropName| {
            return match &prop {
                PropName::Ident(ident) => u_literal(u_string(ident.sym.to_string().as_str())),
                PropName::Str(sym) => u_literal(u_string(sym.value.to_string().as_str())),
                PropName::Num(n) => u_literal(u_number(n.value)),
                PropName::Computed(comp) => {
                    self.ingest_expression(scopes, comp.expr.as_ref())
                }
                PropName::BigInt(big_int) => u_literal(u_number(
                    format!("{:o}", big_int.value)
                        .parse()
                        .expect("bigint unimplemented!"),
                )),
            };
        };
        let scopes = scopes_copy;
        match pat {
            Pat::Ident(ident) => {
                let to = GcCell::borrow_mut(scopes)
                    .get(&Rc::new(ident.sym.to_string()))
                    .unwrap_or_else(|| {
                        if declare {
                            GcCell::borrow_mut(scopes).insert_top(Rc::new(ident.sym.to_string()))
                        } else {
                            GcCell::borrow_mut(scopes).insert_here(Rc::new(ident.sym.to_string()))
                        }
                    });
                return FnOpRepr::Assign {
                    target: to,
                    what: Rc::new(source),
                };
            }
            Pat::Array(arr) => {
                let mut result = Vec::new();

                let iterator = u_deref(
                    source,
                    u_deref(u_load_global("Symbol"), u_literal(u_string("iterator"))),
                );
                let (arr_var, array) = u_cached(scopes, u_array_e());
                result.push(array.clone());

                let mut it_provider = u_cached(scopes, iterator).1;
                let (push_var, mut array_push_provider) = u_cached(
                    scopes,
                    u_deref(u_read_var(arr_var), u_literal(u_string("push"))),
                );
                result.push(array_push_provider.clone());

                for x in &arr.elems {
                    if let Some(p) = x.as_ref() {
                        if let Pat::Rest(rest) = p {
                            let v = scopes.insert_here(Rc::new("".into()));
                            let v2 = scopes.insert_here(Rc::new("".into()));
                            result.push(u_write_var(v, u_literal(u_true())));
                            result.push(u_while(
                                u_read_var(v.clone()),
                                u_block(vec![
                                    u_write_var(v2.clone(), it_provider.clone()),
                                    u_if_else(
                                        u_deref(
                                            u_read_var(v2.clone()),
                                            u_literal(u_string("done")),
                                        ),
                                        u_write_var(v.clone(), u_literal(JsValue::Boolean(false))),
                                        u_call(
                                            arr_var.clone(),
                                            array_push_provider.clone(),
                                            u_array(vec![u_deref(
                                                u_read_var(v2.clone()),
                                                u_literal(u_string("value")),
                                            )]),
                                        ),
                                    ),
                                ]),
                            ));
                            result.push(self.ingest_assignment(scopes, &rest.arg, array.clone(), declare));                            ));
                            break;
                        }
                        result.push(self.ingest_assignment(scopes)(
                            p,
                            u_call_simple(arr_var.clone(), u_deref(it_provider.clone(), u_literal(u_string("next")))),
                            declare,
                        ));
                    }
                }
                return FnOpRepr::Multi { block: result };
            }
            Pat::Rest(_) => {
                unreachable!("We SHOULD have handled this in the code above?!")
            }
            Pat::Object(obj) => {
                let mut result = Vec::new();
                let mut target = u_cached(scopes, source).1;
                for pat_prop in &obj.props {
                    match pat_prop {
                        ObjectPatProp::KeyValue(key_value) => {
                            // TODO is to_string ok?
                            result.push(self.ingest_assignment(scopes, &key_value.value, target.clone()));
                        }
                        ObjectPatProp::Assign(assign) => {
                            if let Some(v) = &assign.value {
                                result.push(u_write_var(
                                    scopes
                                        .get_or_global(Rc::new(assign.key.sym.to_string())),
                                    self.ingest_expression(scopes, v),
                                ));
                            } else {
                                result.push(u_write_var(
                                    scopes
                                        .get_or_global(Rc::new(assign.key.sym.to_string())),
                                    u_deref(
                                        target(),
                                        u_literal(u_string(&assign.key.sym.to_string())),
                                    ),
                                ))
                            }
                        }
                        ObjectPatProp::Rest(obj_rest) => {
                            unimplemented!("Object rest destructing not complete because I want to execute code lol")
                        }
                    }
                }
            }
            Pat::Assign(assign) => {
                return self.ingest_assignment(scopes)(
                    &assign.left,
                    self.ingest_expression(scopes)(&assign.right),
                    false,
                );
            }
            Pat::Invalid(invalid) => {
                unimplemented!("Invalid assignment is todo")
            }
            Pat::Expr(expr) => {
                return self.ingest_expression(scopes)(&expr);
            }
        }
        unimplemented!()
    }

    fn ingest_expression(&self, scopes: &mut ScopeLookup, expr: &Expr) -> FnOpRepr {
        let prop_name = |prop: &PropName| {
            return match prop {
                PropName::Ident(ident) => u_literal(u_string(ident.sym.to_string().as_str())),
                PropName::Str(sym) => u_literal(u_string(sym.value.to_string().as_str())),
                PropName::Num(n) => u_literal(u_number(n.value)),
                PropName::Computed(comp) => self.ingest_expression(scopes, &comp.expr),
                PropName::BigInt(big_int) => u_literal(u_number(
                    format!("{:o}", big_int.value)
                        .parse()
                        .expect("bigint unimplemented!"),
                )),
            };
        };

        match &expr {
            Expr::This(_) => {
                return u_this();
            }
            Expr::Array(arr_lit) => {
                let arr = u_array_e();
                let mut push = u_deref(arr.clone(), u_literal(u_string("push")));
                let mut ret = Vec::new();
                for exp in &arr_lit.elems {
                    if let Some(exp) = exp {
                        if let Some(_spread) = exp.spread {
                            let mut it = u_cached(&scopes, u_deref(
                                self.ingest_expression(scopes, &exp.expr),
                                u_deref(u_load_global("Symbol"), u_literal(u_string("iterator"))),
                            )).1;
                            // TODO dup code
                            let done = scopes.insert_here(Rc::new("".into()));
                            let (has_next, value) = u_it_next(it());
                            u_while(
                                u_not(u_read_var(done.clone())),
                                u_block(vec![
                                    u_call(array.clone(), push(), u_array(vec![value])),
                                    u_if(u_not(has_next), u_write_var(done, u_literal(u_true()))),
                                ]),
                            );
                        }
                        ret.push(u_call(
                            push(),
                            u_array(vec![self.ingest_expression(scopes)(&exp.expr)]),
                        ))
                    } else {
                        ret.push(u_assign(
                            arr.clone(),
                            u_literal(u_string("length")),
                            u_plus_num(
                                u_deref(arr.clone(), u_literal(u_string("length"))),
                                u_literal(u_number(1.0)),
                            ),
                        ));
                    }
                    ret.push(arr.clone());
                    return u_block(ret);
                }
            }
            Expr::Object(lit) => {
                let obj = u_obj();
                let mut ret = Vec::new();
                for prop_or_spread in &lit.props {
                    match prop_or_spread {
                        PropOrSpread::Spread(spread) => {}
                        PropOrSpread::Prop(prop) => match Box::deref(&prop) {
                            Prop::Shorthand(shorthand) => {
                                let name: String = shorthand.sym.to_string();
                                ret.push(u_assign(
                                    u_literal(obj.clone()),
                                    u_literal(u_string(name.as_str())),
                                    u_read_var(
                                        scopes.get_or_global(Rc::new(name)),
                                    ),
                                ))
                            }
                            Prop::KeyValue(kv) => ret.push(u_assign(
                                u_literal(obj.clone()),
                                prop_name(&kv.key),
                                self.ingest_expression(scopes)(&kv.value),
                            )),
                            Prop::Assign(assign) => {
                                let k = assign.key.sym.to_string();
                                ret.push(u_assign(
                                    u_literal(obj.clone()),
                                    u_literal(u_string(&k)),
                                    self.ingest_expression(scopes)(&assign.value),
                                ))
                            }
                            Prop::Getter(_getter) => {
                                unimplemented!("getters are not implemented yet")
                            }
                            Prop::Setter(_setter) => {
                                unimplemented!("setters are not implemented yet")
                            }
                            Prop::Method(method) => {
                                ret.push(u_assign(
                                    u_literal(obj.clone()),
                                    prop_name(&method.key),
                                    self.ingest_function(scopes)(&method.function),
                                ));
                            }
                        },
                    }
                }
            }
            Expr::Fn(_) => {}
            Expr::Unary(_) => {}
            Expr::Update(up) => {}
            Expr::Bin(_) => {}
            Expr::Assign(_) => {}
            Expr::Member(_) => {}
            Expr::Cond(_) => {}
            Expr::Call(call) => match &call.callee {
                ExprOrSuper::Super(super_call) => {
                    unimplemented!()
                }
                ExprOrSuper::Expr(expr) => {
                    let mut ops = Vec::new();

                    let iterator = u_deref(
                        self.ingest_expression(scopes, &expr),
                        u_deref(u_load_global("Symbol"), u_literal(u_string("iterator"))),
                    );
                    let (array_v, array) = u_cached(&scopes, u_array_e());
                    ops.push(array);
                    let mut it_provider = u_cached(&scopes, iterator);
                    let (push_var, array_push_provider) = u_cached(&scopes, u_deref(
                        array.clone(),
                        u_literal(u_string("push")),
                    ));


                    for (i, arg_or_spread) in (&call.args).iter().enumerate() {
                        if let Some(_spread) = &arg_or_spread.spread {
                            let done = scopes.insert_here(s_pool(""));
                            let (has_next, value) = u_it_next(it_provider());
                            u_while(
                                u_not(u_read_var(done.clone())),
                                u_block(vec![
                                    u_call(array_v.clone(), array_push_provider.clone(), u_array(vec![value])),
                                    u_if(u_not(has_next), u_write_var(done, u_literal(u_true()))),
                                ]),
                            );
                            break;
                        }
                        ops.push(u_call(
                            array_v.clone(),
                            array_push_provider.clone(),
                            u_array(vec![u_it_next(it_provider()).1]),
                        ));
                    }
                    ops.push(u_call(
                        scopes.insert_here(s_pool("")),
                        self.ingest_expression(scopes,&expr),
                        array.clone(),
                    ));
                    return FnOpRepr::Multi { block: ops };
                }
            },
            Expr::New(n) => {
                unimplemented!()
            }
            Expr::Seq(seq) => {
                let mut result = Vec::new();
                for expr in &seq.exprs {
                    result.push(self.ingest_expression(scopes)(&expr))
                }
                return u_block(result);
            }
            Expr::Ident(ident) => {
                let name: String = ident.sym.to_string();
                return u_read_var(scopes.get_or_global(Rc::new(name)));
            }
            Expr::Lit(lit) => match lit {
                Lit::Str(s) => return u_literal(u_string(&s.value.to_string())),
                Lit::Bool(b) => return u_literal(u_bool(b.value)),
                Lit::Null(_) => return u_literal(u_null()),
                Lit::Num(n) => {
                    return u_literal(u_number(n.value));
                }
                Lit::BigInt(big_int) => {
                    return u_literal(u_number(
                        format!("{:o}", big_int.value)
                            .parse()
                            .expect("bigint unimplemented!"),
                    ));
                }
                Lit::Regex(regex) => {
                    unimplemented!()
                }
                Lit::JSXText(_) => {
                    unimplemented!()
                }
            },
            Expr::Tpl(_) => {
                unimplemented!("no jsx")
            }
            Expr::TaggedTpl(_) => {
                unimplemented!("no jsx")
            }
            Expr::Arrow(arrow_expr) => {
                let arrow_scope = ScopeLookup::new(Some(scopes), true);
                let mut arrow_body = Vec::new();
                match &arrow_expr.body {
                    BlockStmtOrExpr::BlockStmt(b) => {
                        for stmt in &b.stmts {
                            arrow_body.push(self.ingest_statement(arrow_scope.clone(), stmt));
                        }
                    }
                    BlockStmtOrExpr::Expr(expr) => {
                        arrow_body.push(FnOpRepr::Return { what: Rc::from(self.ingest_expression(scopes, &expr)) });
                    }
                }

                return u_literal(
                    JsObjectBuilder::new(None)
                        .with_callable(JSCallable::Js {
                            content: Rc::new("TODO".to_string()),
                            creator: JSCallable::Js { content: Rc::new("".to_string()), creator: Gc::new(JsFn
                            { ops: Rc::new(FnOpRepr::Multi { block: arrow_body }), captures: Rc::new(vec![]) }) }
                        })
                        .build(),
                );
            }
            Expr::Class(_) => {
                unimplemented!("classes not implemented")
            }
            Expr::Yield(_) => {
                unimplemented!("generators not implemented")
            }
            Expr::MetaProp(_) => {
                unimplemented!("meta props not implemented")
            }
            Expr::Await(aw) => {
                // TODO we are not handling await yet
                return GcDestr::new(FnOpRepr::Await {
                    what: Box::new(self.ingest_expression(scopes)(&aw.arg)),
                });
            }
            Expr::Paren(par) => return self.ingest_expression(scopes)(&par.expr),
            Expr::JSXMember(_) => {
                unimplemented!()
            }
            Expr::JSXNamespacedName(_) => {
                unimplemented!()
            }
            Expr::JSXEmpty(_) => {
                unimplemented!()
            }
            Expr::JSXElement(_) => {
                unimplemented!()
            }
            Expr::JSXFragment(_) => {
                unimplemented!()
            }
            Expr::TsTypeAssertion(_) => {
                unimplemented!()
            }
            Expr::TsConstAssertion(_) => {
                unimplemented!()
            }
            Expr::TsNonNull(_) => {
                unimplemented!()
            }
            Expr::TsTypeCast(_) => {
                unimplemented!()
            }
            Expr::TsAs(_) => {
                unimplemented!()
            }
            Expr::PrivateName(_) => {
                unimplemented!()
            }
            Expr::OptChain(_) => {
                unimplemented!("optional chaining not supported yet")
            }
            Expr::Invalid(_) => {
                unimplemented!()
            }
        }
        unimplemented!()
    }

    fn ingest_function(&'a self, scopes: &mut ScopeLookup, function: &Function) -> FnOpRepr {
        return Box::new(move |function: &Function| {
            if !function.decorators.is_empty() {
                panic!("decorators (@) unimplemented!")
            }

            // TODO |   uh boy... can't wait to implement the proper engine parts...
            // TODO |   this is about as safe & reasonable as cuddling a lion:
            // TODO |   Can go well, but won't always and when it does not the owner (me)
            // TODO |   has some explaining to do...
            let function: &'static Function = unsafe { std::mem::transmute(function) };
            let mut self_: &'static mut Self = unsafe { std::mem::transmute(self) };

            u_literal(
                JsObjectBuilder::new(None)
                    .with_callable(JSCallable::Js {
                        content: Rc::new("TODO".to_string()),
                        // TODO capturing function by ref is not safe here - will be sorted out when we separate AST and engine state
                        creator: Gc::from(JsFn::new(
                            (scopes,),
                            move |(scopes,), ret, args, this| {
                                let scopes = ScopeLookup::new(Some(&scopes));
                                let mut setup = Vec::new();
                                for (i, par) in function.params.iter().enumerate() {
                                    setup.push(self_.ingest_assignment(scopes)(
                                        &par.pat,
                                        u_deref(
                                            u_literal(args.clone()),
                                            u_literal(u_string(&i.to_string())),
                                        ),
                                        true,
                                    ));
                                }
                                if let Some(body) = &function.body {
                                    for stmt in &body.stmts {
                                        setup.push(self_.ingest_statement(scopes)(stmt))
                                    }
                                }
                                StackFrame {
                                    vars: vec![], // TODO
                                    remaining_ops: setup,
                                    this,
                                    ret_store: ret,
                                }
                            },
                        )),
                    })
                    .build(),
            );

            unimplemented!()
        });
    }
}

struct TempFix {}

impl Write for TempFix {
    fn write(&mut self, buf: &[u8]) -> Result<usize, std::io::Error> {
        std::io::stdout().write(buf)
    }

    fn flush(&mut self) -> Result<(), std::io::Error> {
        Ok(())
    }
}

pub fn m1() {
    println!("Running\na = 'Hello Wonderful World!'; console.log(a)\n(hand compiled)\n");
    let demo_fn = build_demo_fn();
    let mut engine_state = EngineState {
        tick_queue: vec![],
        external_calls: Arc::new(Mutex::new(vec![])),
    };
    engine_state.get_queuer().enqueue_js_fn(demo_fn);
    let consumed = engine_state.run_queue(100000000);
    println!("\nConsumed: {}", consumed);
    let cm: Lrc<SourceMap> = Default::default();
    let handler = Handler::with_emitter(true, false, Box::new(Empty {}));
    let fm = cm.new_source_file(
        FileName::Custom("test.js".into()),
        "function foo() {}".into(),
    );
    let lexer = Lexer::new(
        Syntax::Es(Default::default()),
        JscTarget::Es2020,
        StringInput::from(&*fm),
        None,
    );
    let mut parser = Parser::new_from(lexer);

    for e in parser.take_errors() {
        e.into_diagnostic(&handler).emit();
    }

    let _module = parser
        .parse_module()
        .map_err(|err| err.into_diagnostic(&handler).emit())
        .unwrap();
}
