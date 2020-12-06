extern crate swc_ecma_parser;
use self::swc_ecma_parser::JscTarget;
use crate::js::data::gc_util::GcDestr;
use crate::js::data::js_execution::{build_demo_fn, EngineState, FnOpRepr, JsVar};
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
    ArrowExpr, BlockStmtOrExpr, Expr, ExprOrSpread, ExprOrSuper, Function, Lit, Module, ModuleItem,
    ObjectPatProp, Pat, Prop, PropName, PropOrSpread, Stmt, VarDecl, VarDeclOrExpr,
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
struct ScopeLookup {
    cur: HashMap<Rc<String>, JsVar>,
    prev: Option<Rc<GcCell<ScopeLookup>>>,
}

impl ScopeLookup {
    fn new(prev: Option<&Rc<GcCell<ScopeLookup>>>) -> Rc<GcCell<ScopeLookup>> {
        Rc::new(GcCell::new(ScopeLookup {
            cur: Default::default(),
            prev: prev.map_or(None, |r| Some(r.clone())),
        }))
    }

    fn get(&self, name: &Rc<String>) -> Option<JsVar> {
        return self
            .cur
            .get(name)
            .map(|r| r.clone())
            .or_else(|| {
                self.prev
                    .as_ref()
                    .map_or(None, |mut p| GcCell::borrow_mut(&p).get(&name))
            })
            .map(|r| r.clone());
    }

    fn insert_here(&mut self, name: Rc<String>) -> JsVar {
        return self.get(&name).unwrap_or_else(|| {
            let ret = JsVar::new(name.clone());
            self.cur.insert(name.clone(), ret.clone());
            ret
        });
    }

    fn get_or_global(&mut self, name: Rc<String>) -> JsVar {
        return self.get(&name).unwrap_or_else(|| {
            let ret = JsVar::new(name.clone());
            self.insert_top(name);
            ret
        });
    }

    fn insert_top(&mut self, name: Rc<String>) -> JsVar {
        if let Some(prev) = &self.prev {
            return GcCell::borrow_mut(prev).insert_top(name);
        } else {
            let v = JsVar::new(name.clone());
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
    pub fn ingest_code(&self, mut module: Module) -> impl Fn() -> JsValue {
        module
            .body
            .drain(..)
            .map(|mod_item| match mod_item {
                ModuleItem::ModuleDecl(_declr) => {
                    println!("Skipping module item!");
                    return GcDestr::new(FnOpRepr::Nop {});
                }
                ModuleItem::Stmt(stmt) => {
                    return unimplemented!();
                    //return self.ingest_statement(&stmt, ScopeLookup::new(None));
                }
            })
            .collect::<Vec<GcDestr<_>>>();
        for mod_item in module.body.iter() {}
        || unimplemented!()
    }

    fn ingest_statement<'a>(
        &'a self,
        scopes: Rc<GcCell<ScopeLookup>>,
    ) -> Box<impl Fn(&'a Stmt) -> GcDestr<FnOpRepr>> {
        return Box::new(move |stmt: &'a Stmt| {
            let this = self;
            match &stmt {
                Stmt::Block(block) => {
                    let block_scope = ScopeLookup::new(Some(&scopes));
                    return GcDestr::new(FnOpRepr::Multi {
                        block: block
                            .stmts
                            .iter()
                            .map(|stm| this.ingest_statement(block_scope.clone())(stm))
                            .collect(),
                    });
                }
                Stmt::Empty(_stmt) => return GcDestr::new(FnOpRepr::Nop {}),
                Stmt::Debugger(_stmt) => {
                    println!("Note: skipping debugger statement");
                    return GcDestr::new(FnOpRepr::Nop {});
                }
                Stmt::With(_with_stmt) => {
                    println!("Note: skipping with statement");
                    return GcDestr::new(FnOpRepr::Nop {});
                }
                Stmt::Return(ret_stmt) => {
                    if let Some(ret_stmt) = &ret_stmt.arg {
                        return GcDestr::new(FnOpRepr::Return {
                            what: Box::new(this.ingest_expression(scopes.clone())(ret_stmt)),
                        });
                    } else {
                        return GcDestr::new(FnOpRepr::Return {
                            what: Box::from(GcDestr::new(FnOpRepr::LoadStatic {
                                value: JsValue::Undefined,
                            })),
                        });
                    }
                }
                Stmt::Labeled(_lbl) => {
                    println!("Note: label not supported");
                    return GcDestr::new(FnOpRepr::Nop {});
                }
                Stmt::Break(_break_stmt) => {
                    println!("Note: break not supported");
                    return GcDestr::new(FnOpRepr::Nop {});
                }
                Stmt::Continue(_) => {
                    println!("Note: continue not supported");
                    return GcDestr::new(FnOpRepr::Nop {});
                }
                Stmt::If(if_stmt) => {
                    return GcDestr::new(FnOpRepr::IfElse {
                        condition: Box::new(this.ingest_expression(scopes.clone())(&if_stmt.test)),
                        if_block: Box::new(this.ingest_statement(scopes.clone())(&if_stmt.cons)),
                        else_block: Box::new(
                            (&if_stmt.alt)
                                .as_ref()
                                .map(|stmt| this.ingest_statement(scopes.clone())(&stmt))
                                .unwrap_or(GcDestr::new(FnOpRepr::Nop {})),
                        ),
                    })
                }
                Stmt::Switch(_) => {
                    println!("Note: switch not supported");
                    return GcDestr::new(FnOpRepr::Nop {});
                }
                Stmt::Throw(throw_stmt) => {
                    return GcDestr::new(FnOpRepr::Throw {
                        what: Box::new(this.ingest_expression(scopes.clone())(&throw_stmt.arg)),
                    })
                }
                Stmt::Try(_) => {
                    println!("Note: try not supported");
                    return GcDestr::new(FnOpRepr::Nop {});
                }
                Stmt::While(while_stmt) => {
                    return GcDestr::new(FnOpRepr::While {
                        condition: Box::new(this.ingest_expression(scopes.clone())(
                            &while_stmt.test,
                        )),
                        block: Box::new(this.ingest_statement(scopes.clone())(&while_stmt.body)),
                    })
                }
                Stmt::DoWhile(_) => {
                    println!("Note: do while not supported");
                    return GcDestr::new(FnOpRepr::Nop {});
                }
                Stmt::For(for_stmt) => {
                    return GcDestr::new(FnOpRepr::For {
                        initial: /*Box::new(for_stmt.init.map(|var_or_expr| {
                            match var_or_expr {
                                VarDeclOrExpr::VarDecl(var_decl) => {}
                                VarDeclOrExpr::Expr(_) => {}
                            }
                        }),*/ unimplemented!(),
                        condition: Box::new(unimplemented!()),
                        each: Box::new(unimplemented!()),
                        block: Box::new(this.ingest_statement(scopes.clone())(&for_stmt.body))
                    });
                }
                Stmt::ForIn(_) => {}
                Stmt::ForOf(_) => {}
                Stmt::Decl(_) => {}
                Stmt::Expr(_) => {}
            }
            return unimplemented!();
        });
    }

    fn ingest_var_decl(
        &self,
        scopes: Rc<GcCell<ScopeLookup>>,
    ) -> Box<impl Fn(&VarDecl) -> GcDestr<FnOpRepr>> {
        return Box::new(move |decl: &VarDecl| {
            return GcDestr::new(FnOpRepr::Multi {
                block: decl
                    .decls
                    .iter()
                    .map(|decl_single| {
                        GcCell::borrow_mut(&scopes).insert_here(Rc::new(unimplemented!()));
                        let init = ScopeLookup::insert_here(
                            &mut GcCell::borrow_mut(&scopes),
                            Rc::new(unimplemented!()),
                        );
                        return unimplemented!();
                    })
                    .collect(),
            });
        });
    }

    fn ingest_assignment(
        &'a self,
        scopes: Rc<GcCell<ScopeLookup>>,
    ) -> Box<impl Fn(&'a Pat, GcDestr<FnOpRepr>, bool) -> GcDestr<FnOpRepr>> {
        // TODO dup code
        let scopes_copy = scopes.clone();
        let prop_name = move |prop: &PropName| {
            return match &prop {
                PropName::Ident(ident) => u_literal(u_string(ident.sym.to_string().as_str())),
                PropName::Str(sym) => u_literal(u_string(sym.value.to_string().as_str())),
                PropName::Num(n) => u_literal(u_number(n.value)),
                PropName::Computed(comp) => {
                    self.ingest_expression(scopes.clone())(comp.expr.as_ref())
                }
                PropName::BigInt(big_int) => u_literal(u_number(
                    format!("{:o}", big_int.value)
                        .parse()
                        .expect("bigint unimplemented!"),
                )),
            };
        };
        let scopes = scopes_copy;
        return Box::new(move |pat: &Pat, source, declare| {
            match pat {
                Pat::Ident(ident) => {
                    let to = GcCell::borrow_mut(&scopes)
                        .get(&Rc::new(ident.sym.to_string()))
                        .unwrap_or_else(|| {
                            if declare {
                                GcCell::borrow_mut(&scopes)
                                    .insert_top(Rc::new(ident.sym.to_string()))
                            } else {
                                GcCell::borrow_mut(&scopes)
                                    .insert_here(Rc::new(ident.sym.to_string()))
                            }
                        });
                    return GcDestr::new(FnOpRepr::Assign {
                        target: to,
                        what: Box::new(source),
                    });
                }
                Pat::Array(arr) => {
                    let iterator = u_deref(
                        source,
                        u_deref(u_load_global("Symbol"), u_literal(u_string("iterator"))),
                    );
                    let array = u_array_e();
                    let mut it_provider = u_cached(iterator);
                    let mut array_push_provider = u_cached(u_deref(
                        u_literal(array.clone()),
                        u_literal(u_string("push")),
                    ));
                    let mut result = Vec::new();
                    for x in &arr.elems {
                        if let Some(p) = x.as_ref() {
                            if let Pat::Rest(rest) = p {
                                let v = JsVar::new(Rc::new("#temp#".into()));
                                let v2 = JsVar::new(Rc::new("#temp#".into()));
                                v.set(JsValue::Boolean(true));
                                result.push(u_while(
                                    u_read_var(v.clone()),
                                    u_block(vec![
                                        u_write_var(v2.clone(), it_provider()),
                                        u_if_else(
                                            u_deref(
                                                u_read_var(v2.clone()),
                                                u_literal(u_string("done")),
                                            ),
                                            u_write_var(
                                                v.clone(),
                                                u_literal(JsValue::Boolean(false)),
                                            ),
                                            u_call(
                                                array_push_provider(),
                                                u_array(vec![u_deref(
                                                    u_read_var(v2.clone()),
                                                    u_literal(u_string("value")),
                                                )]),
                                            ),
                                        ),
                                    ]),
                                ));
                                result.push(self.ingest_assignment(scopes.clone())(
                                    &rest.arg,
                                    u_literal(array.clone()),
                                    declare,
                                ));
                                break;
                            }
                            result.push(self.ingest_assignment(scopes.clone())(
                                p,
                                u_call_simple(u_deref(it_provider(), u_literal(u_string("next")))),
                                declare,
                            ));
                        }
                    }
                    return GcDestr::new(FnOpRepr::Multi { block: result });
                }
                Pat::Rest(_) => {
                    unreachable!("We SHOULD have handled this in the code above?!")
                }
                Pat::Object(obj) => {
                    let mut result = Vec::new();
                    let mut target = u_cached(source);
                    for pat_prop in &obj.props {
                        match pat_prop {
                            ObjectPatProp::KeyValue(key_value) => {
                                // TODO is to_string ok?
                                result.push(self.ingest_assignment(scopes.clone())(
                                    &key_value.value,
                                    u_deref(target(), prop_name(&key_value.key)),
                                    false,
                                ))
                            }
                            ObjectPatProp::Assign(assign) => {
                                if let Some(v) = &assign.value {
                                    result.push(u_write_var(
                                        GcCell::borrow_mut(&scopes)
                                            .get_or_global(Rc::new(assign.key.sym.to_string())),
                                        self.ingest_expression(scopes.clone())(v),
                                    ));
                                } else {
                                    result.push(u_write_var(
                                        GcCell::borrow_mut(&scopes)
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
                    return self.ingest_assignment(scopes.clone())(
                        &assign.left,
                        self.ingest_expression(scopes.clone())(&assign.right),
                        false,
                    );
                }
                Pat::Invalid(invalid) => {
                    unimplemented!("Invalid assignment is todo")
                }
                Pat::Expr(expr) => {
                    return self.ingest_expression(scopes.clone())(&expr);
                }
            }
            unimplemented!()
        });
    }

    fn ingest_expression<'a>(
        &'a self,
        scopes: Rc<GcCell<ScopeLookup>>,
    ) -> Box<impl Fn(&'a Expr) -> GcDestr<FnOpRepr>> {
        let scopes_copy = scopes.clone();
        return Box::new(move |expr: &'a Expr| {
            let prop_name = |prop: &'a PropName| {
                return match prop {
                    PropName::Ident(ident) => u_literal(u_string(ident.sym.to_string().as_str())),
                    PropName::Str(sym) => u_literal(u_string(sym.value.to_string().as_str())),
                    PropName::Num(n) => u_literal(u_number(n.value)),
                    PropName::Computed(comp) => {
                        self.ingest_expression(scopes.clone())(comp.expr.as_ref())
                    }
                    PropName::BigInt(big_int) => u_literal(u_number(
                        format!("{:o}", big_int.value)
                            .parse()
                            .expect("bigint unimplemented!"),
                    )),
                };
            };
            let scopes = scopes_copy.clone();

            match &expr {
                Expr::This(_) => {
                    return u_this();
                }
                Expr::Array(arr_lit) => {
                    let arr = u_array_e();
                    let mut push =
                        u_cached(u_deref(u_literal(arr.clone()), u_literal(u_string("push"))));
                    let mut ret = Vec::new();
                    for exp in &arr_lit.elems {
                        if let Some(exp) = exp {
                            if let Some(_spread) = exp.spread {
                                let mut it = u_cached(u_deref(
                                    self.ingest_expression(scopes.clone())(&exp.expr),
                                    u_deref(
                                        u_load_global("Symbol"),
                                        u_literal(u_string("iterator")),
                                    ),
                                ));
                                // TODO dup code
                                let done = JsVar::new_t();
                                let (has_next, value) = u_it_next(it());
                                u_while(
                                    u_not(u_read_var(done.clone())),
                                    u_block(vec![
                                        u_call(push(), u_array(vec![value])),
                                        u_if(
                                            u_not(has_next),
                                            u_write_var(done, u_literal(u_true())),
                                        ),
                                    ]),
                                );
                            }
                            ret.push(u_call(
                                push(),
                                u_array(vec![self.ingest_expression(scopes.clone())(&exp.expr)]),
                            ))
                        } else {
                            ret.push(u_assign(
                                u_literal(arr.clone()),
                                u_literal(u_string("length")),
                                u_plus_num(
                                    u_deref(u_literal(arr.clone()), u_literal(u_string("length"))),
                                    u_literal(u_number(1.0)),
                                ),
                            ));
                        }
                        ret.push(u_literal(arr.clone()));
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
                                            GcCell::borrow_mut(&scopes)
                                                .get_or_global(Rc::new(name)),
                                        ),
                                    ))
                                }
                                Prop::KeyValue(kv) => ret.push(u_assign(
                                    u_literal(obj.clone()),
                                    prop_name(&kv.key),
                                    self.ingest_expression(scopes.clone())(&kv.value),
                                )),
                                Prop::Assign(assign) => {
                                    let k = assign.key.sym.to_string();
                                    ret.push(u_assign(
                                        u_literal(obj.clone()),
                                        u_literal(u_string(&k)),
                                        self.ingest_expression(scopes.clone())(&assign.value),
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
                                        self.ingest_function(scopes.clone())(&method.function),
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
                        let iterator = u_deref(
                            self.ingest_expression(scopes.clone())(&expr),
                            u_deref(u_load_global("Symbol"), u_literal(u_string("iterator"))),
                        );
                        let array = u_array_e();
                        let mut it_provider = u_cached(iterator);
                        let mut array_push_provider = u_cached(u_deref(
                            u_literal(array.clone()),
                            u_literal(u_string("push")),
                        ));

                        let mut ops = Vec::new();

                        for (i, arg_or_spread) in (&call.args).iter().enumerate() {
                            if let Some(_spread) = &arg_or_spread.spread {
                                let done = JsVar::new_t();
                                let (has_next, value) = u_it_next(it_provider());
                                u_while(
                                    u_not(u_read_var(done.clone())),
                                    u_block(vec![
                                        u_call(array_push_provider(), u_array(vec![value])),
                                        u_if(
                                            u_not(has_next),
                                            u_write_var(done, u_literal(u_true())),
                                        ),
                                    ]),
                                );
                                break;
                            }
                            ops.push(u_call(
                                u_literal(u_function(array_push_provider())),
                                u_array(vec![u_it_next(it_provider()).1]),
                            ));
                        }
                        ops.push(u_call(
                            self.ingest_expression(scopes.clone())(&expr),
                            u_literal(array),
                        ));
                        return GcDestr::new(FnOpRepr::Multi { block: ops });
                    }
                },
                Expr::New(n) => {
                    unimplemented!()
                }
                Expr::Seq(seq) => {
                    let mut result = Vec::new();
                    for expr in &seq.exprs {
                        result.push(self.ingest_expression(scopes.clone())(&expr))
                    }
                    return u_block(result);
                }
                Expr::Ident(ident) => {
                    let name: String = ident.sym.to_string();
                    return u_read_var(GcCell::borrow_mut(&scopes).get_or_global(Rc::new(name)));
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
                    // todo dup code

                    // TODO |   uh boy... can't wait to implement the proper engine parts...
                    // TODO |   this is about as safe & reasonable as cuddling a lion:
                    // TODO |   Can go well, but won't always and when it does not the owner (me)
                    // TODO |   has some explaining to do...
                    let arrow_expr: &'static ArrowExpr = unsafe { std::mem::transmute(arrow_expr) };
                    let self_: &'static mut Self = unsafe { std::mem::transmute(self) };

                    return u_literal(
                        JsObjectBuilder::new(None)
                            .with_callable(JSCallable::Js {
                                content: Rc::new("TODO".to_string()),
                                // TODO capturing function by ref is not safe here - will be sorted out when we separate AST and engine state
                                creator: Gc::from(JsFn::new(
                                    (scopes.clone(),),
                                    move |(scopes,), ret, args, this| {
                                        let scopes = ScopeLookup::new(Some(&scopes));
                                        let mut setup = Vec::new();
                                        for (i, par) in arrow_expr.params.iter().enumerate() {
                                            setup.push(self_.ingest_assignment(scopes.clone())(
                                                &par,
                                                u_deref(
                                                    u_literal(args.clone()),
                                                    u_literal(u_string(&i.to_string())),
                                                ),
                                                true,
                                            ));
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
                        what: Box::new(self.ingest_expression(scopes.clone())(&aw.arg)),
                    });
                }
                Expr::Paren(par) => return self.ingest_expression(scopes.clone())(&par.expr),
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
        });
    }

    fn ingest_function(
        &'a self,
        scopes: Rc<GcCell<ScopeLookup>>,
    ) -> Box<impl FnMut(&'a Function) -> GcDestr<FnOpRepr>> {
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
                            (scopes.clone(),),
                            move |(scopes,), ret, args, this| {
                                let scopes = ScopeLookup::new(Some(&scopes));
                                let mut setup = Vec::new();
                                for (i, par) in function.params.iter().enumerate() {
                                    setup.push(self_.ingest_assignment(scopes.clone())(
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
                                        setup.push(self_.ingest_statement(scopes.clone())(stmt))
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
