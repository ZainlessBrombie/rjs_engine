extern crate swc_ecma_parser;
use self::swc_ecma_parser::JscTarget;
use crate::js::data::gc_util::GcDestr;
use crate::js::data::js_execution::{build_demo_fn, EngineState, FnOp, JsVar};
use crate::js::data::js_types;
use crate::js::data::js_types::{JsNext, JsValue};
use crate::js::data::util::{u_deref, u_literal, u_standard_load_global, u_string};
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::marker::PhantomData;
use std::pin::Pin;
use std::rc::Rc;
use std::sync::atomic::AtomicU64;
use std::sync::{Arc, Mutex};
use swc_common::errors::{DiagnosticBuilder, Emitter};
use swc_common::sync::Lrc;
use swc_common::{errors::Handler, FileName, SourceMap};
use swc_ecma_ast::{Expr, Module, ModuleItem, Pat, Stmt, VarDecl, VarDeclOrExpr};
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
}

struct ScopeLookup {
    cur: HashMap<Rc<String>, JsVar>,
    prev: Option<Rc<RefCell<ScopeLookup>>>,
}

impl ScopeLookup {
    fn new(prev: Option<&Rc<RefCell<ScopeLookup>>>) -> Rc<RefCell<ScopeLookup>> {
        Rc::new(RefCell::new(ScopeLookup {
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
                    .map_or(None, |mut p| RefCell::borrow_mut(&p).get(&name))
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

    fn insert_top(&mut self, name: Rc<String>) -> JsVar {
        if let Some(prev) = &self.prev {
            return RefCell::borrow_mut(prev).insert_top(name);
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
                    return GcDestr::new(FnOp::Nop {});
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
        scopes: Rc<RefCell<ScopeLookup>>,
    ) -> Box<impl Fn(&'a Stmt) -> GcDestr<FnOp>> {
        return Box::new(move |stmt: &'a Stmt| {
            let this = self;
            match &stmt {
                Stmt::Block(block) => {
                    let block_scope = ScopeLookup::new(Some(&scopes));
                    return GcDestr::new(FnOp::Multi {
                        block: block
                            .stmts
                            .iter()
                            .map(|stm| this.ingest_statement(block_scope.clone())(stm))
                            .collect(),
                    });
                }
                Stmt::Empty(_stmt) => return GcDestr::new(FnOp::Nop {}),
                Stmt::Debugger(_stmt) => {
                    println!("Note: skipping debugger statement");
                    return GcDestr::new(FnOp::Nop {});
                }
                Stmt::With(_with_stmt) => {
                    println!("Note: skipping with statement");
                    return GcDestr::new(FnOp::Nop {});
                }
                Stmt::Return(ret_stmt) => {
                    if let Some(ret_stmt) = &ret_stmt.arg {
                        return GcDestr::new(FnOp::Return {
                            what: Box::new(this.ingest_expression(scopes.clone())(ret_stmt)),
                        });
                    } else {
                        return GcDestr::new(FnOp::Return {
                            what: Box::from(GcDestr::new(FnOp::LoadStatic {
                                value: JsValue::Undefined,
                            })),
                        });
                    }
                }
                Stmt::Labeled(_lbl) => {
                    println!("Note: label not supported");
                    return GcDestr::new(FnOp::Nop {});
                }
                Stmt::Break(_break_stmt) => {
                    println!("Note: break not supported");
                    return GcDestr::new(FnOp::Nop {});
                }
                Stmt::Continue(_) => {
                    println!("Note: continue not supported");
                    return GcDestr::new(FnOp::Nop {});
                }
                Stmt::If(if_stmt) => {
                    return GcDestr::new(FnOp::IfElse {
                        condition: Box::new(this.ingest_expression(scopes.clone())(&if_stmt.test)),
                        if_block: Box::new(this.ingest_statement(scopes.clone())(&if_stmt.cons)),
                        else_block: Box::new(
                            (&if_stmt.alt)
                                .as_ref()
                                .map(|stmt| this.ingest_statement(scopes.clone())(&stmt))
                                .unwrap_or(GcDestr::new(FnOp::Nop {})),
                        ),
                    })
                }
                Stmt::Switch(_) => {
                    println!("Note: switch not supported");
                    return GcDestr::new(FnOp::Nop {});
                }
                Stmt::Throw(throw_stmt) => {
                    return GcDestr::new(FnOp::Throw {
                        what: Box::new(this.ingest_expression(scopes.clone())(&throw_stmt.arg)),
                    })
                }
                Stmt::Try(_) => {
                    println!("Note: try not supported");
                    return GcDestr::new(FnOp::Nop {});
                }
                Stmt::While(while_stmt) => {
                    return GcDestr::new(FnOp::While {
                        condition: Box::new(this.ingest_expression(scopes.clone())(
                            &while_stmt.test,
                        )),
                        block: Box::new(this.ingest_statement(scopes.clone())(&while_stmt.body)),
                    })
                }
                Stmt::DoWhile(_) => {
                    println!("Note: do while not supported");
                    return GcDestr::new(FnOp::Nop {});
                }
                Stmt::For(for_stmt) => {
                    return GcDestr::new(FnOp::For {
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
        scopes: Rc<RefCell<ScopeLookup>>,
    ) -> Box<impl Fn(&VarDecl) -> GcDestr<FnOp>> {
        return Box::new(move |decl: &VarDecl| {
            return GcDestr::new(FnOp::Multi {
                block: decl
                    .decls
                    .iter()
                    .map(|decl_single| {
                        scopes.borrow_mut().insert_here(Rc::new(unimplemented!()));
                        let init = ScopeLookup::insert_here(
                            &mut RefCell::borrow_mut(&scopes),
                            Rc::new(unimplemented!()),
                        );
                        return unimplemented!();
                    })
                    .collect(),
            });
        });
    }

    fn ingest_assignment<'a>(
        &self,
        scopes: Rc<RefCell<ScopeLookup>>,
    ) -> Box<impl Fn(&Pat, GcDestr<FnOp>) -> GcDestr<FnOp>> {
        return Box::new(move |pat: &Pat, source| {
            match pat {
                Pat::Ident(ident) => {
                    let to = scopes
                        .borrow_mut()
                        .get(&Rc::new(ident.sym.to_string()))
                        .unwrap_or_else(|| {
                            scopes
                                .borrow_mut()
                                .insert_top(Rc::new(ident.sym.to_string()))
                        });
                    return GcDestr::new(FnOp::Assign {
                        target: to,
                        what: Box::new(source),
                    });
                }
                Pat::Array(arr) => {
                    let iterator = u_deref(
                        source,
                        u_deref(
                            u_standard_load_global("Symbol"),
                            u_literal(u_string("iterator")),
                        ),
                    );
                    for x in &arr.elems {}
                }
                Pat::Rest(_) => {}
                Pat::Object(_) => {}
                Pat::Assign(_) => {}
                Pat::Invalid(_) => {}
                Pat::Expr(_) => {}
            }
            unimplemented!()
        });
    }

    fn ingest_expression(
        &self,
        scopes: Rc<RefCell<ScopeLookup>>,
    ) -> Box<impl Fn(&Expr) -> GcDestr<FnOp>> {
        return Box::new(|expr: &Expr| {
            match &expr {
                Expr::This(_) => {
                    println!("This is not supported yet");
                    return GcDestr::new(FnOp::Nop {});
                }
                Expr::Array(arr_lit) => {}
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
                Expr::Arrow(arrow_expr) => {}
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
