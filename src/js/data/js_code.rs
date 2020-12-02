extern crate swc_ecma_parser;
use self::swc_ecma_parser::JscTarget;
use crate::js::data::gc_util::GcDestr;
use crate::js::data::js_execution::{build_demo_fn, EngineState, FnOp, JsVar};
use crate::js::data::js_types;
use crate::js::data::js_types::{JsNext, JsValue};
use std::io::Write;
use std::rc::Rc;
use std::sync::atomic::AtomicU64;
use std::sync::{Arc, Mutex};
use swc_common::errors::{DiagnosticBuilder, Emitter};
use swc_common::sync::Lrc;
use swc_common::{errors::Handler, FileName, SourceMap};
use swc_ecma_ast::{Expr, Module, ModuleItem, Stmt, VarDeclOrExpr, VarDecl, Pat};
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax};
use std::collections::HashMap;
use std::rt::panic_count::get;
use std::cell::RefCell;

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
    prev: Option<Rc<RefCell<ScopeLookup>>>
}

impl ScopeLookup {
    fn new(prev: Option<&Rc<RefCell<ScopeLookup>>>) -> Rc<RefCell<ScopeLookup>> {
        Rc::new(RefCell::new(ScopeLookup {
            cur: Default::default(),
            prev: prev.map_or(None, |r| Some(r.clone()))
        }))
    }

    fn get(&self, name: &Rc<String>) -> Option<JsVar> {
        return self.cur.get(name).map(|r| r.clone()).or_else(|| self.prev
            .map_or(None, |mut p| RefCell::borrow_mut(&p).get(&name))).map(|r| r.clone());
    }

    fn insert_here(&mut self, name: Rc<String>) -> Option<JsVar> {
        return self.get(&name).or_else(|| {
            self.cur.insert(name.clone(), JsVar::new(name))
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
        let mut next = js_types::next_done();

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
                    return self.ingest_statement(next, stmt);
                }
            })
            .collect();
        for mod_item in module.body.iter() {}
        unimplemented!()
    }

    fn ingest_statement(&self, stmt: &Stmt, scopes: Rc<RefCell<ScopeLookup>>) -> impl Fn() -> GcDestr<FnOp> {
        return || {
            match stmt {
                Stmt::Block(mut block) => {
                    let block_scope = ScopeLookup::new(Some(&scopes));
                    return GcDestr::new(FnOp::Multi {
                        block: block
                            .stmts
                            .iter()
                            .map(|stm| self.ingest_statement(stm, block_scope.clone())())
                            .collect(),
                    })
                }
                Stmt::Empty(stmt) => return GcDestr::new(FnOp::Nop {}),
                Stmt::Debugger(stmt) => {
                    println!("Note: skipping debugger statement");
                    return GcDestr::new(FnOp::Nop {});
                }
                Stmt::With(with_stmt) => {
                    println!("Note: skipping with statement");
                    return GcDestr::new(FnOp::Nop {});
                }
                Stmt::Return(ret_stmt) => {
                    if let Some(ret_stmt) = &ret_stmt.arg {
                        return GcDestr::new(FnOp::Return {
                            what: Box::new(self.ingest_expression(ret_stmt, scopes)),
                        });
                    } else {
                        return GcDestr::new(FnOp::Return {
                            what: Box::from(GcDestr::new(FnOp::LoadStatic {
                                value: JsValue::Undefined,
                            })),
                        });
                    }
                }
                Stmt::Labeled(lbl) => {
                    println!("Note: label not supported");
                    return GcDestr::new(FnOp::Nop {});
                }
                Stmt::Break(break_stmt) => {
                    println!("Note: break not supported");
                    return GcDestr::new(FnOp::Nop {});
                }
                Stmt::Continue(_) => {
                    println!("Note: continue not supported");
                    return GcDestr::new(FnOp::Nop {});
                }
                Stmt::If(if_stmt) => {
                    return GcDestr::new(FnOp::IfElse {
                        condition: Box::new(self.ingest_expression(&if_stmt.test, scopes.clone())()),
                        if_block: Box::new(self.ingest_statement(&if_stmt.cons, scopes.clone())()),
                        else_block: Box::new(
                            if_stmt
                                .alt
                                .map(|stmt| self.ingest_statement(&stmt, scopes)())
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
                        what: Box::new(self.ingest_expression(&throw_stmt.arg, scopes)()),
                    })
                }
                Stmt::Try(_) => {
                    println!("Note: try not supported");
                    return GcDestr::new(FnOp::Nop {});
                }
                Stmt::While(while_stmt) => {
                    return GcDestr::new(FnOp::While {
                        condition: Box::new(self.ingest_expression(&while_stmt.test, scopes.clone())()),
                        block: Box::new(self.ingest_statement(&while_stmt.body, scopes)()),
                    })
                }
                Stmt::DoWhile(_) => {
                    println!("Note: do while not supported");
                    return GcDestr::new(FnOp::Nop {});
                }
                Stmt::For(for_stmt) => {
                    return GcDestr::new(FnOp::For {
                        initial: Box::new(for_stmt.init.map(|var_or_expr| {
                            match var_or_expr {
                                VarDeclOrExpr::VarDecl(var_decl) => {}
                                VarDeclOrExpr::Expr(_) => {}
                            }
                        }).map_or(GcDestr::new(FnOp::Nop {}))
                            .),
                        condition: Box::new(()),
                        each: Box::new(()),
                        block: Box::new(self.ingest_statement(*for_stmt.body))
                    })
                }
                Stmt::ForIn(_) => {}
                Stmt::ForOf(_) => {}
                Stmt::Decl(_) => {}
                Stmt::Expr(_) => {}
            }
        }
    }

    fn ingest_var_decl(&self, decl: &VarDecl, scopes: Rc<RefCell<ScopeLookup>>) -> impl Fn() -> GcDestr<FnOp> {
        return || {
            return GcDestr::new(FnOp::Multi { block: decl.decls.iter().map(|decl_single| {
                let init = ScopeLookup::insert_here(scopes.borrow_mut(), Rc::new(decl_single.name))
            }).collect()});
        };
    }

    fn ingest_assignment(&self, pat: &Pat, scopes: Rc<RefCell<ScopeLookup>>) -> impl Fn() -> GcDestr<FnOp> {
        return || {
            match pat {
                Pat::Ident(_) => {}
                Pat::Array(_) => {}
                Pat::Rest(_) => {}
                Pat::Object(_) => {}
                Pat::Assign(_) => {}
                Pat::Invalid(_) => {}
                Pat::Expr(_) => {}
            }
        };
    }

    fn ingest_expression(&self, expr: &Expr, scopes: Rc<RefCell<ScopeLookup>>) ->impl Fn() -> GcDestr<FnOp>  {
        match expr {
            Expr::This(_) => {}
            Expr::Array(_) => {}
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
