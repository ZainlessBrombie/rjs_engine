extern crate swc_ecma_parser;
use self::swc_ecma_parser::JscTarget;
use crate::js::data::js_execution::{EngineState, NativeFunction};
use crate::js::data::js_types::{JSCallable, JsFn, JsValue};
use crate::js::data::util::{
    s_pool, u_bool, u_null, u_number, u_string, u_true, u_undefined, JsObjectBuilder, OpBuilder,
    VType,
};
use safe_gc::{GcCell, Mark};
use std::io::Write;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::atomic::AtomicU64;
use std::sync::{Arc, Mutex};
use swc_common::errors::{DiagnosticBuilder, Emitter};
use swc_common::sync::Lrc;
use swc_common::{errors::Handler, FileName, SourceMap};
use swc_ecma_ast::{
    BinaryOp, Decl, Expr, ExprOrSuper, Function, Lit, Module, ModuleItem, ObjectPatProp, Pat,
    PatOrExpr, PropName, Stmt, VarDecl, VarDeclOrExpr,
};
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax};
#[macro_use]
use crate::*;

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
    eng: Box<JsEngineInternal>,
}

struct JsEngineInternal {
    max_mem: u64,
    cur_mem: AtomicU64,
    state: EngineState,
}

impl JsEngine {
    pub fn new() -> JsEngine {
        return JsEngine {
            eng: Box::new(JsEngineInternal {
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
        let mut b = OpBuilder::start();
        js_var!(
            "console" = js_val!(o: {
                [js_val!("console")]: js_native! (function (this, args) {
                    println!("{}", args.to_system_string());
                })
            })
        )(&mut b);

        js_var!(
            "Array" = js_val!(o: {
                [js_val!("push")]: js_function! {
                    js_var!(
                    (js_this!())[js_prop!(( {js_this!()} )["length"])]
                        = js_this!()
                    )
                }
            })
        );

        b.var_w(s_pool("Array"), |b| {
            let tv = b.var_t();
            b.var_w_t(tv.clone(), |b| {
                b.obj_e();
            });

            // Assign push
            b.assign_ref(
                |b| b.var_r_t(tv.clone()),
                |b| {
                    b.literal(u_string("push"));
                },
                |b| {
                    // Push Fn
                    b.func(|b| {
                        // Get current length
                        let len = b.var_t();
                        b.var_w_t(len.clone(), |b| {
                            b.deref(
                                |b| {
                                    let this = b.this();
                                    b.var_r_t(this);
                                },
                                |b| {
                                    b.literal(u_string("length"));
                                },
                            );
                        });
                        b.var_w_t(len.clone(), |b| {
                            b.literal(u_number(0.0));
                        });
                        // Getting length done

                        // Assigning args[0] to this[this.length]
                        let alloc = b.this();
                        b.assign_ref(
                            |b| b.var_r_t(alloc),
                            |b| b.var_r_t(len.clone()),
                            |b| {
                                let alloc1 = b.args();
                                b.deref(
                                    |b| b.var_r_t(alloc1),
                                    |b| {
                                        b.literal(u_number(0.0));
                                    },
                                );
                            },
                        );

                        b.assign_ref(
                            |b| {
                                let alloc2 = b.this();
                                b.var_r_t(alloc2);
                            },
                            |b| {
                                b.literal(u_string("length"));
                            },
                            |b| {
                                b.numeral_add(
                                    |b| {
                                        b.var_r_t(len.clone());
                                    },
                                    |b| {
                                        b.literal(u_number(1.0));
                                    },
                                );
                            },
                        );

                        b.literal(u_undefined());
                    });
                    // End push Fn
                },
            );
            // End assign push
            b.var_r_t(tv);
        });

        for mod_item in module.body.drain(..) {
            match &mod_item {
                ModuleItem::ModuleDecl(_declr) => {
                    println!("Skipping module item!");
                }
                ModuleItem::Stmt(stmt) => {
                    self.ingest_statement(&mut b, stmt);
                }
            }
        }
        b.build()
    }

    fn ingest_statement(&self, b: &mut OpBuilder, stmt: &Stmt) {
        match &stmt {
            Stmt::Block(block) => {
                b.block(|b| {
                    for stmt in &block.stmts {
                        self.ingest_statement(b, stmt)
                    }
                });
            }
            Stmt::Empty(_stmt) => {}
            Stmt::Debugger(_stmt) => {
                println!("Note: skipping debugger statement");
            }
            Stmt::With(_with_stmt) => {
                println!("Note: skipping with statement");
            }
            Stmt::Return(ret_stmt) => {
                if let Some(ret_stmt) = &ret_stmt.arg {
                    b.ret(|b| {
                        self.ingest_expression(b, ret_stmt);
                    });
                } else {
                    b.ret(|b| {
                        b.static_v(u_undefined());
                    });
                }
            }
            Stmt::Labeled(_lbl) => {
                println!("Note: label not supported");
            }
            Stmt::Break(_break_stmt) => {
                println!("Note: break not supported");
            }
            Stmt::Continue(_) => {
                println!("Note: continue not supported");
            }
            Stmt::If(if_stmt) => {
                b.if_elseb(
                    |cb| {
                        self.ingest_expression(cb, &if_stmt.test);
                    },
                    |bb| {
                        self.ingest_statement(bb, &if_stmt.cons);
                    },
                    |eb| {
                        if let Some(alt) = &if_stmt.alt {
                            self.ingest_statement(eb, &alt);
                        }
                    },
                );
            }
            Stmt::Switch(_) => {
                println!("Note: switch not supported");
            }
            Stmt::Throw(throw_stmt) => {
                b.throw(|b| {
                    self.ingest_expression(b, &throw_stmt.arg);
                });
            }
            Stmt::Try(_) => {
                println!("Note: try not supported");
            }
            Stmt::While(while_stmt) => {
                b.while_l(
                    |b| {
                        self.ingest_expression(b, &while_stmt.test);
                    },
                    |b| self.ingest_statement(b, &while_stmt.body),
                );
            }
            Stmt::DoWhile(_) => {
                println!("Note: do while not supported");
            }
            Stmt::For(for_stmt) => {
                b.for_l(
                    |b| {
                        if let Some(init) = &for_stmt.init {
                            match init {
                                VarDeclOrExpr::VarDecl(dec) => {}
                                VarDeclOrExpr::Expr(expr) => {}
                            }
                        }
                    },
                    |b| {
                        if let Some(cond) = &for_stmt.test {
                            self.ingest_expression(b, &cond);
                        } else {
                            b.literal(u_true());
                        }
                    },
                    |b| {
                        if let Some(each) = &for_stmt.update {
                            self.ingest_expression(b, &each);
                        }
                    },
                    |b| {
                        self.ingest_statement(b, &for_stmt.body);
                    },
                );
            }
            Stmt::ForIn(_) => {}
            Stmt::ForOf(_) => {}
            Stmt::Decl(decl) => match decl {
                Decl::Class(_) => {}
                Decl::Fn(f) => {
                    b.var_dec(VType::Let, Rc::new(f.ident.sym.to_string()));
                    b.var_w(Rc::new(f.ident.sym.to_string()), |b| {
                        self.ingest_function(b, &f.function);
                    });
                }
                Decl::Var(v) => {
                    self.ingest_var_decl(b, v);
                }
                Decl::TsInterface(_) => {}
                Decl::TsTypeAlias(_) => {}
                Decl::TsEnum(_) => {}
                Decl::TsModule(_) => {}
            },
            Stmt::Expr(expr) => {
                self.ingest_expression(b, &expr.expr);
            }
        }
    }

    fn ingest_var_decl(&self, b: &mut OpBuilder, decl: &VarDecl) {
        for x in decl.decls.iter().map(|decl_single| {
            self.ingest_assignment(b, &decl_single.name, |b| {
                if let Some(init) = &decl_single.init {
                    self.ingest_expression(b, &init);
                }
            });
        }) {}
    }

    fn ingest_assignment(
        &'a self,
        b: &mut OpBuilder,
        pat: &Pat,
        source: impl FnOnce(&mut OpBuilder),
    ) {
        // TODO dup code
        let prop_name = move |prop: &PropName| {
            return match &prop {
                PropName::Ident(ident) => u_string(ident.sym.to_string().as_str()),
                PropName::Str(sym) => u_string(sym.value.to_string().as_str()),
                PropName::Num(n) => u_number(n.value),
                PropName::Computed(comp) => unimplemented!(),
                PropName::BigInt(big_int) => u_number(
                    format!("{:o}", big_int.value)
                        .parse()
                        .expect("bigint unimplemented!"),
                ),
            };
        };
        match pat {
            Pat::Ident(ident) => {
                b.var_dec(VType::Let, Rc::new(ident.sym.to_string()));
                b.var_w(Rc::new(ident.sym.to_string()), source);
            }
            Pat::Array(arr) => {
                let arr_v = b.var_t();
                b.var_w_t(arr_v.clone(), |b| {
                    b.array_e();
                });
                let it = b.var_t();
                b.var_w_t(it.clone(), |b| {
                    b.deref(
                        |b| {
                            b.var_r_t(arr_v.clone());
                        },
                        |b| {
                            b.deref(
                                |b| b.load_global(s_pool("Symbol")),
                                |b| {
                                    b.static_v(u_string("iterator"));
                                },
                            );
                        },
                    );
                });

                for x in &arr.elems {
                    unimplemented!()
                }
            }
            Pat::Rest(_) => {
                unreachable!("We SHOULD have handled this in the code above?!")
            }
            Pat::Object(obj) => {
                for pat_prop in &obj.props {
                    match pat_prop {
                        ObjectPatProp::KeyValue(key_value) => {
                            unimplemented!()
                        }
                        ObjectPatProp::Assign(assign) => {
                            if let Some(v) = &assign.value {
                                unimplemented!()
                            } else {
                                unimplemented!()
                            }
                        }
                        ObjectPatProp::Rest(obj_rest) => {
                            unimplemented!("Object rest destructing not complete because I want to execute code lol")
                        }
                    }
                }
            }
            Pat::Assign(assign) => {
                self.ingest_assignment(b, &assign.left, source);
            }
            Pat::Invalid(invalid) => {
                unimplemented!("Invalid assignment is todo")
            }
            Pat::Expr(expr) => {
                match Box::deref(expr) {
                    Expr::Member(mem) => {
                        b.assign_ref(
                            |b| {
                                let ex = match &mem.obj {
                                    ExprOrSuper::Super(_) => {
                                        unimplemented!()
                                    }
                                    ExprOrSuper::Expr(ex) => ex,
                                };
                                self.ingest_expression(b, &ex);
                            },
                            |b| {
                                self.ingest_expression(b, &mem.prop);
                            },
                            source,
                        );
                    }
                    _ => {
                        unimplemented!()
                    }
                }
                self.ingest_expression(b, &expr);
            }
        }
    }

    fn ingest_expression(&self, b: &mut OpBuilder, expr: &Expr) {
        let prop_name = |b: &mut OpBuilder, prop: &PropName| {
            return match prop {
                PropName::Ident(ident) => {
                    b.literal(u_string(ident.sym.to_string().as_str()));
                }
                PropName::Str(sym) => {
                    b.literal(u_string(sym.value.to_string().as_str()));
                }
                PropName::Num(n) => {
                    b.literal(u_number(n.value));
                }
                PropName::Computed(comp) => unimplemented!(),
                PropName::BigInt(big_int) => {
                    b.literal(u_number(
                        format!("{:o}", big_int.value)
                            .parse()
                            .expect("bigint unimplemented!"),
                    ));
                }
            };
        };

        match &expr {
            Expr::This(_) => {
                let alloc = b.this();
                b.var_r_t(alloc);
            }
            Expr::Array(arr_lit) => {
                unimplemented!()
            }
            Expr::Object(lit) => {
                unimplemented!()
            }
            Expr::Fn(f) => {
                self.ingest_function(b, &f.function);
            }
            Expr::Unary(_) => {}
            Expr::Update(up) => {}
            Expr::Bin(bin) => match &bin.op {
                BinaryOp::Add => {
                    b.numeral_add(
                        |b| {
                            self.ingest_expression(b, &bin.left);
                        },
                        |b| {
                            self.ingest_expression(b, &bin.right);
                        },
                    );
                }
                _ => {
                    unimplemented!("{:?}", bin)
                }
            },
            Expr::Assign(assign) => match &assign.left {
                PatOrExpr::Expr(expr) => {
                    unimplemented!()
                }
                PatOrExpr::Pat(pat) => {
                    self.ingest_assignment(b, pat, |b| {
                        self.ingest_expression(b, &assign.right);
                    });
                }
            },
            Expr::Member(member) => {
                // omg thank you swc
                let temp = b.var_t();

                b.var_w_t(temp.clone(), |b| {
                    match &member.obj {
                        ExprOrSuper::Super(_) => {
                            unimplemented!()
                        }
                        ExprOrSuper::Expr(ex) => self.ingest_expression(b, &ex),
                    };
                });

                match Box::deref(&member.prop) {
                    Expr::Ident(ident) => {
                        b.deref(
                            |b| {
                                b.var_r_t(temp.clone());
                            },
                            |b| {
                                b.literal(u_string(&ident.sym.to_string())); // TODO this stuff...
                            },
                        );
                    }
                    _ => {
                        b.deref(
                            |b| {
                                b.var_r_t(temp.clone());
                            },
                            |b| {
                                self.ingest_expression(b, &member.prop);
                            },
                        );
                    }
                }
            }
            Expr::Cond(_cond) => {
                // ternary
                unimplemented!()
            }
            Expr::Call(call) => match &call.callee {
                ExprOrSuper::Super(_super_call) => {
                    unimplemented!()
                }
                ExprOrSuper::Expr(expr) => {
                    let this = b.var_t();
                    b.call(
                        this,
                        |b| {
                            self.ingest_expression(b, &expr);
                        },
                        |b| {
                            let arr_v = b.var_t();
                            b.var_w_t(arr_v.clone(), |b| {
                                b.array_e();
                            });
                            let push = b.var_t();
                            b.var_w_t(push.clone(), |b| {
                                b.deref(
                                    |b| {
                                        b.var_r_t(arr_v.clone());
                                    },
                                    |b| {
                                        b.literal(u_string("push"));
                                    },
                                );
                            });
                            for exp in &call.args {
                                b.call(
                                    arr_v.clone(),
                                    |b| {
                                        b.var_r_t(push.clone());
                                    },
                                    |b| {
                                        // TODO spread
                                        let vt = b.var_t();
                                        b.var_w_t(vt.clone(), |b| {
                                            b.array_e();
                                        });
                                        b.assign_ref(
                                            |b| b.var_r_t(vt.clone()),
                                            |b| {
                                                b.literal(u_number(0.0));
                                            },
                                            |b| {
                                                self.ingest_expression(b, &exp.expr);
                                            },
                                        );
                                        b.var_r_t(vt.clone());
                                    },
                                );
                            }
                            b.var_r_t(arr_v);
                        },
                    );
                }
            },
            Expr::New(_n) => {
                unimplemented!()
            }
            Expr::Seq(_seq) => {
                unimplemented!()
            }
            Expr::Ident(ident) => {
                b.var_r(Rc::new(ident.sym.to_string()));
            }
            Expr::Lit(lit) => match lit {
                Lit::Str(s) => {
                    b.literal(u_string(&s.value.to_string()));
                }
                Lit::Bool(bo) => {
                    b.literal(u_bool(bo.value));
                }
                Lit::Null(_) => {
                    b.literal(u_null());
                }
                Lit::Num(n) => {
                    b.literal(u_number(n.value));
                }
                Lit::BigInt(big_int) => {
                    b.literal(u_number(
                        format!("{:o}", big_int.value)
                            .parse()
                            .expect("bigint unimplemented!"),
                    ));
                }
                Lit::Regex(_regex) => {
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
            Expr::Arrow(_arrow_expr) => {
                unimplemented!()
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
            Expr::Await(_aw) => {
                // TODO we are not handling await yet
                unimplemented!()
            }
            Expr::Paren(par) => {
                self.ingest_expression(b, &par.expr);
            }
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
    }

    fn ingest_function(&self, b: &mut OpBuilder, function: &Function) {
        if !function.decorators.is_empty() {
            panic!("decorators (@) unimplemented!")
        }

        b.func(|b| {
            for (i, param) in function.params.iter().enumerate() {
                self.ingest_assignment(b, &param.pat, |b| {
                    let args = b.args();
                    b.deref(
                        |b| b.var_r_t(args),
                        |b| {
                            b.literal(u_number(i as f64));
                        },
                    );
                });
            }
            for stmt in &function.body {
                for stmt in &stmt.stmts {
                    self.ingest_statement(b, stmt);
                }
            }
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
    println!("Running small test program\n");
    let mut engine_state = EngineState {
        tick_queue: vec![],
        external_calls: Arc::new(Mutex::new(vec![])),
    };
    let consumed = engine_state.run_queue(100000000);
    println!("\nConsumed: {}", consumed);
    let cm: Lrc<SourceMap> = Default::default();
    let handler = Handler::with_emitter(true, false, Box::new(Empty {}));
    let source = "{
        console.log('Hello World');
    }";
    let fm = cm.new_source_file(FileName::Custom("test.js".into()), source.into());
    let lexer = Lexer::new(
        Syntax::Es(Default::default()),
        JscTarget::Es2020,
        StringInput::from(&*fm),
        None,
    );
    let mut parser = Parser::new_from(lexer);

    let module = parser
        .parse_module()
        .map_err(|err| {
            println!("{:?}", err.into_diagnostic(&handler).span);
            for e in parser.take_errors() {
                e.into_diagnostic(&handler).emit();
            }
            panic!()
        })
        .unwrap();

    let mut engine = JsEngine {
        eng: Box::new(JsEngineInternal {
            max_mem: 10000,
            cur_mem: Default::default(),
            state: engine_state,
        }),
    };
    let module = engine.ingest_code(module);
    engine.eng.state.get_queuer().enqueue_js_fn(module);

    engine.eng.state.run_queue(1000);
}
