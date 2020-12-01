extern crate swc_ecma_parser;
use self::swc_ecma_parser::JscTarget;
use crate::js::data::js_types;
use crate::js::data::js_types::JsNext;
use std::io::Write;
use std::rc::Rc;
use std::sync::atomic::AtomicU64;
use swc_common::errors::{DiagnosticBuilder, Emitter};
use swc_common::sync::Lrc;
use swc_common::{errors::Handler, FileName, SourceMap};
use swc_ecma_ast::{Module, ModuleItem, Stmt};
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

impl JsEngine {
    pub fn new() -> JsEngine {
        return JsEngine {
            eng: Rc::new(JsEngineInternal {
                max_mem: 1_000_000,
                cur_mem: AtomicU64::new(0),
            }),
        };
    }

    pub fn ingest_code(&self, module: Module) -> Box<dyn JsNext> {
        let mut next = js_types::next_done();

        for mod_item in module.body.iter() {
            match mod_item {
                ModuleItem::ModuleDecl(_declr) => {
                    println!("Note: Skipping module Decl")
                }
                ModuleItem::Stmt(stmt) => {
                    next = self.ingest_statement(next, stmt);
                }
            }
        }
        unimplemented!()
    }

    fn ingest_statement(&self, _prev: Box<dyn JsNext>, stmt: &Stmt) -> Box<dyn JsNext> {
        match stmt {
            Stmt::Block(_) => {}
            Stmt::Empty(_) => {}
            Stmt::Debugger(_) => {}
            Stmt::With(_) => {}
            Stmt::Return(_) => {}
            Stmt::Labeled(_) => {}
            Stmt::Break(_) => {}
            Stmt::Continue(_) => {}
            Stmt::If(_) => {}
            Stmt::Switch(_) => {}
            Stmt::Throw(_) => {}
            Stmt::Try(_) => {}
            Stmt::While(_) => {}
            Stmt::DoWhile(_) => {}
            Stmt::For(_) => {}
            Stmt::ForIn(_) => {}
            Stmt::ForOf(_) => {}
            Stmt::Decl(_) => {}
            Stmt::Expr(_) => {}
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
