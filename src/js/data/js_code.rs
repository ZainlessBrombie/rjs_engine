extern crate swc_ecma_parser;
use self::swc_ecma_parser::JscTarget;
use crate::js::data::execution_v2::function::FunctionInstance;
use crate::js::data::execution_v2::stack_executor::run_stack;
use crate::js::data::intermediate::converter::build_function;
use crate::js::data::intermediate::{empty_var_access, VarAccess, VarAccessTrait};
use crate::js::data::js_execution::JsVar::Stack;
use crate::js::data::js_execution::{EngineState, NativeFunction};
use crate::js::data::js_types::{JSCallable, JsFn, JsValue};
use crate::js::data::parsing::parse_module;
use crate::js::data::util::{
    s_pool, u_bool, u_null, u_number, u_string, u_true, u_undefined, JsObjectBuilder, VType,
};
use crate::*;
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

    let mut access = empty_var_access(None, false);
    let console = access.get_or_global(&s_pool("console"));
    let module = parse_module(module, access, Rc::new(source.into()));
    let mut stack = js::data::execution_v2::Stack::create_stack(build_function(
        module,
        console,
        s_pool(source),
    ));

    let mut steps: usize = 0;
    let mut target = String::new();
    println!("Press enter to start debugging...");
    loop {
        target.clear();
        std::io::stdin()
            .read_line(&mut target)
            .expect("error: unable to read user input");
        if target.ends_with("\n") {
            target.pop();
        }
        if target.as_str() == "stop" {
            break;
        }
        let mut do_step = 1;
        if target.len() != 0 {
            if let Ok(n) = target.parse::<usize>() {
                do_step = n;
            } else {
                println!("Unknown input '{}'", target);
                continue;
            }
        }
        steps += run_stack(&mut stack, do_step, true);
        println!("Step: {}", steps);
    }

    println!("Consumed: {}", steps);
}
