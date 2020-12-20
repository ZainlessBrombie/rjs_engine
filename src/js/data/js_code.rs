extern crate swc_ecma_parser;
use self::swc_ecma_parser::JscTarget;
use crate::js::data::execution_v2::stack_executor::run_stack;
use crate::js::data::intermediate::converter::build_function;
use crate::js::data::intermediate::empty_var_access;
use crate::js::data::parsing::parse_module;
use crate::js::data::std::build_std_global;
use crate::js::data::util::s_pool;
use crate::*;
use std::io::Write;
use std::rc::Rc;
use std::time::Instant;
use swc_common::errors::{DiagnosticBuilder, Emitter};
use swc_common::sync::Lrc;
use swc_common::{errors::Handler, FileName, SourceMap};
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
    let source = "
function a() {
    console.log('A called');
}

a()

";
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

    let global_access = empty_var_access(None, false);
    let module = parse_module(
        module,
        empty_var_access(Some(global_access.clone()), false),
        Rc::new(source.into()),
    );
    let mut stack = js::data::execution_v2::Stack::create_stack(
        build_function(module, s_pool(source)),
        build_std_global(),
    );

    let mut steps: usize = 0;
    let mut target = String::new();
    println!("Press enter to start debugging or type 'run' to complete the program...");
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
        if target.as_str() == "timed" {
            let before = Instant::now();
            steps += run_stack(&mut stack, 100000000, false);
            println!("{}ms", (Instant::now() - before).as_millis());
            break;
        }
        if target.as_str() == "run" {
            steps += run_stack(&mut stack, 100000000, false);
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
