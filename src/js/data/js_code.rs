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
use swc_common::SourceMap;
use swc_ecma_parser::Parser;

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
    let source = "
function a() {
    console.log('A called');
}

a()

";

    let global_access = empty_var_access(None, false);
    let mut stack = js::data::execution_v2::Stack::create_stack(
        build_function(),
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
