/*pub trait JsValueTrait {
    fn to_native_string(&self) -> String;

    fn call(&self) -> dyn JsValue;

    fn lookup(&self, key: &dyn JsValue) -> dyn JsValue;

    fn type_of(&self) -> dyn JsValue;
}*/

use gc::{Finalize, Gc, GcCell, Trace};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Trace, Finalize)]
pub struct JsProperty {
    pub enumerable: bool,
    pub configurable: bool,
    pub writable: bool,
    pub value: Gc<GcCell<JsValue>>,
}

pub enum ExecResult {
    Done,
    Errored,
    Stepped(Box<dyn JsNext>),
}

pub trait JsNext {
    fn step(self, n: u64) -> ExecResult;
}

pub fn next_done() -> Box<dyn JsNext> {
    struct DoneNext {}
    impl JsNext for DoneNext {
        fn step(self, n: u64) -> ExecResult {
            ExecResult::Done
        }
    }
    Box::new(DoneNext {})
}

pub fn next_err() -> Box<dyn JsNext> {
    struct ErrNext {}
    impl JsNext for ErrNext {
        fn step(self, n: u64) -> ExecResult {
            ExecResult::Errored
        }
    }
    Box::new(ErrNext {})
}

#[derive(Trace, Finalize, Clone)]
pub enum JSCallable {
    NotCallable,
    Js { content: Rc<String> },
    Native {},
}

#[derive(Trace, Finalize, Clone)]
pub enum JsValue {
    Undefined,
    Null,
    Number(f64),
    Boolean(bool),
    String(Rc<String>),
    Object {
        is_array: bool,
        content: Gc<GcCell<HashMap<Rc<String>, JsProperty>>>,
        call: GcCell<JSCallable>,
    },
}

impl JsValue {
    pub fn to_system_string(&self) -> Rc<String> {
        match self {
            JsValue::Undefined => "undefined".into(),
            JsValue::Null => "null".into(),
            JsValue::Number(n) => n.to_string(),
            JsValue::Boolean(b) => b.to_string(),
            JsValue::String(s) => s.to_string(),
            JsValue::Object { .. } => "[object Object]".into(),
        }
        .into()
    }

    pub fn truthy(&self) -> bool {
        return match self {
            JsValue::Undefined => false,
            JsValue::Null => false,
            JsValue::Number(n) => *n == 0.0 || n.is_nan(),
            JsValue::Boolean(b) => *b,
            JsValue::String(s) => !s.is_empty(),
            JsValue::Object { .. } => true,
        };
    }
}
