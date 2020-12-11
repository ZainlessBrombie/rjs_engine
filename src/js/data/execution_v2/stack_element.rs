use crate::js::data::execution_v2::function::FunctionExecution;
use crate::js::data::execution_v2::var::JsVar;
use crate::js::data::js_types::{Identity, JsValue};

pub struct FunctionHead {
    pub prev_function: usize,
    pub execution: FunctionExecution,
}

pub enum StackElement {
    HeapVar(JsVar),
    Value(JsValue),
    FunctionHead(FunctionHead),
    /// We want to be able to own Elements, so that be can own the current execution.
    Borrowed,
}

impl Default for StackElement {
    fn default() -> Self {
        StackElement::Borrowed
    }
}

impl StackElement {
    pub fn assume_head(&mut self) -> FunctionHead {
        match std::mem::take(self) {
            StackElement::FunctionHead(mut head) => head,
            _ => {
                panic!("Element is not a FunctionHead")
            }
        }
    }

    pub fn place_head(&mut self, mut head: FunctionHead) {
        std::mem::swap(self, &mut StackElement::FunctionHead(head))
    }

    pub fn assume_value(&self) -> JsValue {
        match self {
            StackElement::Value(v) => v.clone(),
            _ => {
                panic!("Assumed value to be at this stack location. There was not.")
            }
        }
    }
}
