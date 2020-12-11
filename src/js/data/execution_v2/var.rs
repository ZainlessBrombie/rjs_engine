use crate::js::data::js_types::JsValue;
use safe_gc::{Gc, GcCell};
use std::rc::Rc;

/// A variable that sits on the heap
/// Since it is not associated with an execution that has opcodes with meta,
/// we want to keep a name.
#[derive(Clone)]
pub struct JsVar {
    pub(crate) name: Rc<String>,
    pub(crate) value: Gc<GcCell<JsValue>>,
}

impl JsVar {
    pub fn set(&self, val: JsValue) {
        *self.value.borrow().borrow_mut() = val;
    }

    pub fn get(&self) -> JsValue {
        return self.value.borrow().borrow_mut().clone();
    }
}
