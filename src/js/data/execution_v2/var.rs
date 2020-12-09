use std::rc::Rc;
use safe_gc::{Gc, GcCell};
use crate::js::data::js_types::JsValue;


/// A variable that sits on the heap
/// Since it is not associated with an execution that has opcodes with meta,
/// we want to keep a name.
pub struct JsVar {
    name: Rc<String>,
    value: Gc<GcCell<JsValue>>,
}

impl JsVar {
    pub fn set(&self, val: JsValue) {
        *value.borrow().borrow_mut() = val;
    }

    pub fn get(&self) -> JsValue {
        return self.value.borrow().borrow_mut().clone();
    }
}
