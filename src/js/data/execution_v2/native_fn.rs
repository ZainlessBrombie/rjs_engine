use crate::js::data::js_types::JsValue;
use safe_gc::Mark;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

pub trait NativeFunction: Mark {
    fn call(&self, this: JsValue, args: JsValue) -> Result<JsValue, JsValue>;
}

pub fn native_from<F: 'static>(f: F) -> Rc<dyn NativeFunction>
where
    F: Fn(JsValue, JsValue) -> Result<JsValue, JsValue>,
{
    struct NativeImpl<F1: Fn(JsValue, JsValue) -> Result<JsValue, JsValue> + ?Sized> {
        f: Rc<F1>,
    }
    impl<T: Fn(JsValue, JsValue) -> Result<JsValue, JsValue>> Mark for NativeImpl<T> {
        fn mark_all(&self) {
            unimplemented!()
        }

        fn unroot(&self) {
            unimplemented!()
        }

        fn root(&self) {
            unimplemented!()
        }
    }
    impl<T: Fn(JsValue, JsValue) -> Result<JsValue, JsValue>> NativeFunction for NativeImpl<T> {
        fn call(&self, this: JsValue, args: JsValue) -> Result<JsValue, JsValue> {
            (self.f)(this, args)
        }
    }
    Rc::new(NativeImpl { f: Rc::new(f) })
}

impl Debug for dyn NativeFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[native function]")
    }
}
