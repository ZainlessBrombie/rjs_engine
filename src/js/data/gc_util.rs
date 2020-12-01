use gc::{Finalize, GcCell, GcCellRefMut, Trace};
use std::ops::DerefMut;

#[derive(Clone, Trace, Finalize)]
pub struct GcDestr<T: 'static + Trace + Finalize> {
    state: GcCell<DestroyState<T>>,
}

#[derive(Finalize, Trace, Clone)]
enum DestroyState<T> {
    There(T),
    Destroyed,
}

impl<T: Finalize + Trace + 'static> From<T> for GcDestr<T> {
    fn from(o: T) -> Self {
        GcDestr {
            state: GcCell::new(DestroyState::There(o)),
        }
    }
}

impl<T: Trace + Finalize> GcDestr<T> {
    pub fn new(o: T) -> GcDestr<T> {
        return GcDestr::from(o);
    }

    pub fn destroy(&mut self) -> T {
        let mut prev = DestroyState::Destroyed;
        std::mem::swap(
            &mut prev,
            GcCellRefMut::deref_mut(&mut self.state.borrow_mut()),
        );
        if let DestroyState::There(o) = prev {
            return o;
        } else {
            panic!("Cannot destroy a destroyed gc value!")
        }
    }
}
