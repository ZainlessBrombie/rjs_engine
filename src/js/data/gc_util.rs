use gc::{Finalize, Trace};
use std::ops::{Deref, DerefMut};

#[derive(Clone, Trace, Finalize)]
pub struct GcDestr<T: 'static + Trace + Finalize> {
    state: DestroyState<T>,
}

impl<T: Finalize + Trace> Deref for GcDestr<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        if let DestroyState::There(ref o) = self.state {
            o
        } else {
            panic!("Cannot deref destroyed value")
        }
    }
}

impl<T: Trace + Finalize> DerefMut for GcDestr<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        if let DestroyState::There(ref mut o) = self.state {
            o
        } else {
            panic!("Cannot deref destroyed value")
        }
    }
}

#[derive(Finalize, Trace, Clone)]
pub enum DestroyState<T> {
    There(T),
    Destroyed,
}

impl<T: Finalize + Trace> Default for DestroyState<T> {
    fn default() -> Self {
        DestroyState::Destroyed
    }
}

impl<T: Finalize + Trace + 'static> From<T> for GcDestr<T> {
    fn from(o: T) -> Self {
        GcDestr {
            state: DestroyState::There(o),
        }
    }
}

impl<T: Trace + Finalize> GcDestr<T> {
    pub fn new(o: T) -> GcDestr<T> {
        return GcDestr::from(o);
    }

    pub fn destroy_move(&mut self) -> GcDestr<T> {
        GcDestr {
            state: std::mem::take(&mut self.state),
        }
    }
}
