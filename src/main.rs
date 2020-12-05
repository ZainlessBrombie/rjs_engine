#![feature(or_patterns)]
#![feature(in_band_lifetimes)]
#![allow(dead_code)]
//#[macro_use]
extern crate swc_common;
mod js;

fn main() {
    js::data::js_code::m1();
}
