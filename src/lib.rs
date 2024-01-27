#![feature(ascii_char)]

use std::rc::Rc;

#[cfg(test)]
#[macro_use]
extern crate test_case;

extern crate thiserror;

pub mod pff2;
pub mod theme_txt;

pub type OwnedSlice<T> = Rc<T>;
