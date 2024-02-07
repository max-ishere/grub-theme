use std::rc::Rc;

#[cfg(test)]
#[macro_use]
extern crate test_case;

#[macro_use]
extern crate log;

extern crate thiserror;

pub mod parser {
    pub mod pff2;
    pub mod theme_txt;
}

pub mod render {
    pub mod pff2;
}

pub type OwnedSlice<T> = Rc<T>;

trait Sealed {}
