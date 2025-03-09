//! A fast and simple rust parser combinator framework, ***originally developed for `mlang` development***.

mod errors;
pub use errors::*;

mod input;
pub use input::*;

mod parser;
pub use parser::*;
