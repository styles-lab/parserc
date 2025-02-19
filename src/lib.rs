//! A fast and simple rust parser combinator framework, ***originally developed for `mlang` development***.
//!
//! Why develop `parserc` and not [`nom`](https://docs.rs/nom/7.1.3/nom/):
//!
//!

mod errors;
pub use errors::*;
mod input;
pub use input::*;
mod parser;
pub use parser::*;
