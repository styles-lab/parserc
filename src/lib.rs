//! A fast and simple rust parser combinator framework, ***originally developed for `stylang` development***.

#![cfg_attr(docsrs, feature(doc_cfg))]

mod errors;
pub use errors::*;

mod input;
pub use input::*;

mod parser;
pub use parser::*;

#[cfg(feature = "derive")]
#[cfg_attr(docsrs, doc(cfg(feature = "derive")))]
mod parse;
#[cfg(feature = "derive")]
pub use parse::*;
