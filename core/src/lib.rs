//! A fast and simple rust parser combinator framework, ***originally developed for `stylang` development***.

#![cfg_attr(docsrs, feature(doc_cfg))]

pub mod errors;
pub mod input;
pub mod lang;
pub mod parser;
pub mod span;

#[cfg(feature = "syntax")]
pub mod syntax;
