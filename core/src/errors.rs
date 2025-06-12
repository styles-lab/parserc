//! This mod provides error types/traits used by this crate.

use std::{error::Error, fmt::Debug};

use crate::inputs::Input;

/// Diagnosis error type that returns by `parsers` should implement this trait.
pub trait ParseError: Error + Debug + PartialEq {
    type Input: Input + PartialEq + Debug;

    /// Error raised by parser [`next`](crate::parser::next).
    fn expect_next(item: <Self::Input as Input>::Item, input: Self::Input) -> Self;

    /// Error raised by parser [`next_if`](crate::parser::next_if).
    fn expect_next_if(diagnosis: &'static str, input: Self::Input) -> Self;

    /// Error raised by parser [`take_util`](crate::parser::take_until).
    fn expect_find<K: Debug>(keyword: K, input: Self::Input) -> Self;

    /// Error raised by parser [`take_util`](crate::parser::take_until).
    fn expect_start_with<K: Debug>(keyword: K, input: Self::Input) -> Self;

    /// Error raised by `Syntax`.
    fn expect_token(diagnosis: &'static str, input: Self::Input) -> Self;
}

#[derive(Debug, PartialEq, thiserror::Error)]
pub enum ErrorKind<I>
where
    I: Input,
{
    #[error("expect next({0:?}), ({1})")]
    Next(I::Item, String),

    #[error("expect next_if({0:?}), ({1})")]
    NextIf(&'static str, String),

    #[error("not found ({0}), ({1})")]
    Find(String, String),

    #[error("expect start with({0}), ({1})")]
    StartWith(String, String),

    #[error("expect token({0}), ({1})")]
    Token(&'static str, String),
}

impl<I> ParseError for ErrorKind<I>
where
    I: Input + PartialEq + Debug,
{
    type Input = I;

    fn expect_next(item: <Self::Input as Input>::Item, input: Self::Input) -> Self {
        Self::Next(item, input.debug())
    }

    fn expect_next_if(diagnosis: &'static str, input: Self::Input) -> Self {
        Self::NextIf(diagnosis, input.debug())
    }

    fn expect_find<K: Debug>(keyword: K, input: Self::Input) -> Self {
        Self::Find(format!("{:?}", keyword), input.debug())
    }

    fn expect_start_with<K: Debug>(keyword: K, input: Self::Input) -> Self {
        Self::StartWith(format!("{:?}", keyword), input.debug())
    }

    fn expect_token(diagnosis: &'static str, input: Self::Input) -> Self {
        Self::Token(diagnosis, input.debug())
    }
}

/// A [`ParseError`] wrapper type that control the parsing flow.
#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ControlFlow<E>
where
    E: ParseError,
{
    /// A fatal error must broke the parsing process.
    Fatal(E),
    /// A recovable error generally lead to a retrospective parsing process.
    Recovable(E),
    /// This error means that the parsing process failed because it reached the end of the input stream.
    Incomplete(E),
}

/// `Result` type used by `parserc`
pub type Result<T, I, E> = std::result::Result<(T, I), ControlFlow<E>>;
