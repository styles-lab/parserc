//! This mod provides error types/traits used by this crate.

use std::fmt::Debug;

use crate::span::Span;

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ErrorKind<P> {
    #[error("next")]
    Next(Span<P>),
    #[error("next_if")]
    NextIf(Span<P>),
    #[error("keyword")]
    Keyword(Span<P>),
    #[error("take_until")]
    TakeUntil(Span<P>),
    #[error("token")]
    Token(&'static str, Span<P>),
}

/// Diagnosis error type that returns by `parsers` should implement this trait.
pub trait ParseError<P>: From<ErrorKind<P>> {}

impl<P> ParseError<P> for ErrorKind<P> {}

/// A [`ParseError`] wrapper type that control the parsing flow.
#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ControlFlow<E> {
    /// A fatal error must broke the parsing process.
    Fatal(E),
    /// A recovable error generally lead to a retrospective parsing process.
    Recovable(E),
    /// This error means that the parsing process failed because it reached the end of the input stream.
    Incomplete(E),
}

/// `Result` type used by `parserc`
pub type Result<T, I, E> = std::result::Result<(T, I), ControlFlow<E>>;
