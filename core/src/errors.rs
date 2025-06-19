//! This mod provides error types/traits used by this crate.

use std::fmt::Debug;

use crate::inputs::Input;

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ErrorKind {
    #[error("next")]
    Next,
    #[error("next_if")]
    NextIf,
    #[error("keyword")]
    Keyword,
    #[error("take_until")]
    TakeUntil,
    #[error("token")]
    Token,
}

/// Diagnosis error type that returns by `parsers` should implement this trait.
pub trait ParseError: From<ErrorKind> + Debug + PartialEq {
    fn expect_token<I: Input>(name: &'static str, input: I) -> Self;
}

impl ParseError for ErrorKind {
    fn expect_token<I: Input>(_: &'static str, _: I) -> Self {
        Self::Token
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
