//! This mod provides error types/traits used by this crate.

use std::fmt::Debug;

use crate::span::{Span, ToSpan};

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

impl<P> ToSpan<P> for ErrorKind<P>
where
    P: Clone,
{
    fn to_span(&self) -> Span<P> {
        match self {
            ErrorKind::Next(span) => span.clone(),
            ErrorKind::NextIf(span) => span.clone(),
            ErrorKind::Keyword(span) => span.clone(),
            ErrorKind::TakeUntil(span) => span.clone(),
            ErrorKind::Token(_, span) => span.clone(),
        }
    }
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

/// An extension trait for `std::result::Result<(T, I), ControlFlow<E>>`
pub trait MapFatal {
    /// map underlying `ControlFlow`
    fn fatal(self) -> Self;
}

impl<T, I, E> MapFatal for Result<T, I, E> {
    fn fatal(self) -> Self {
        match self {
            Err(ControlFlow::Recovable(e)) | Err(ControlFlow::Incomplete(e)) => {
                Err(ControlFlow::Fatal(e))
            }
            _ => self,
        }
    }
}

/// An extension trait for `std::result::Result<(T, I), ControlFlow<E>>`
pub trait MapError<E1, E2> {
    type Output;
    /// map underlying `ControlFlow`
    fn map_control_flow_err<F>(self, f: F) -> Self::Output
    where
        F: FnOnce(E1) -> E2;
}

impl<T, I, E1, E2> MapError<E1, E2> for Result<T, I, E1> {
    type Output = Result<T, I, E2>;

    fn map_control_flow_err<F>(self, f: F) -> Self::Output
    where
        F: FnOnce(E1) -> E2,
    {
        match self {
            Err(ControlFlow::Fatal(err)) => Err(ControlFlow::Fatal(f(err))),
            Err(ControlFlow::Recovable(err)) => Err(ControlFlow::Recovable(f(err))),
            Err(ControlFlow::Incomplete(err)) => Err(ControlFlow::Incomplete(f(err))),
            Ok(r) => Ok(r),
        }
    }
}

/// An extension trait for `std::result::Result<(T, I), ControlFlow<E>>`
pub trait Map<T1, T2> {
    type Output;
    /// map underlying `ControlFlow`
    fn map_control_flow<F>(self, f: F) -> Self::Output
    where
        F: FnOnce(T1) -> T2;
}

impl<T1, T2, I, E> Map<T1, T2> for Result<T1, I, E> {
    type Output = Result<T2, I, E>;

    fn map_control_flow<F>(self, f: F) -> Self::Output
    where
        F: FnOnce(T1) -> T2,
    {
        match self {
            Ok((t, i)) => Ok((f(t), i)),
            Err(err) => Err(err),
        }
    }
}
