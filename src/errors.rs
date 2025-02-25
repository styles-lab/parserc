use std::fmt::Debug;

/// A trait that parserc error must implement.
pub trait ParseError: From<Kind> + Clone + Debug {}

/// Error type returns by parserc.
#[derive(Debug, thiserror::Error, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Kind {
    #[error("ensure_char")]
    EnsureChar,
    #[error("ensure_char_if")]
    EnsureCharIf,
    #[error("ensure_keyword")]
    EnsureKeyword,
}

impl ParseError for Kind {}

/// Error type to control combinator parsing procedure.
#[derive(Debug, thiserror::Error, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum ControlFlow<E>
where
    E: ParseError,
{
    #[error("combinator report a recoverable error. {0}")]
    Recoverable(E),
    #[error("combinator report that reached the end of the source code. {0}")]
    Incomplete(E),
    #[error("combinator report a fatal error. {0}")]
    Fatal(E),
}

impl<E> ControlFlow<E>
where
    E: ParseError,
{
    /// Convert `ControlFlow` into inner error.
    pub fn into_raw(self) -> E {
        match self {
            ControlFlow::Recoverable(e) => e,
            ControlFlow::Incomplete(e) => e,
            ControlFlow::Fatal(e) => e,
        }
    }
}

/// `Result` type that used by parser combinator.
pub type Result<T, E> = std::result::Result<T, ControlFlow<E>>;
