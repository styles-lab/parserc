use std::fmt::Debug;

/// Error type to control combinator parsing procedure.
#[derive(Debug, thiserror::Error, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum ControlFlow<E>
where
    E: Clone + Debug,
{
    #[error("combinator report a recoverable error. {0}")]
    Recoverable(Option<E>),
    #[error("combinator report that reached the end of the source code. {0}")]
    Incomplete(Option<E>),
    #[error("combinator report a fatal error. {0}")]
    Fatal(Option<E>),
}

/// `Result` type that used by parser combinator.
pub type Result<T, E> = std::result::Result<T, ControlFlow<E>>;
