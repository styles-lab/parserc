use std::{fmt::Debug, num::NonZeroUsize};

/// Contains information on needed data if a parser returned Incomplete
#[derive(Debug, PartialEq, Eq)]
pub enum Needed {
    /// Needs more data, but we do not know how much
    Unknown,
    /// Contains the required data size in bytes
    Size(NonZeroUsize),
}

/// ControlFlow for parserc `Parser`.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum ControlFlow<E>
where
    E: Debug,
{
    /// indicates that more data is needed to decide. The Needed enum can contain how many additional bytes are necessary.
    /// If you are sure your parser is working on full data, you can wrap your parser with the complete combinator to
    /// transform that case in Error
    #[error("incomplete: {0:?}")]
    Incomplete(Needed),
    /// indicates an unrecoverable error. For example, when a prefix has been recognised and the next parser has been confirmed,
    /// if that parser fails, then the entire process fails; there are no more parsers to try.
    #[error("fatal: {0:?}")]
    Fatal(E),
    /// means some parser did not succeed, but another one might (as an example, when testing different branches of an `or` combinator)
    ///
    #[error("recovable: {0:?}")]
    Recovable(E),
}

/// parserc inner error kind.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum Kind {
    #[error("none")]
    None,
    #[error("keyword")]
    Keyword,
    #[error("char")]
    Char,
    #[error("byte")]
    Byte,
}

/// Result type used by `parserc`.
pub type Result<O, I, E> = std::result::Result<(O, I), ControlFlow<E>>;
