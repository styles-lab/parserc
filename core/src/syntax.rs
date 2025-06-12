//! Types/Traits to support build syntax tree(struct/enum)

use std::marker::PhantomData;

use parserc_derive::def_tuple_syntax;

use crate::{
    errors::{ParseError, Result},
    inputs::Input,
    parser::Parser,
};

pub use parserc_derive::Syntax;

#[cfg(feature = "token")]
pub use parserc_derive::tokens;

struct SyntaxParser<S, E, T>(PhantomData<S>, PhantomData<E>, PhantomData<T>);

impl<I, E, T> Parser<I> for SyntaxParser<I, E, T>
where
    E: ParseError<Input = I>,
    I: Input,
    T: Syntax<I, E>,
{
    type Error = E;

    type Output = T;

    fn parse(self, input: I) -> Result<Self::Output, I, Self::Error> {
        T::parse(input)
    }
}

/// A syntax tree struct/enum should implment this trait
pub trait Syntax<I, E>: Sized
where
    I: Input,
    E: ParseError<Input = I>,
{
    /// Parse input data and construct a new `Syntax` instance.
    fn parse(input: I) -> Result<Self, I, E>;

    /// Create a new `Parser` from this type.
    fn into_parser() -> impl Parser<I, Output = Self, Error = E> {
        SyntaxParser(Default::default(), Default::default(), Default::default())
    }
}

impl<T, I, E> Syntax<I, E> for PhantomData<T>
where
    I: Input,
    E: ParseError<Input = I>,
{
    fn parse(input: I) -> Result<Self, I, E> {
        Ok((Self::default(), input))
    }
}

impl<T, I, E> Syntax<I, E> for Option<T>
where
    T: Syntax<I, E>,
    I: Input + Clone,
    E: ParseError<Input = I>,
{
    fn parse(input: I) -> Result<Self, I, E> {
        T::into_parser().ok().parse(input)
    }
}

impl<T, I, E> Syntax<I, E> for Box<T>
where
    T: Syntax<I, E>,
    I: Input + Clone,
    E: ParseError<Input = I>,
{
    fn parse(input: I) -> Result<Self, I, E> {
        T::into_parser().boxed().parse(input)
    }
}

def_tuple_syntax!();

/// An extension trait that adds `parse` func to `Input`.
pub trait SyntaxEx: Input {
    /// Parse a specific `Syntax` type.
    fn parse<S, E>(self) -> Result<S, Self, E>
    where
        Self: Sized,
        S: Syntax<Self, E>,
        E: ParseError<Input = Self>,
    {
        S::parse(self)
    }

    /// Parse a specific `Syntax` type.
    fn ensure_parse<S, E>(self) -> Result<S, Self, E>
    where
        Self: Sized,
        S: Syntax<Self, E>,
        E: ParseError<Input = Self>,
    {
        S::into_parser().fatal().parse(self)
    }
}

impl<I> SyntaxEx for I where I: Input {}

#[cfg(test)]
mod tests {
    use crate::{
        errors::{ErrorKind, ParseError},
        inputs::Input,
        syntax::Syntax,
    };

    struct Mock;

    impl<I, E> Syntax<I, E> for Mock
    where
        I: Input,
        E: ParseError<Input = I>,
    {
        fn parse(input: I) -> crate::errors::Result<Self, I, E> {
            Ok((Mock, input))
        }
    }

    #[test]
    fn test_tuple() {
        <(Mock, Mock) as Syntax<_, ErrorKind<_>>>::parse("hello").unwrap();
    }
}
