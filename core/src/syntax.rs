//! Types/Traits to support build syntax tree(struct/enum)

use std::{fmt::Debug, marker::PhantomData};

use parserc_derive::def_tuple_syntax;

use crate::{
    errors::{ParseError, Result},
    input::Input,
    parser::Parser,
    span::{Span, ToSpan},
};

pub use parserc_derive::Syntax;

#[cfg(feature = "token")]
pub use parserc_derive::tokens;

/// A syntax tree struct/enum should implment this trait
pub trait Syntax<I, E>: Sized
where
    I: Input,
    E: ParseError<I::Position>,
{
    /// Parse input data and construct a new `Syntax` instance.
    fn parse(input: I) -> Result<Self, I, E>;

    /// Create a new `Parser` from this type.
    fn into_parser() -> impl Parser<I, Output = Self, Error = E> {
        SyntaxParser(Default::default(), Default::default(), Default::default())
    }
}

struct SyntaxParser<S, E, T>(PhantomData<S>, PhantomData<E>, PhantomData<T>);

impl<I, E, T> Parser<I> for SyntaxParser<I, E, T>
where
    I: Input,
    E: ParseError<I::Position>,
    T: Syntax<I, E>,
{
    type Error = E;

    type Output = T;

    fn parse(self, input: I) -> Result<Self::Output, I, Self::Error> {
        T::parse(input)
    }
}

impl<T, I, E> Syntax<I, E> for PhantomData<T>
where
    I: Input,
    E: ParseError<I::Position>,
{
    fn parse(input: I) -> Result<Self, I, E> {
        Ok((Self::default(), input))
    }
}

impl<T, I, E> Syntax<I, E> for Option<T>
where
    T: Syntax<I, E>,
    I: Input + Clone,
    E: ParseError<I::Position>,
{
    fn parse(input: I) -> Result<Self, I, E> {
        T::into_parser().ok().parse(input)
    }
}

impl<T, I, E> Syntax<I, E> for Box<T>
where
    T: Syntax<I, E>,
    I: Input + Clone,
    E: ParseError<I::Position>,
{
    fn parse(input: I) -> Result<Self, I, E> {
        T::into_parser().boxed().parse(input)
    }
}

impl<T, I, E> Syntax<I, E> for Vec<T>
where
    T: Syntax<I, E>,
    I: Input + Clone,
    E: ParseError<I::Position>,
{
    fn parse(mut input: I) -> Result<Self, I, E> {
        let mut elms = vec![];
        loop {
            let elm;
            (elm, input) = T::into_parser().ok().parse(input)?;

            let Some(elm) = elm else {
                break;
            };

            elms.push(elm);
        }

        Ok((elms, input))
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
        E: ParseError<Self::Position>,
    {
        S::parse(self)
    }

    /// Parse a specific `Syntax` type.
    fn ensure_parse<S, E>(self) -> Result<S, Self, E>
    where
        Self: Sized,
        S: Syntax<Self, E>,
        E: ParseError<Self::Position>,
    {
        S::into_parser().fatal().parse(self)
    }
}

impl<I> SyntaxEx for I where I: Input {}

/// A short syntax for grouping token that surrounds a syntax body.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Delimiter<Start, End, Body> {
    /// Syntax start token.
    pub start: Start,
    /// Syntax end token.
    pub end: End,
    /// Syntax body.
    pub body: Body,
}

impl<I, E, Start, End, Body> Syntax<I, E> for Delimiter<Start, End, Body>
where
    I: Input,
    E: ParseError<I::Position>,
    Start: Syntax<I, E>,
    End: Syntax<I, E>,
    Body: Syntax<I, E>,
{
    fn parse(input: I) -> Result<Self, I, E> {
        let (start, input) = Start::parse(input)?;
        let (body, input) = Body::into_parser().fatal().parse(input)?;
        let (end, input) = End::into_parser().fatal().parse(input)?;

        Ok((Self { start, body, end }, input))
    }
}

impl<Start, End, Body, Position> ToSpan<Position> for Delimiter<Start, End, Body>
where
    Start: ToSpan<Position>,
    End: ToSpan<Position>,
    Body: ToSpan<Position>,
    Position: PartialOrd,
{
    fn to_span(&self) -> Span<Position> {
        self.start.to_span() ^ self.body.to_span() ^ self.end.to_span()
    }
}

/// A punctuated sequence of syntax tree nodes of type T separated by punctuation of type P.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Punctuated<T, P> {
    /// (T,P) pairs
    pub pairs: Vec<(T, P)>,
    /// individual tail `T`
    pub tail: Option<Box<T>>,
}

impl<T, P, I, E> Syntax<I, E> for Punctuated<T, P>
where
    T: Syntax<I, E>,
    P: Syntax<I, E>,
    E: ParseError<I::Position>,
    I: Input + Clone,
{
    fn parse(mut input: I) -> Result<Self, I, E> {
        let mut pairs = vec![];

        loop {
            let t;
            (t, input) = T::into_parser().ok().parse(input)?;

            let Some(t) = t else {
                return Ok((Self { pairs, tail: None }, input));
            };

            let p;
            (p, input) = P::into_parser().ok().parse(input)?;

            let Some(p) = p else {
                return Ok((
                    Self {
                        pairs,
                        tail: Some(Box::new(t)),
                    },
                    input,
                ));
            };

            pairs.push((t, p));
        }
    }
}

impl<T, P, Position> ToSpan<Position> for Punctuated<T, P>
where
    T: ToSpan<Position>,
    P: ToSpan<Position>,
    Position: PartialOrd,
{
    fn to_span(&self) -> Span<Position> {
        self.pairs.to_span() ^ self.tail.to_span()
    }
}

/// Use the parsed prefix to parse the syntax tree.
pub trait PartialSyntax<I, E, P>: Sized
where
    I: Input,
    E: ParseError<I::Position>,
{
    ///  Use the parsed prefix to parse the syntax tree.
    fn parse_with_prefix(prefix: P, input: I) -> Result<Self, I, E>;

    /// Create a new `Parser` with parsed prefix subtree.
    fn into_parser_with_prefix(prefix: P) -> impl Parser<I, Output = Self, Error = E> {
        PartialSyntaxParser(
            prefix,
            Default::default(),
            Default::default(),
            Default::default(),
        )
    }
}

struct PartialSyntaxParser<S, E, P, T>(P, PhantomData<S>, PhantomData<E>, PhantomData<T>);

impl<I, E, P, T> Parser<I> for PartialSyntaxParser<I, E, P, T>
where
    E: ParseError<I::Position>,
    I: Input,
    T: PartialSyntax<I, E, P>,
{
    type Error = E;

    type Output = T;

    fn parse(self, input: I) -> Result<Self::Output, I, Self::Error> {
        T::parse_with_prefix(self.0, input)
    }
}

#[cfg(test)]
mod tests {
    use crate::{errors::ParseError, input::Input, syntax::Syntax};

    struct Mock;

    impl<I, E> Syntax<I, E> for Mock
    where
        I: Input,
        E: ParseError<I::Position>,
    {
        fn parse(input: I) -> crate::errors::Result<Self, I, E> {
            Ok((Mock, input))
        }
    }
}
