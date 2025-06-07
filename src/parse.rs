use std::{fmt::Debug, marker::PhantomData};

use crate::{Input, Kind, Parser, ParserExt, Result};

pub use parserc_derive::derive_parse;

/// All types that can be parsed from an input type should implement this trait.
pub trait Parse<I>: Sized
where
    I: Input,
{
    /// Error kind of this parser type.
    type Error: From<Kind> + Debug;

    /// Parse input type as `Self`.
    fn parse(input: I) -> Result<Self, I, Self::Error>;

    /// Convert [`Parse`] into [`Parser`].
    #[inline(always)]
    fn into_parser() -> impl Parser<I, Output = Self, Error = Self::Error> {
        AsParser(Default::default(), Default::default())
    }
}

parserc_derive::make_tuple_parse_impl!();

#[derive(Debug, Clone, Copy)]
struct AsParser<I, P>(PhantomData<I>, PhantomData<P>);

impl<I, P> Parser<I> for AsParser<I, P>
where
    P: Parse<I>,
    I: Input,
{
    type Error = P::Error;
    type Output = P;

    #[inline(always)]
    fn parse(&mut self, input: I) -> Result<Self::Output, I, Self::Error> {
        P::parse(input)
    }
}

impl<I, P> Parse<I> for Option<P>
where
    I: Input + Clone,
    P: Parse<I>,
{
    type Error = P::Error;

    fn parse(input: I) -> Result<Self, I, Self::Error> {
        P::into_parser().ok().parse(input)
    }
}

impl<I, P> Parse<I> for Box<P>
where
    I: Input + Clone,
    P: Parse<I>,
{
    type Error = P::Error;

    fn parse(input: I) -> Result<Self, I, Self::Error> {
        P::into_parser().boxed().parse(input)
    }
}

impl<I, P> Parse<I> for Vec<P>
where
    I: Input + Clone,
    P: Parse<I>,
{
    type Error = P::Error;

    fn parse(mut input: I) -> Result<Self, I, Self::Error> {
        let mut values = vec![];

        loop {
            let p;
            (p, input) = P::into_parser().ok().parse(input)?;

            if let Some(p) = p {
                values.push(p);
                continue;
            }

            return Ok((values, input));
        }
    }
}

/// A type that parse rest part from the input.
pub trait Partial<I>: Sized
where
    I: Input,
{
    /// Error kind of this parser type.
    type Error: From<Kind> + Debug;

    /// The parsed part of the type.
    type Parsed: Clone;

    /// Parse input type as `Self`.
    fn partial_parse(parsed: Self::Parsed, input: I) -> Result<Self, I, Self::Error>;

    /// Create a [`Parser`] with parsed part.
    #[inline(always)]
    fn into_partial_parser(
        parsed: Self::Parsed,
    ) -> impl Parser<I, Output = Self, Error = Self::Error> {
        PartialParser(parsed, Default::default(), Default::default())
    }
}

struct PartialParser<I, P, T>(P, PhantomData<T>, PhantomData<I>)
where
    I: Input;

impl<I, P, T> Parser<I> for PartialParser<I, P, T>
where
    P: Clone,
    T: Partial<I, Parsed = P>,
    I: Input,
{
    type Output = T;

    type Error = T::Error;

    fn parse(&mut self, input: I) -> Result<Self::Output, I, Self::Error> {
        T::partial_parse(self.0.clone(), input)
    }
}

impl<I, P, T> Partial<I> for Option<T>
where
    P: Clone,
    I: Input + Clone,
    T: Partial<I, Parsed = P>,
{
    type Error = T::Error;

    type Parsed = P;

    fn partial_parse(parsed: Self::Parsed, input: I) -> Result<Self, I, Self::Error> {
        T::into_partial_parser(parsed).ok().parse(input)
    }
}

impl<I, P, T> Partial<I> for Box<T>
where
    P: Clone,
    I: Input + Clone,
    T: Partial<I, Parsed = P>,
{
    type Error = T::Error;

    type Parsed = P;

    fn partial_parse(parsed: Self::Parsed, input: I) -> Result<Self, I, Self::Error> {
        T::into_partial_parser(parsed).boxed().parse(input)
    }
}

/// An extension trait that add parse function to `Input` trait.
pub trait ParseFromInput: Input + Sized {
    /// parse a new item from the input stream.
    fn parse<P: Parse<Self>>(self) -> Result<P, Self, P::Error> {
        P::parse(self)
    }

    /// Parse next item from the input stream, and converts the error that occurs to `ControlFlow::Fatal`.
    fn ensure_parse<P: Parse<Self>>(self) -> Result<P, Self, P::Error> {
        P::into_parser().fatal().parse(self)
    }
}

impl<I> ParseFromInput for I where I: Input {}

/// A punctuated sequence of syntax tree nodes of type T separated by punctuation of type P.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Punctuated<T, P> {
    /// (T,P) pairs
    pub pairs: Vec<(T, P)>,
    /// individual tail `T`
    pub tail: Option<Box<T>>,
}

impl<T, P, I, E> Parse<I> for Punctuated<T, P>
where
    T: Parse<I, Error = E>,
    P: Parse<I, Error = E>,
    E: From<Kind> + Debug,
    I: Input + Clone,
{
    type Error = E;

    fn parse(mut input: I) -> Result<Self, I, Self::Error> {
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

/// A grouping token `(Start,End)` that surrounds a type `B`
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Delimiter<Start, End, Body> {
    pub delimiter_start: Start,
    pub body: Body,
    pub delimiter_end: End,
}

impl<Start, End, Body, I, E> Parse<I> for Delimiter<Start, End, Body>
where
    Start: Parse<I, Error = E>,
    End: Parse<I, Error = E>,
    Body: Parse<I, Error = E>,
    E: From<Kind> + Debug,
    I: Input + Clone,
{
    type Error = E;

    fn parse(input: I) -> Result<Self, I, Self::Error> {
        let (delimiter_start, input) = Start::parse(input)?;
        let (body, input) = Body::into_parser().fatal().parse(input)?;
        let (delimiter_end, input) = End::into_parser().fatal().parse(input)?;

        Ok((
            Self {
                delimiter_start,
                body,
                delimiter_end,
            },
            input,
        ))
    }
}
