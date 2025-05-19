use std::{fmt::Debug, marker::PhantomData};

use memchr::memmem;

use crate::{AsBytes, ControlFlow, Input, Item, Kind, Result};

/// A parserc `Parser` must implement this trait.
pub trait Parser<I>
where
    I: Input,
{
    /// Output data type.
    type Output;
    /// Error kind of this parser type.
    type Error: From<Kind> + Debug;

    /// A parser takes in input type, and returns a Result containing either the remaining input and the output value, or an error.
    fn parse(&mut self, input: I) -> Result<Self::Output, I, Self::Error>;
}

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
        IntoParser(Default::default(), Default::default())
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
        P::into_parser().map(|v| Box::new(v)).parse(input)
    }
}

/// A parser wrapped from [`Parse`] type.
#[derive(Debug, Clone, Copy)]
pub struct IntoParser<I, P>(PhantomData<I>, PhantomData<P>);

impl<I, P> Parser<I> for IntoParser<I, P>
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

impl<O, I, E, F> Parser<I> for F
where
    F: FnMut(I) -> Result<O, I, E>,
    E: From<Kind> + Debug,
    I: Input,
{
    type Output = O;

    type Error = E;

    #[inline(always)]
    fn parse(&mut self, input: I) -> Result<Self::Output, I, Self::Error> {
        (*self)(input)
    }
}

/// An extension trait add combination fns for any [`Parser`]
pub trait ParserExt<I>: Parser<I> + Sized
where
    I: Input,
{
    /// Create an [`Optional`] parser from this parser.
    fn ok(self) -> impl Parser<I, Error = Self::Error, Output = Option<Self::Output>>
    where
        I: Clone,
    {
        Optional(self)
    }

    /// Create a [`Map`] parser from this parser.
    fn map<F, O>(self, map: F) -> impl Parser<I, Error = Self::Error, Output = O>
    where
        F: FnMut(Self::Output) -> O,
    {
        Map(self, map)
    }

    /// Create a parser that box this parser's output.
    fn boxed(self) -> impl Parser<I, Error = Self::Error, Output = Box<Self::Output>> {
        self.map(|v| Box::new(v))
    }

    /// Create a [`Map`] parser from this parser.
    fn or<R>(self, parser: R) -> impl Parser<I, Error = Self::Error, Output = Self::Output>
    where
        I: Clone,
        R: Parser<I, Error = Self::Error, Output = Self::Output>,
    {
        Or(self, parser)
    }

    /// Create a [`MapErr`] parser from this parser.
    fn map_err<F, E>(self, map_err: F) -> impl Parser<I, Error = E, Output = Self::Output>
    where
        F: FnMut(I, Self::Error) -> E,
        E: From<Kind> + Debug,
        I: Clone,
    {
        MapErr(self, map_err)
    }

    /// Create a [`MapControlFlow`] parser from this parser.
    fn map_control_flow<F>(self, f: F) -> MapControlFlow<Self, F> {
        MapControlFlow(self, f)
    }

    /// Convert [`ControlFlow::Recovable`] to [`ControlFlow::Fatal`]
    fn fatal(self) -> impl Parser<I, Output = Self::Output, Error = Self::Error> {
        self.map_control_flow(|c| match c {
            ControlFlow::Incomplete(e) | ControlFlow::Recovable(e) | ControlFlow::Fatal(e) => {
                ControlFlow::Fatal(e)
            }
        })
    }

    /// Convert [`ControlFlow::Fatal`] to [`ControlFlow::Recovable`]
    fn recovable(self) -> impl Parser<I, Output = Self::Output, Error = Self::Error> {
        self.map_control_flow(|c| match c {
            ControlFlow::Incomplete(needed) => ControlFlow::Incomplete(needed),
            ControlFlow::Recovable(e) | ControlFlow::Fatal(e) => ControlFlow::Fatal(e),
        })
    }
}

impl<I, P> ParserExt<I> for P
where
    I: Input,
    P: Parser<I>,
{
}

/// A parser that changes the [`ControlFlow`] from the inner parser to a new one.
///
/// This combinator does not change the real error type.
pub struct MapControlFlow<P, F>(P, F);

impl<I, P, F, E> Parser<I> for MapControlFlow<P, F>
where
    P: Parser<I>,
    E: From<Kind> + Debug,
    F: FnMut(ControlFlow<P::Error>) -> ControlFlow<E>,
    I: Input,
{
    type Error = E;
    type Output = P::Output;

    fn parse(&mut self, input: I) -> Result<Self::Output, I, Self::Error> {
        match self.0.parse(input) {
            Ok(v) => Ok(v),
            Err(c) => Err((self.1)(c)),
        }
    }
}

/// A parser that convert error `E` returns by inner parser to `E1`.
///
/// This combinator does not change the [`ControlFlow`]
pub struct MapErr<P, F>(P, F);

impl<I, P, F, E> Parser<I> for MapErr<P, F>
where
    P: Parser<I>,
    E: From<Kind> + Debug,
    F: FnMut(I, P::Error) -> E,
    I: Input + Clone,
{
    type Error = E;
    type Output = P::Output;

    fn parse(&mut self, input: I) -> Result<Self::Output, I, Self::Error> {
        match self.0.parse(input.clone()) {
            Ok(v) => Ok(v),
            Err(c) => match c {
                ControlFlow::Incomplete(e) => Err(ControlFlow::Incomplete((self.1)(input, e))),
                ControlFlow::Fatal(e) => Err(ControlFlow::Fatal((self.1)(input, e))),
                ControlFlow::Recovable(e) => Err(ControlFlow::Recovable((self.1)(input, e))),
            },
        }
    }
}

/// Tests two parsers one by one until one succeeds or all failed.
pub struct Or<L, R>(L, R);

impl<I, L, R, O, E> Parser<I> for Or<L, R>
where
    L: Parser<I, Error = E, Output = O>,
    R: Parser<I, Error = E, Output = O>,
    E: From<Kind> + Debug,
    I: Input + Clone,
{
    type Error = E;
    type Output = O;

    #[inline(always)]
    fn parse(&mut self, input: I) -> Result<Self::Output, I, Self::Error> {
        match self.0.parse(input.clone()) {
            Ok(v) => Ok(v),
            Err(ControlFlow::Recovable(_)) => self.1.parse(input),
            Err(err) => return Err(err),
        }
    }
}

/// A parser that convert inner parser's output to another type.
pub struct Map<P, F>(P, F);

impl<I, P, F, O> Parser<I> for Map<P, F>
where
    P: Parser<I>,
    I: Input,
    F: FnMut(P::Output) -> O,
{
    type Error = P::Error;
    type Output = O;

    #[inline(always)]
    fn parse(&mut self, input: I) -> Result<Self::Output, I, Self::Error> {
        match self.0.parse(input) {
            Ok((o, input)) => Ok(((self.1)(o), input)),
            Err(err) => Err(err),
        }
    }
}

/// A parser that convert [`recovable`](super::ControlFlow::Recovable) error from inner parser into [`None`] value.
pub struct Optional<P>(P);

impl<I, P> Parser<I> for Optional<P>
where
    P: Parser<I>,
    I: Input + Clone,
{
    type Error = P::Error;
    type Output = Option<P::Output>;

    #[inline(always)]
    fn parse(&mut self, input: I) -> Result<Self::Output, I, Self::Error> {
        match self.0.parse(input.clone()) {
            Err(ControlFlow::Recovable(_)) => return Ok((None, input)),
            Err(ControlFlow::Incomplete(_)) => return Ok((None, input)),
            Err(e) => return Err(e),
            Ok((o, input)) => Ok((Some(o), input)),
        }
    }
}

/// Recogonize a keyword
#[inline]
pub fn keyword<KW, I, E>(keyword: KW) -> impl Parser<I, Output = I, Error = E>
where
    I: Input + AsBytes,
    KW: Input + AsBytes,
    E: From<Kind> + Debug,
{
    move |mut input: I| {
        let len = keyword.len();
        if input.len() < len {
            return Err(ControlFlow::Recovable(Kind::Keyword.into()));
        }

        if &input.as_bytes()[..len] == keyword.as_bytes() {
            return Ok((input.split_to(len), input));
        } else {
            return Err(ControlFlow::Recovable(Kind::Keyword.into()));
        }
    }
}

/// Recognize next input item.
#[inline(always)]
pub const fn next<C, I, E>(c: C) -> impl Parser<I, Output = I, Error = E> + Clone
where
    I: Input<Item = C>,
    C: Item,
    E: From<Kind> + Debug,
{
    move |mut input: I| {
        if let Some(next) = input.iter().next() {
            if next == c {
                return Ok((input.split_to(c.len()), input));
            }
        }

        return Err(ControlFlow::Recovable(Kind::Next.into()));
    }
}

/// Recognize next inpput item.
#[inline(always)]
pub const fn satisfy<F, I, E>(mut predicate: F) -> impl Parser<I, Output = I, Error = E>
where
    F: FnMut(I::Item) -> bool,
    I: Input,
    E: From<Kind> + Debug,
{
    move |mut input: I| {
        if let Some(next) = input.iter().next() {
            if predicate(next) {
                return Ok((input.split_to(next.len()), input));
            }
        }

        return Err(ControlFlow::Recovable(Kind::Satisfy.into()));
    }
}

/// Returns the input slice up to the first occurrence of the pattern.
#[inline(always)]
pub fn take_until<KW, I, E>(keyword: KW) -> impl Parser<I, Output = I, Error = E>
where
    I: Input + AsBytes,
    KW: Input + AsBytes,
    E: From<Kind> + Debug,
{
    move |mut input: I| {
        let len = keyword.len();
        if input.len() < len {
            return Err(ControlFlow::Recovable(Kind::TakeUntil.into()));
        }

        if let Some(offset) = memmem::find(input.as_bytes(), keyword.as_bytes()) {
            return Ok((input.split_to(offset), input));
        }

        return Err(ControlFlow::Recovable(Kind::TakeUntil.into()));
    }
}

/// Returns the longest input slice (if any) that matches the predicate.
#[inline(always)]
pub fn take_while<I, E, F>(mut cond: F) -> impl Parser<I, Output = I, Error = E>
where
    I: Input,
    E: From<Kind> + Debug,
    F: FnMut(I::Item) -> bool,
{
    move |mut input: I| {
        let mut iter = input.iter();
        let mut offset = 0;
        loop {
            if let Some(next) = iter.next() {
                if !(cond)(next) {
                    break;
                }

                offset += next.len();
            } else {
                break;
            }
        }

        return Ok((input.split_to(offset), input));
    }
}

/// Returns the longest input slice (if any) till a predicate is met.
#[inline(always)]
pub fn take_till<I, E, F>(mut cond: F) -> impl Parser<I, Output = I, Error = E>
where
    I: Input,
    E: From<Kind> + Debug,
    F: FnMut(I::Item) -> bool,
{
    take_while(move |c: I::Item| !cond(c))
}

/// This module provides utilities to parse rust types.
pub mod rustypes {
    use super::*;

    impl<I> Parse<I> for bool
    where
        I: Input + AsBytes + Clone,
    {
        type Error = Kind;

        fn parse(input: I) -> Result<Self, I, Self::Error> {
            keyword("true")
                .map(|_| true)
                .or(keyword("false").map(|_| false))
                .parse(input)
        }
    }
}

#[cfg(feature = "derive")]
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

#[cfg(test)]
mod tests {

    use crate::{
        ControlFlow, Kind, Parser, Result, keyword, next, take_till, take_until, take_while,
    };

    use super::{Parse, ParserExt};

    fn mock_recovable(_: &str) -> Result<&str, &str, Kind> {
        Err(ControlFlow::Recovable(Kind::None))
    }

    #[test]
    fn test_opt() {
        assert_eq!(
            mock_recovable.ok().parse("hello world"),
            Ok((None, "hello world"))
        );
    }

    #[test]
    fn test_ensure_keyword() {
        assert_eq!(
            keyword::<&str, &str, Kind>("hello").parse("hello world"),
            Ok(("hello", " world"))
        );

        assert_eq!(
            keyword("hello").parse(" world  "),
            Err(ControlFlow::Recovable(Kind::Keyword))
        );
    }

    #[test]
    fn test_char() {
        assert_eq!(
            next::<char, &str, Kind>('你').parse("你 world  "),
            Ok(("你", " world  "))
        );
    }

    #[test]
    fn test_byte() {
        assert_eq!(
            next::<u8, &[u8], Kind>(b'<').parse(b"<world  ".as_slice()),
            Ok((b"<".as_slice(), b"world  ".as_slice()))
        );
    }

    #[test]
    fn test_take_until() {
        assert_eq!(
            take_until::<&str, &[u8], Kind>("<!--").parse(b"<world  <!--".as_slice()),
            Ok((b"<world  ".as_slice(), b"<!--".as_slice()))
        );
    }

    #[test]
    fn test_map() {
        assert_eq!(bool::parse("false"), Ok((false, "")));

        assert_eq!(bool::parse("true"), Ok((true, "")));
    }

    #[test]
    fn test_take_while() {
        fn digit(input: &str) -> Result<&str, &str, Kind> {
            take_while(|c: char| c.is_ascii_digit()).parse(input)
        }

        assert_eq!(digit("123hello"), Ok(("123", "hello")));
        assert_eq!(digit("hello123"), Ok(("", "hello123")));
    }

    #[test]
    fn test_take_till() {
        fn digit(input: &str) -> Result<&str, &str, Kind> {
            take_till(|c: char| c.is_ascii_digit()).parse(input)
        }

        assert_eq!(digit("123hello"), Ok(("", "123hello")));
        assert_eq!(digit("hello123"), Ok(("hello", "123")));
    }

    #[test]
    fn test_take_till1() {
        assert_eq!(
            take_till::<&[u8], Kind, _>(|c| c == b'<').parse(b"\n".as_slice()),
            Err(ControlFlow::Incomplete(Kind::TakeWhile.into())),
        );
    }
}
