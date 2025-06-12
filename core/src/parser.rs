//! Terminator/Non-terminator parsing combinators

use std::fmt::Debug;

use crate::{
    errors::{ControlFlow, ParseError, Result},
    inputs::{Find, Input, Item, StartWith},
};

/// A parsing combinator should implement this trait.
pub trait Parser<I>
where
    I: Input,
{
    type Output;
    /// error type returns by this `Parser`.
    type Error: ParseError<Input = I>;

    /// Consumes itself and parses the input stream to generate the `output` product.
    fn parse(self, input: I) -> Result<Self::Output, I, Self::Error>;

    /// Creates a new parser that converts `non-fatal` error into `None` value.
    fn ok(self) -> impl Parser<I, Output = Option<Self::Output>, Error = Self::Error>
    where
        I: Clone,
        Self: Sized,
    {
        IsOk(self)
    }

    /// On success, use func `F` to convert origin output to type `O`
    fn map<F, O>(self, f: F) -> impl Parser<I, Output = O, Error = Self::Error>
    where
        F: FnOnce(Self::Output) -> O,
        Self: Sized,
    {
        Map(self, f)
    }

    /// On failed, use func `F` to convert error into another type `E`.
    fn map_err<F, E>(self, f: F) -> impl Parser<I, Output = Self::Output, Error = E>
    where
        I: Clone,
        F: FnOnce(Self::Error) -> E,
        Self: Sized,
        E: ParseError<Input = I>,
    {
        MapErr(self, f)
    }

    /// Creates a parser that convert all `non-fatal` error into [`fatal`](ControlFlow::Fatal) error.
    fn fatal(self) -> impl Parser<I, Output = Self::Output, Error = Self::Error>
    where
        Self: Sized,
    {
        Fatal(self)
    }

    /// Map output into `Box<Self::Output>`, this func is short for code `Parser::map(|v|Box::new(v))`
    fn boxed(self) -> impl Parser<I, Output = Box<Self::Output>, Error = Self::Error>
    where
        Self: Sized,
    {
        self.map(|v| Box::new(v))
    }

    /// Executre another `Parser` if this one returns a `non-fatal` error.
    fn or<R>(self, parser: R) -> impl Parser<I, Output = Self::Output, Error = Self::Error>
    where
        I: Clone,
        R: Parser<I, Output = Self::Output, Error = Self::Error>,
        Self: Sized,
    {
        Or(self, parser)
    }
}

/// Implement [`Parser`] for all `FnOnce(I) -> Result<O, I, E>`
impl<O, I, E, F> Parser<I> for F
where
    I: Input,
    F: FnOnce(I) -> Result<O, I, E>,
    E: ParseError<Input = I>,
{
    type Output = O;
    type Error = E;

    fn parse(self, input: I) -> Result<Self::Output, I, Self::Error> {
        self(input)
    }
}

struct IsOk<P>(P);

impl<P, I> Parser<I> for IsOk<P>
where
    I: Input + Clone,
    P: Parser<I>,
{
    type Output = Option<P::Output>;

    type Error = P::Error;

    fn parse(self, input: I) -> Result<Self::Output, I, Self::Error> {
        // for retrospective analysis, we clone the input stream.
        match self.0.parse(input.clone()) {
            Err(ControlFlow::Fatal(err)) => Err(ControlFlow::Fatal(err)),
            Err(ControlFlow::Recovable(_)) | Err(ControlFlow::Incomplete(_)) => Ok((None, input)),
            Ok((t, input)) => Ok((Some(t), input)),
        }
    }
}

struct Map<P, F>(P, F);

impl<P, I, F, O> Parser<I> for Map<P, F>
where
    I: Input,
    P: Parser<I>,
    F: FnOnce(P::Output) -> O,
{
    type Output = O;
    type Error = P::Error;

    fn parse(self, input: I) -> Result<Self::Output, I, Self::Error> {
        self.0
            .parse(input)
            .map(|(output, input)| ((self.1)(output), input))
    }
}

struct MapErr<P, F>(P, F);

impl<P, I, F, E> Parser<I> for MapErr<P, F>
where
    I: Input,
    P: Parser<I>,
    F: FnOnce(P::Error) -> E,
    E: ParseError<Input = I>,
{
    type Output = P::Output;
    type Error = E;

    fn parse(self, input: I) -> Result<Self::Output, I, Self::Error> {
        self.0
            .parse(input)
            .map_err(|control_flow| match control_flow {
                ControlFlow::Fatal(err) => ControlFlow::Fatal((self.1)(err)),
                ControlFlow::Recovable(err) => ControlFlow::Recovable((self.1)(err)),
                ControlFlow::Incomplete(err) => ControlFlow::Incomplete((self.1)(err)),
            })
    }
}

struct Fatal<P>(P);

impl<P, I> Parser<I> for Fatal<P>
where
    I: Input,
    P: Parser<I>,
{
    type Output = P::Output;

    type Error = P::Error;

    fn parse(self, input: I) -> Result<Self::Output, I, Self::Error> {
        match self.0.parse(input) {
            Err(ControlFlow::Recovable(e)) | Err(ControlFlow::Incomplete(e)) => {
                Err(ControlFlow::Fatal(e))
            }
            r => r,
        }
    }
}

struct Or<L, R>(L, R);

impl<L, R, I, O, E> Parser<I> for Or<L, R>
where
    I: Input + Clone,
    E: ParseError<Input = I>,
    L: Parser<I, Output = O, Error = E>,
    R: Parser<I, Output = O, Error = E>,
{
    type Output = O;

    type Error = E;

    fn parse(self, input: I) -> Result<Self::Output, I, Self::Error> {
        if let (Some(v), input) = self.0.ok().parse(input.clone())? {
            return Ok((v, input));
        }

        self.1.parse(input)
    }
}

/// A parser match next item, otherwise raise an error.
pub const fn next<I, E>(item: I::Item) -> impl Parser<I, Output = I, Error = E>
where
    I: Input,
    E: ParseError<Input = I>,
{
    move |mut input: I| {
        if let Some(next) = input.iter().next() {
            if next == item {
                return Ok((input.split_to(item.len()), input));
            }

            Err(ControlFlow::Recovable(E::expect_next(item, input)))
        } else {
            Err(ControlFlow::Incomplete(E::expect_next(item, input)))
        }
    }
}

/// A parser match next item by `F`, otherwise raise an error.
pub const fn next_if<I, E, F>(
    diagnosis: &'static str,
    f: F,
) -> impl Parser<I, Output = I, Error = E>
where
    I: Input,
    E: ParseError<Input = I>,
    F: FnOnce(I::Item) -> bool,
{
    move |mut input: I| {
        if let Some(next) = input.iter().next() {
            if f(next) {
                return Ok((input.split_to(next.len()), input));
            }

            Err(ControlFlow::Recovable(E::expect_next_if(diagnosis, input)))
        } else {
            Err(ControlFlow::Incomplete(E::expect_next_if(diagnosis, input)))
        }
    }
}

/// Recogonize a keyword
#[inline]
pub fn keyword<KW, I, E>(keyword: KW) -> impl Parser<I, Output = I, Error = E>
where
    I: Input + StartWith<KW>,
    E: ParseError<Input = I>,
    KW: Debug + Clone,
{
    move |mut input: I| {
        if let Some(len) = input.starts_with(keyword.clone()) {
            Ok((input.split_to(len), input))
        } else {
            Err(ControlFlow::Recovable(E::expect_start_with(keyword, input)))
        }
    }
}

/// Returns the input slice up to the first occurrence of the keyword.
///
/// If the pattern is never found, returns [`ControlFlow::Incomplete`] error.
pub fn take_until<I, E, K>(keyword: K) -> impl Parser<I, Output = I, Error = E>
where
    K: Debug + Clone,
    I: Input + Find<K>,
    E: ParseError<Input = I>,
{
    move |mut input: I| {
        if let Some(offset) = input.find(keyword.clone()) {
            return Ok((input.split_to(offset), input));
        }

        return Err(ControlFlow::Incomplete(E::expect_find(keyword, input)));
    }
}

/// Returns the longest input slice (if any) that the predicate `F` returns true.
///
/// This parser will never returns an error.
pub fn take_while<I, E, F>(mut cond: F) -> impl Parser<I, Output = I, Error = E>
where
    I: Input,
    E: ParseError<Input = I>,
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
///
/// This parser is a short for `take_while(move |c: I::Item| !cond(c))`.
#[inline(always)]
pub fn take_till<I, E, F>(mut cond: F) -> impl Parser<I, Output = I, Error = E>
where
    I: Input,
    E: ParseError<Input = I>,
    F: FnMut(I::Item) -> bool,
{
    take_while(move |c: I::Item| !cond(c))
}

#[cfg(test)]
mod tests {
    use crate::{
        errors::{ControlFlow, ErrorKind},
        parser::{Parser, next},
    };

    #[test]
    fn test_next() {
        assert_eq!(next::<_, ErrorKind<_>>('c').parse("c"), Ok(("c", "")));
        assert_eq!(
            next('c').parse("a"),
            Err(ControlFlow::Recovable(ErrorKind::Next(
                'c',
                "a".to_string()
            )))
        );
        assert_eq!(
            next('c').parse(""),
            Err(ControlFlow::Incomplete(ErrorKind::Next(
                'c',
                "".to_string()
            )))
        );
    }
}
