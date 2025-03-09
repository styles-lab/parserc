use std::fmt::Debug;

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
    fn parse(self, input: I) -> Result<Self::Output, I, Self::Error>;
}

impl<O, I, E, F> Parser<I> for F
where
    F: FnOnce(I) -> Result<O, I, E>,
    E: From<Kind> + Debug,
    I: Input,
{
    type Output = O;

    type Error = E;

    fn parse(self, input: I) -> Result<Self::Output, I, Self::Error> {
        (self)(input)
    }
}

/// An extension trait add combination fns for any [`Parser`]
pub trait ParserExt<I>: Parser<I> + Sized
where
    I: Input,
{
    /// Create an [`OptParser`] from this parser.
    fn ok(self) -> OptParser<Self> {
        OptParser(self)
    }
}

impl<I, P> ParserExt<I> for P
where
    I: Input,
    P: Parser<I>,
{
}

/// A parser that convert [`recovable`](super::ControlFlow::Recovable) error from inner parser into [`None`] value.
pub struct OptParser<P>(P);

impl<I, P> Parser<I> for OptParser<P>
where
    P: Parser<I>,
    I: Input + Clone,
{
    type Error = P::Error;
    type Output = Option<P::Output>;

    #[inline(always)]
    fn parse(self, input: I) -> Result<Self::Output, I, Self::Error> {
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
pub fn ensure_keyword<KW, I>(keyword: KW) -> impl Parser<I, Output = I, Error = Kind>
where
    I: Input + AsBytes,
    KW: Input + AsBytes,
{
    move |mut input: I| {
        let len = keyword.len();
        if input.len() < len {
            return Err(ControlFlow::Recovable(Kind::Keyword));
        }

        if &input.as_bytes()[..len] == keyword.as_bytes() {
            return Ok((input.split_to(len), input));
        } else {
            return Err(ControlFlow::Recovable(Kind::Keyword));
        }
    }
}

/// Recognize next inpput item.
#[inline(always)]
pub fn ensure_next<C, I>(c: C) -> impl Parser<I, Output = I, Error = Kind>
where
    I: Input<Item = C>,
    C: Item,
{
    move |mut input: I| {
        if let Some(next) = input.iter().next() {
            if next == c {
                return Ok((input.split_to(c.len()), input));
            }
        }

        return Err(ControlFlow::Recovable(Kind::Char));
    }
}

/// Returns the input slice up to the first occurrence of the pattern.
#[inline(always)]
pub fn take_until<KW, I>(keyword: KW) -> impl Parser<I, Output = I, Error = Kind>
where
    I: Input + AsBytes,
    KW: Input + AsBytes,
{
    move |mut input: I| {
        let len = keyword.len();
        if input.len() < len {
            return Err(ControlFlow::Recovable(Kind::TakeUntil));
        }

        if let Some(offset) = memmem::find(input.as_bytes(), keyword.as_bytes()) {
            return Ok((input.split_to(offset), input));
        }

        return Err(ControlFlow::Recovable(Kind::TakeUntil));
    }
}

#[cfg(test)]
mod tests {
    use crate::{ControlFlow, Kind, Parser, Result, ensure_keyword, ensure_next, take_until};

    use super::ParserExt;

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
            ensure_keyword("hello").parse("hello world"),
            Ok(("hello", " world"))
        );

        assert_eq!(
            ensure_keyword("hello").parse(" world  "),
            Err(ControlFlow::Recovable(Kind::Keyword))
        );
    }

    #[test]
    fn test_char() {
        assert_eq!(
            ensure_next('你').parse("你 world  "),
            Ok(("你", " world  "))
        );
    }

    #[test]
    fn test_byte() {
        assert_eq!(
            ensure_next(b'<').parse(b"<world  ".as_slice()),
            Ok((b"<".as_slice(), b"world  ".as_slice()))
        );
    }

    #[test]
    fn test_take_until() {
        assert_eq!(
            take_until("<!--").parse(b"<world  <!--".as_slice()),
            Ok((b"<world  ".as_slice(), b"<!--".as_slice()))
        );
    }
}
