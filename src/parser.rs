use std::{marker::PhantomData, str::Chars};

use crate::{ControlFlow, Kind, ParseContext, ParseError, Result, Span};

/// A parser produce output by parsing and consuming the source codes.
pub trait Parser {
    /// Error type returns by [`parse`](Parser::parse) when some error occured.
    type Error: ParseError;
    /// Output data type.
    type Output;

    /// Parse and generate a new output.
    fn parse(self, ctx: &mut ParseContext<'_>) -> Result<Self::Output, Self::Error>;
}

/// Implement [`Parser`] for all [`FnMut`](&mut Input<'_>) -> Result<O, E>.
impl<F, O, E> Parser for F
where
    E: ParseError,
    F: FnOnce(&mut ParseContext<'_>) -> Result<O, E>,
{
    type Error = E;
    type Output = O;

    fn parse(self, ctx: &mut ParseContext<'_>) -> Result<Self::Output, Self::Error> {
        (self)(ctx)
    }
}

/// A combinator for [`ok`](ParserExt::ok) function.
#[derive(Clone)]
pub struct Optional<S>(S, bool);

impl<S> Parser for Optional<S>
where
    S: Parser,
{
    type Error = S::Error;
    type Output = Option<S::Output>;

    fn parse(self, ctx: &mut ParseContext<'_>) -> Result<Self::Output, Self::Error> {
        let start = ctx.span();
        match self.0.parse(ctx) {
            Err(err) => match err {
                crate::ControlFlow::Fatal(err) => {
                    if self.1 {
                        ctx.seek(start);
                        return Ok(None);
                    } else {
                        return Err(ControlFlow::Fatal(err));
                    }
                }
                _ => {
                    ctx.seek(start);
                    return Ok(None);
                }
            },
            Ok(output) => Ok(Some(output)),
        }
    }
}

/// A combinator for [`or`](ParserExt::or) function.
#[derive(Clone)]
pub struct Or<S, O>(S, O);

impl<S, O, Output, Error> Parser for Or<S, O>
where
    S: Parser<Output = Output, Error = Error> + Clone,
    O: Parser<Output = Output, Error = Error>,
    Error: ParseError,
{
    type Error = Error;
    type Output = Output;

    fn parse(self, ctx: &mut ParseContext<'_>) -> Result<Self::Output, Self::Error> {
        if let Some(output) = self.0.clone().ok().parse(ctx)? {
            return Ok(output);
        } else {
            self.1.parse(ctx)
        }
    }
}

/// A combinator for [`map`](ParserExt::map) function.
#[derive(Clone)]
pub struct Map<S, F>(S, F);

impl<S, F, U> Parser for Map<S, F>
where
    S: Parser,
    F: FnOnce(S::Output) -> U,
{
    type Error = S::Error;
    type Output = U;

    fn parse(self, ctx: &mut ParseContext<'_>) -> Result<Self::Output, Self::Error> {
        self.0.parse(ctx).map(self.1)
    }
}

/// A combinator for [`map_err`](ParserExt::map_err) function.
#[derive(Clone)]
pub struct MapErr<S, F>(S, F);

impl<S, F, E> Parser for MapErr<S, F>
where
    S: Parser,
    F: FnOnce(S::Error) -> E,
    E: ParseError,
{
    type Error = E;
    type Output = S::Output;

    fn parse(self, ctx: &mut ParseContext<'_>) -> Result<Self::Output, Self::Error> {
        self.0.parse(ctx).map_err(|err| match err {
            ControlFlow::Recoverable(err) => ControlFlow::Recoverable((self.1)(err)),
            ControlFlow::Incomplete(err) => ControlFlow::Incomplete((self.1)(err)),
            ControlFlow::Fatal(err) => ControlFlow::Fatal((self.1)(err)),
        })
    }
}

/// A combinator for [`fatal`](ParserExt::fatal) function.
#[derive(Clone)]
pub struct Fatal<S>(S, S::Error)
where
    S: Parser;

impl<S> Parser for Fatal<S>
where
    S: Parser,
{
    type Error = S::Error;
    type Output = S::Output;

    fn parse(self, ctx: &mut ParseContext<'_>) -> Result<Self::Output, Self::Error> {
        match self.0.parse(ctx) {
            Err(err) => match err {
                ControlFlow::Recoverable(_) => Err(ControlFlow::Fatal(self.1)),
                ControlFlow::Incomplete(_) => Err(ControlFlow::Fatal(self.1)),
                ControlFlow::Fatal(_) => Err(ControlFlow::Fatal(self.1)),
            },
            r => return r,
        }
    }
}

/// An extension trait for [`Parser`] combinators.
pub trait ParserExt: Parser {
    /// Convert parser result from [`Recoverable`] / [`Incomplete`] errors to [`None`].
    ///
    /// [`Recoverable`]: crate::ControlFlow::Recoverable
    /// [`Incomplete`]: crate::ControlFlow::Incomplete
    #[inline]
    fn ok(self) -> Optional<Self>
    where
        Self: Sized,
    {
        Optional(self, false)
    }

    /// Convert parser all errors to [`None`].
    ///
    /// [`Recoverable`]: crate::ControlFlow::Recoverable
    /// [`Incomplete`]: crate::ControlFlow::Incomplete
    #[inline]
    fn catch_fatal(self) -> Optional<Self>
    where
        Self: Sized,
    {
        Optional(self, true)
    }

    /// Sequentially execute two parsers, until one of them returns successfully.
    #[inline]
    fn or<O>(self, other: O) -> Or<Self, O>
    where
        Self: Sized + Clone,
    {
        Or(self, other)
    }

    /// Map parser `Result<T,E>` to `Result<U,E>` by applying a function to a contained Ok value, leaving an Err value untouched.
    fn map<F, U>(self, op: F) -> Map<Self, F>
    where
        F: FnOnce(Self::Output) -> U,
        Self: Sized,
    {
        Map(self, op)
    }

    /// Map parser `Result<T,E>` to `Result<T,U>` by applying a function to a contained Err value, leaving an Ok value untouched.
    fn map_err<F, U>(self, op: F) -> MapErr<Self, F>
    where
        F: FnOnce(<Self as Parser>::Error) -> U,
        U: ParseError,
        Self: Sized,
    {
        MapErr(self, op)
    }

    /// Convert any ControlFlow error to a fatal error.
    fn fatal(self, error: Self::Error) -> Fatal<Self>
    where
        Self: Sized,
    {
        Fatal(self, error)
    }
}

impl<T> ParserExt for T where T: Parser {}

pub trait ParseOkOr<T>: Parser<Output = Option<T>> {
    fn ok_or(self, error: Self::Error) -> OkOr<Self>
    where
        Self: Sized,
    {
        OkOr {
            parser: self,
            error,
        }
    }
}
/// A combinator for [`ok_or`](ParseOkOr::ok_or) function.
pub struct OkOr<P>
where
    P: Parser,
{
    parser: P,
    error: P::Error,
}

impl<S, T> Parser for OkOr<S>
where
    S: Parser<Output = Option<T>>,
{
    type Error = S::Error;
    type Output = T;

    fn parse(self, ctx: &mut ParseContext<'_>) -> Result<Self::Output, Self::Error> {
        match self.parser.parse(ctx) {
            Ok(Some(v)) => return Ok(v),
            Ok(_) => {
                return Err(ControlFlow::Fatal(self.error));
            }
            Err(c) => {
                return Err(c);
            }
        }
    }
}

impl<T, P> ParseOkOr<T> for P where P: Parser<Output = Option<T>> {}

/// All types that can be parsed from source code must implement this trait.
///
/// See [`parse`](ParseExt::parse) function.
pub trait FromSrc {
    type Error: ParseError;
    /// Parse and construct self from `ctx`
    fn parse(ctx: &mut ParseContext<'_>) -> Result<Self, Self::Error>
    where
        Self: Sized;
}

/// A helper trait that convert [`FromSrc`] into a [`Parser`].
pub trait IntoParser: FromSrc {
    /// Conver self into parser.
    #[inline]
    fn into_parser() -> FromSrcParser<Self>
    where
        Self: Sized,
    {
        FromSrcParser(Default::default())
    }
}

impl<T> IntoParser for T where T: FromSrc {}

/// A wrapper parser for [`FromSrc`] type.
pub struct FromSrcParser<T>(PhantomData<T>);

impl<T> Clone for FromSrcParser<T> {
    fn clone(&self) -> Self {
        Self(Default::default())
    }
}

impl<T> Parser for FromSrcParser<T>
where
    T: FromSrc,
{
    type Error = T::Error;
    type Output = T;

    fn parse(self, ctx: &mut ParseContext<'_>) -> Result<Self::Output, Self::Error> {
        T::parse(ctx)
    }
}

impl<T> Parser for Option<T>
where
    T: FromSrc,
{
    type Error = T::Error;
    type Output = Option<T>;

    fn parse(self, ctx: &mut ParseContext<'_>) -> Result<Self::Output, Self::Error> {
        T::into_parser().ok().parse(ctx)
    }
}

/// An extension trait to add `parse` function to [`ParseContext`].
pub trait ParseExt {
    fn parse<Item>(&mut self) -> Result<Item, Item::Error>
    where
        Item: FromSrc;
}

impl<'a> ParseExt for ParseContext<'a> {
    fn parse<Item>(&mut self) -> Result<Item, Item::Error>
    where
        Item: FromSrc,
    {
        Item::parse(self)
    }
}

/// The parser ensue the next token is char `c`.
pub fn ensure_char<E>(c: char) -> impl Parser<Output = Span, Error = E> + Clone
where
    E: ParseError,
{
    move |ctx: &mut ParseContext<'_>| {
        let (next, span) = ctx.next();

        if let Some(next) = next {
            if c == next {
                return Ok(span);
            }

            return Err(ControlFlow::Recoverable(Kind::EnsureChar.into()));
        }

        // ctx.report_error(Kind::Char(c), span);
        return Err(ControlFlow::Incomplete(Kind::EnsureChar.into()));
    }
}

/// The parser ensue the next token is char `c`.
pub fn ensure_char_if<F, E>(f: F) -> impl Parser<Output = Span, Error = E> + Clone
where
    F: FnOnce(char) -> bool + Clone,
    E: ParseError,
{
    move |ctx: &mut ParseContext<'_>| {
        let (next, span) = ctx.next();

        if let Some(next) = next {
            if f(next) {
                return Ok(span);
            }

            return Err(ControlFlow::Recoverable(Kind::EnsureCharIf.into()));
        }

        return Err(ControlFlow::Incomplete(Kind::EnsureCharIf.into()));
    }
}

/// A type trait used by [`ensure_keyword`] function.
pub trait Keyword: Clone {
    /// Get `String` representation of this keyword.
    fn into_string(self) -> String;

    /// Returns char iterator.
    fn chars(&self) -> Chars<'_>;

    /// Return keywrod length in bytes.
    fn len(&self) -> usize;
}

impl Keyword for &str {
    fn into_string(self) -> String {
        str::to_string(&self)
    }

    fn chars(&self) -> Chars<'_> {
        str::chars(&self)
    }

    fn len(&self) -> usize {
        str::len(&self)
    }
}

impl Keyword for String {
    fn into_string(self) -> String {
        self
    }

    fn chars(&self) -> Chars<'_> {
        str::chars(&self)
    }

    fn len(&self) -> usize {
        str::len(&self)
    }
}

/// The parser ensue the next token is a keyword `kw`.
///
/// A keyword is a seqence of chars without spaces.
#[inline(always)]
pub fn ensure_keyword<KW: Keyword, E>(kw: KW) -> impl Parser<Output = Span, Error = E> + Clone
where
    E: ParseError,
{
    assert!(kw.len() > 0, "keyword length must greate than 0");
    move |ctx: &mut ParseContext<'_>| {
        let chars = kw.chars();

        let mut start = None;
        let mut end = None;

        for c in chars {
            let (next, span) = ctx.next();

            if start.is_none() {
                start = Some(span);
            }

            end = Some(span);

            if let Some(next) = next {
                if next != c {
                    // ctx.report_error(Kind::Keyword(kw.into_string()), span);
                    return Err(ControlFlow::Recoverable(Kind::EnsureKeyword.into()));
                }
            } else {
                // ctx.report_error(Kind::Keyword(kw.into_string()), span);
                return Err(ControlFlow::Incomplete(Kind::EnsureKeyword.into()));
            }
        }

        let span = start.unwrap().extend_to_inclusive(end.unwrap());

        Ok(span)
    }
}

/// The parser ensue the next token is a keyword `kw`.
///
/// A keyword is a seqence of chars without spaces.
#[inline(always)]
pub fn ensure_keyword_insensitive<KW: Keyword, E>(
    kw: KW,
) -> impl Parser<Output = Span, Error = E> + Clone
where
    E: ParseError,
{
    assert!(kw.len() > 0, "keyword length must greate than 0");

    move |ctx: &mut ParseContext<'_>| {
        let chars = kw.chars();

        let mut start = None;
        let mut end = None;

        for c in chars {
            let (next, span) = ctx.next();

            if start.is_none() {
                start = Some(span);
            }

            end = Some(span);

            if let Some(next) = next {
                if next.to_ascii_lowercase() != c.to_ascii_lowercase() {
                    // ctx.report_error(Kind::Keyword(kw.into_string()), span);
                    return Err(ControlFlow::Recoverable(Kind::EnsureKeyword.into()));
                }
            } else {
                // ctx.report_error(Kind::Keyword(kw.into_string()), span);
                return Err(ControlFlow::Incomplete(Kind::EnsureKeyword.into()));
            }
        }

        let span = start.unwrap().extend_to_inclusive(end.unwrap());

        Ok(span)
    }
}

/// Returns the longest ctx [`Span`] (if any) that matches the predicate.
#[inline(always)]
pub fn take_while_indices<F, E>(f: F) -> impl Parser<Output = Option<Span>, Error = E>
where
    F: Fn(usize, char) -> bool,
    E: ParseError,
{
    move |ctx: &mut ParseContext<'_>| {
        let (c, start) = ctx.next();

        if c.is_none() {
            return Ok(None);
        }

        let mut indx = 0;

        if !f(indx, c.unwrap()) {
            ctx.seek(start);
            return Ok(None);
        }

        let mut end = start;

        while let (Some(c), span) = ctx.next() {
            indx += 1;

            if !f(indx, c) {
                ctx.seek(span);
                break;
            }

            end = span;
        }

        Ok(Some(start.extend_to_inclusive(end)))
    }
}

/// Returns the longest ctx [`Span`] (if any) that matches the predicate.
///
/// This parser will never returns an error.
#[inline(always)]
pub fn take_while<F, E>(f: F) -> impl Parser<Output = Option<Span>, Error = E>
where
    F: Fn(char) -> bool,
    E: ParseError,
{
    take_while_indices(move |_, c| f(c))
}

/// Returns the longest ctx slice (if any) till a predicate is met.
#[inline(always)]
pub fn take_till<F, E>(f: F) -> impl Parser<Output = Option<Span>, Error = E>
where
    F: Fn(char) -> bool,
    E: ParseError,
{
    take_while_indices(move |_, c| !f(c))
}

/// Returns the longest ctx slice (if any) till a predicate is met.
#[inline(always)]
pub fn take_till_indices<F, E>(f: F) -> impl Parser<Output = Option<Span>, Error = E>
where
    F: Fn(usize, char) -> bool,
    E: ParseError,
{
    take_while_indices(move |idx, c| !f(idx, c))
}

#[cfg(test)]
mod tests {
    use crate::{
        ControlFlow, Kind, ParseContext, ParserExt, Result, Span, ensure_char, take_while,
    };

    use super::{Parser, ensure_keyword};

    #[test]
    fn test_keyword() {
        fn parse(i: &str, keyword: &str) -> Result<Span, Kind> {
            ensure_keyword(keyword).parse(&mut ParseContext::from(i))
        }

        assert_eq!(parse("fnhello", "fn"), Ok(Span::new(0, 2, 1, 1)));

        assert_eq!(parse("structhello", "struct"), Ok(Span::new(0, 6, 1, 1)));

        assert_eq!(
            parse("hfnello", "fn"),
            Err(ControlFlow::Recoverable(Kind::EnsureKeyword))
        );

        assert_eq!(
            parse("", "fn"),
            Err(ControlFlow::Incomplete(Kind::EnsureKeyword))
        );
    }

    #[test]
    fn test_char() {
        fn parse(i: &str, c: char) -> Result<Span, Kind> {
            ensure_char(c).parse(&mut ParseContext::from(i))
        }

        assert_eq!(parse("fnhello", 'f'), Ok(Span::new(0, 1, 1, 1)));

        assert_eq!(
            parse("hfnello", 'f'),
            Err(ControlFlow::Recoverable(Kind::EnsureChar))
        );

        assert_eq!(
            parse("", 'f'),
            Err(ControlFlow::Incomplete(Kind::EnsureChar))
        );
    }

    #[test]
    fn test_take_while() {
        fn parse(i: &str) -> Result<Option<Span>, Kind> {
            take_while(|c| c.is_alphabetic()).parse(&mut ParseContext::from(i))
        }
        assert_eq!(parse("hello1"), Ok(Some(Span::new(0, 5, 1, 1))));

        assert_eq!(parse("捏啊哈！！"), Ok(Some(Span::new(0, 9, 1, 1))));

        assert_eq!(parse("！hello"), Ok(None));

        assert_eq!(parse("he！llo"), Ok(Some(Span::new(0, 2, 1, 1))));

        assert_eq!(parse(""), Ok(None));
    }

    #[test]
    fn test_ok() {
        fn parse(i: &str, keyword: &str) -> Result<Option<Span>, Kind> {
            ensure_keyword(keyword)
                .ok()
                .parse(&mut ParseContext::from(i))
        }

        assert_eq!(parse("fn", "fn"), Ok(Some(Span::new(0, 2, 1, 1))));

        assert_eq!(parse("!fn", "fn"), Ok(None));

        assert_eq!(parse("", "fn"), Ok(None));

        assert_eq!(parse("ft", "fn"), Ok(None));
    }

    #[test]
    fn test_map() {
        fn parse(i: &str, keyword: &str) -> Result<bool, Kind> {
            ensure_keyword(keyword)
                .map(|_| true)
                .parse(&mut ParseContext::from(i))
        }
        assert_eq!(parse("fn", "fn"), Ok(true));

        assert_eq!(
            parse("!fn", "fn"),
            Err(ControlFlow::Recoverable(Kind::EnsureKeyword))
        );

        assert_eq!(
            parse("", "fn"),
            Err(ControlFlow::Incomplete(Kind::EnsureKeyword))
        );
    }

    #[test]
    fn test_or() {
        fn parse(i: &str, k1: &str, k2: &str) -> Result<bool, Kind> {
            ensure_keyword(k1)
                .map(|_| true)
                .or(ensure_keyword(k2).map(|_| false))
                .parse(&mut ParseContext::from(i))
        }
        assert_eq!(parse("fn", "fn", "struct"), Ok(true));

        assert_eq!(parse("struct", "fn", "struct"), Ok(false));
    }
}
