use std::{fmt::Debug, marker::PhantomData, str::Chars};

use crate::{ControlFlow, ParseContext, Result, Span, PARSERC_LOG_TARGET};

/// A parser produce output by parsing and consuming the source codes.
pub trait Parser {
    /// Output data type.
    type Output;

    /// Parse and generate a new output.
    fn parse(self, ctx: &mut ParseContext<'_>) -> Result<Self::Output>;
}

/// Implement [`Parser`] for all [`FnMut`](&mut Input<'_>) -> Result<O, E>.
impl<F, O> Parser for F
where
    F: FnOnce(&mut ParseContext<'_>) -> Result<O>,
{
    type Output = O;

    fn parse(self, ctx: &mut ParseContext<'_>) -> Result<Self::Output> {
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
    type Output = Option<S::Output>;

    fn parse(self, ctx: &mut ParseContext<'_>) -> Result<Self::Output> {
        let start = ctx.span();
        match self.0.parse(ctx) {
            Err(err) => match err {
                crate::ControlFlow::Fatal => {
                    if self.1 {
                        ctx.seek(start);
                        return Ok(None);
                    } else {
                        return Err(ControlFlow::Fatal);
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

impl<S, O, Output> Parser for Or<S, O>
where
    S: Parser<Output = Output> + Clone,
    O: Parser<Output = Output>,
{
    type Output = Output;

    fn parse(self, ctx: &mut ParseContext<'_>) -> Result<Self::Output> {
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
    type Output = U;

    fn parse(self, ctx: &mut ParseContext<'_>) -> Result<Self::Output> {
        self.0.parse(ctx).map(self.1)
    }
}

/// A combinator for [`fatal`](ParserExt::fatal) function.
#[derive(Clone)]
pub struct Fatal<S, E>(S, E, Span);

impl<S, E> Parser for Fatal<S, E>
where
    S: Parser,
    E: Debug,
{
    type Output = S::Output;

    fn parse(self, ctx: &mut ParseContext<'_>) -> Result<Self::Output> {
        match self.0.parse(ctx) {
            Err(_) => {
                log::debug!(target: ctx.debug, span:serde = self.2; "{:?}",self.1);
                return Err(ControlFlow::Fatal);
            }
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

    /// Convert any ControlFlow error to a fatal error.
    fn fatal<E>(self, error: E, span: Span) -> Fatal<Self, E>
    where
        E: Debug,
        Self: Sized,
    {
        Fatal(self, error, span)
    }
}

impl<T> ParserExt for T where T: Parser {}

pub trait ParseOkOr<T>: Parser<Output = Option<T>> {
    fn ok_or<E>(self, error: E, span: Span) -> OkOr<Self, E>
    where
        E: Debug,
        Self: Sized,
    {
        OkOr(self, error, span)
    }
}
/// A combinator for [`ok_or`](ParseOkOr::ok_or) function.
pub struct OkOr<P, E>(P, E, Span);

impl<S, E, T> Parser for OkOr<S, E>
where
    S: Parser<Output = Option<T>>,
    E: Debug,
{
    type Output = T;

    fn parse(self, ctx: &mut ParseContext<'_>) -> Result<Self::Output> {
        match self.0.parse(ctx) {
            Ok(Some(v)) => return Ok(v),
            Ok(_) => {
                log::debug!(target: PARSERC_LOG_TARGET,"{:?},{}",self.1,self.2);
                return Err(ControlFlow::Fatal);
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
    /// Parse and construct self from `ctx`
    fn parse(ctx: &mut ParseContext<'_>) -> Result<Self>
    where
        Self: Sized;
}

/// A helper trait that convert [`FromSrc`] into a [`Parser`].
pub trait IntoParser: FromSrc {
    /// Conver self into parser.
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
    type Output = T;

    fn parse(self, ctx: &mut ParseContext<'_>) -> Result<Self::Output> {
        T::parse(ctx)
    }
}

impl<T> Parser for Option<T>
where
    T: FromSrc,
{
    type Output = Option<T>;

    fn parse(self, ctx: &mut ParseContext<'_>) -> Result<Self::Output> {
        T::into_parser().ok().parse(ctx)
    }
}

/// An extension trait to add `parse` function to [`ParseContext`].
pub trait ParseExt {
    fn parse<Item>(&mut self) -> Result<Item>
    where
        Item: FromSrc;
}

impl<'a> ParseExt for ParseContext<'a> {
    fn parse<Item>(&mut self) -> Result<Item>
    where
        Item: FromSrc,
    {
        Item::parse(self)
    }
}

/// The parser ensue the next token is char `c`.
pub fn ensure_char(c: char) -> impl Parser<Output = Span> + Clone {
    move |ctx: &mut ParseContext<'_>| {
        let (next, span) = ctx.next();

        if let Some(next) = next {
            if c == next {
                return Ok(span);
            }

            return Err(ControlFlow::Recoverable);
        }

        // ctx.report_error(Kind::Char(c), span);
        return Err(ControlFlow::Incomplete);
    }
}

/// The parser ensue the next token is char `c`.
pub fn ensure_char_if<F>(f: F) -> impl Parser<Output = Span> + Clone
where
    F: FnOnce(char) -> bool + Clone,
{
    move |ctx: &mut ParseContext<'_>| {
        let (next, span) = ctx.next();

        if let Some(next) = next {
            if f(next) {
                return Ok(span);
            }

            return Err(ControlFlow::Recoverable);
        }

        return Err(ControlFlow::Incomplete);
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
pub fn ensure_keyword<KW: Keyword>(kw: KW) -> impl Parser<Output = Span> + Clone {
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
                    return Err(ControlFlow::Recoverable);
                }
            } else {
                // ctx.report_error(Kind::Keyword(kw.into_string()), span);
                return Err(ControlFlow::Incomplete);
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
pub fn ensure_keyword_insensitive<KW: Keyword>(kw: KW) -> impl Parser<Output = Span> + Clone {
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
                    return Err(ControlFlow::Recoverable);
                }
            } else {
                // ctx.report_error(Kind::Keyword(kw.into_string()), span);
                return Err(ControlFlow::Incomplete);
            }
        }

        let span = start.unwrap().extend_to_inclusive(end.unwrap());

        Ok(span)
    }
}

/// Returns the longest ctx [`Span`] (if any) that matches the predicate.
#[inline(always)]
pub fn take_while_indices<F>(f: F) -> impl Parser<Output = Option<Span>>
where
    F: Fn(usize, char) -> bool,
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
pub fn take_while<F>(f: F) -> impl Parser<Output = Option<Span>>
where
    F: Fn(char) -> bool,
{
    take_while_indices(move |_, c| f(c))
}

/// Returns the longest ctx slice (if any) till a predicate is met.
#[inline(always)]
pub fn take_till<F>(f: F) -> impl Parser<Output = Option<Span>>
where
    F: Fn(char) -> bool,
{
    take_while_indices(move |_, c| !f(c))
}

/// Returns the longest ctx slice (if any) till a predicate is met.
#[inline(always)]
pub fn take_till_indices<F>(f: F) -> impl Parser<Output = Option<Span>>
where
    F: Fn(usize, char) -> bool,
{
    take_while_indices(move |idx, c| !f(idx, c))
}

#[cfg(test)]
mod tests {
    use crate::{ensure_char, take_while, ControlFlow, ParseContext, ParserExt, Span};

    use super::{ensure_keyword, Parser};

    #[test]
    fn test_keyword() {
        assert_eq!(
            ensure_keyword("fn").parse(&mut ParseContext::from("fnhello")),
            Ok(Span::new(0, 2, 1, 1))
        );

        assert_eq!(
            ensure_keyword("struct").parse(&mut ParseContext::from("structhello")),
            Ok(Span::new(0, 6, 1, 1))
        );

        assert_eq!(
            ensure_keyword("fn").parse(&mut ParseContext::from("hfnello")),
            Err(ControlFlow::Recoverable)
        );

        assert_eq!(
            ensure_keyword("fn").parse(&mut ParseContext::from("")),
            Err(ControlFlow::Incomplete)
        );
    }

    #[test]
    fn test_char() {
        assert_eq!(
            ensure_char('f').parse(&mut ParseContext::from("fnhello")),
            Ok(Span::new(0, 1, 1, 1))
        );

        let mut ctx = ParseContext::from("hfnello");

        assert_eq!(
            ensure_char('f').parse(&mut ctx),
            Err(ControlFlow::Recoverable)
        );

        assert_eq!(ctx.size_hint(), (1, 7));

        let mut ctx = ParseContext::from("");

        assert_eq!(
            ensure_char('f').parse(&mut ctx),
            Err(ControlFlow::Incomplete)
        );

        assert_eq!(ctx.size_hint(), (0, 0));
    }

    #[test]
    fn test_take_while() {
        assert_eq!(
            take_while(|c| c.is_alphabetic()).parse(&mut ParseContext::from("hello1")),
            Ok(Some(Span::new(0, 5, 1, 1)))
        );

        assert_eq!(
            take_while(|c| c.is_alphabetic()).parse(&mut ParseContext::from("捏啊哈！！")),
            Ok(Some(Span::new(0, 9, 1, 1)))
        );

        let mut ctx = ParseContext::from("！hello");

        assert_eq!(take_while(|c| c.is_alphabetic()).parse(&mut ctx), Ok(None));

        let mut ctx = ParseContext::from("he！llo");

        assert_eq!(
            take_while(|c| c.is_alphabetic()).parse(&mut ctx),
            Ok(Some(Span::new(0, 2, 1, 1)))
        );

        assert_eq!(ctx.size_hint(), (2, 8));

        let mut ctx = ParseContext::from("");
        assert_eq!(take_while(|c| c.is_alphabetic()).parse(&mut ctx), Ok(None));

        assert_eq!(ctx.size_hint(), (0, 0));
    }

    #[test]
    fn test_ok() {
        assert_eq!(
            ensure_keyword("fn")
                .ok()
                .parse(&mut ParseContext::from("fn")),
            Ok(Some(Span::new(0, 2, 1, 1)))
        );

        assert_eq!(
            ensure_keyword("fn")
                .ok()
                .parse(&mut ParseContext::from("!fn")),
            Ok(None)
        );

        assert_eq!(
            ensure_keyword("fn").ok().parse(&mut ParseContext::from("")),
            Ok(None)
        );

        let mut ctx = ParseContext::from("ft");

        assert_eq!(ensure_keyword("fn").ok().parse(&mut ctx), Ok(None));

        assert_eq!(ctx.size_hint(), (0, 2));
    }

    #[test]
    fn test_map() {
        assert_eq!(
            ensure_keyword("fn")
                .map(|_| true)
                .parse(&mut ParseContext::from("fn")),
            Ok(true)
        );

        assert_eq!(
            ensure_keyword("fn")
                .map(|_| true)
                .parse(&mut ParseContext::from("!fn")),
            Err(ControlFlow::Recoverable)
        );

        assert_eq!(
            ensure_keyword("fn")
                .map(|_| true)
                .parse(&mut ParseContext::from("")),
            Err(ControlFlow::Incomplete)
        );
    }

    #[test]
    fn test_or() {
        assert_eq!(
            ensure_keyword("fn")
                .map(|_| true)
                .or(ensure_keyword("struct").map(|_| false))
                .parse(&mut ParseContext::from("fn")),
            Ok(true)
        );

        assert_eq!(
            ensure_keyword("fn")
                .map(|_| true)
                .or(ensure_keyword("struct").map(|_| false))
                .parse(&mut ParseContext::from("struct")),
            Ok(false)
        );
    }
}
