use std::fmt::Debug;

use parserc::{
    errors::{ControlFlow, ErrorKind},
    input::{Input, StartWith},
    lang::{Span, TokenStream},
    span::ToSpan,
    syntax::{Delimiter, Punctuated, Syntax, tokens},
};

tokens!(match Token {
    "{" => LeftBracket,
    "{{" => LeftBracketBracket,
    "}" => RightBracket,
    "}}" => RightBracketBracket,
    "\r\n" => NewLine,
    "," => Comma,
});

#[derive(Syntax, PartialEq, Debug)]
#[input(TokenStream<'a>)]
#[error(ErrorKind<usize>)]
pub struct Mock<'a> {
    pub f1: LeftBracket<TokenStream<'a>>,
    #[fatal]
    pub f2: RightBracket<TokenStream<'a>>,
}

#[derive(Syntax, PartialEq, Debug)]
#[input(TokenStream<'a>)]
pub enum MockEnum<'a> {
    LeftBracket(LeftBracket<TokenStream<'a>>),
    RightBracket {
        bracket: RightBracket<TokenStream<'a>>,
    },
}

#[derive(Syntax, PartialEq, Debug)]
#[input(I)]
pub struct Mock2<I>
where
    I: Input + Clone + StartWith<&'static [u8]>,
    I::Position: PartialOrd,
{
    pub f1: LeftBracketBracket<I>,
    #[fatal]
    pub f2: RightBracket<I>,
}

#[test]
fn test_syntax() {
    assert_eq!(
        <Token<_> as Syntax<_, ErrorKind<_>>>::parse(TokenStream::from("{{")),
        Ok((
            Token::LeftBracketBracket(LeftBracketBracket(TokenStream::from("{{"))),
            TokenStream::from((2, ""))
        ))
    );

    assert_eq!(
        Mock::parse(TokenStream::from("{}")),
        Ok((
            Mock {
                f1: LeftBracket(TokenStream::from("{")),
                f2: RightBracket(TokenStream::from((1, "}")))
            },
            TokenStream::from((2, ""))
        ))
    );

    assert_eq!(
        Mock2::parse(TokenStream::from("{{}")),
        Ok((
            Mock2 {
                f1: LeftBracketBracket(TokenStream::from("{{")),
                f2: RightBracket(TokenStream::from((2, "}")))
            },
            TokenStream::from((3, ""))
        ))
    );

    assert_eq!(
        Mock::parse(TokenStream::from("{{}")),
        Err(ControlFlow::Recovable(ErrorKind::Token(
            "{",
            Span::Some { start: 0, end: 3 }
        )))
    );

    assert_eq!(
        Mock::parse(TokenStream::from("{")),
        Err(ControlFlow::Fatal(ErrorKind::Token(
            "}",
            Span::Some { start: 1, end: 1 }
        )))
    );

    assert_eq!(
        <NewLine<_> as Syntax<_, ErrorKind<_>>>::parse(TokenStream::from("\r\n")),
        Ok((
            NewLine(TokenStream::from("\r\n")),
            TokenStream::from((2, ""))
        ))
    );
}

type List<'a> = Punctuated<Mock<'a>, Comma<TokenStream<'a>>>;

#[test]
fn test_punctuated() {
    assert_eq!(
        List::parse(TokenStream::from("{},{}")),
        Ok((
            Punctuated {
                pairs: vec![(
                    Mock {
                        f1: LeftBracket(TokenStream::from("{")),
                        f2: RightBracket(TokenStream::from((1, "}")))
                    },
                    Comma(TokenStream::from((2, ",")))
                )],
                tail: Some(Box::new(Mock {
                    f1: LeftBracket(TokenStream::from((3, "{"))),
                    f2: RightBracket(TokenStream::from((4, "}")))
                }))
            },
            TokenStream::from((5, ""))
        ))
    );

    assert_eq!(
        List::parse(TokenStream::from("{},{},")),
        Ok((
            Punctuated {
                pairs: vec![
                    (
                        Mock {
                            f1: LeftBracket(TokenStream::from("{")),
                            f2: RightBracket(TokenStream::from((1, "}")))
                        },
                        Comma(TokenStream::from((2, ",")))
                    ),
                    (
                        Mock {
                            f1: LeftBracket(TokenStream::from((3, "{"))),
                            f2: RightBracket(TokenStream::from((4, "}")))
                        },
                        Comma(TokenStream::from((5, ",")))
                    )
                ],
                tail: None
            },
            TokenStream::from((6, ""))
        ))
    );
}

#[test]
fn test_delimiter() {
    type D<I> = Delimiter<LeftBracket<I>, RightBracket<I>, Comma<I>>;

    assert_eq!(
        <D<_> as Syntax<_, ErrorKind<_>>>::parse(TokenStream::from("{,}")),
        Ok((
            Delimiter {
                start: LeftBracket(TokenStream::from("{")),
                end: RightBracket(TokenStream::from((2, "}"))),
                body: Comma(TokenStream::from((1, ",")))
            },
            TokenStream::from((3, ""))
        ))
    );
}

#[test]
fn to_span() {
    assert_eq!(
        LeftBracket(TokenStream::from("{")).to_span(),
        Span::Some { start: 0, end: 1 }
    );

    let v = Box::new(LeftBracket(TokenStream::from("{")));
    assert_eq!(v.to_span(), Span::Some { start: 0, end: 1 });

    assert_eq!(
        Some(LeftBracket(TokenStream::from("{"))).to_span(),
        Span::Some { start: 0, end: 1 }
    );

    assert_eq!(
        Option::<LeftBracket<TokenStream<'_>>>::None.to_span(),
        Span::None
    );

    assert_eq!(
        RightBracket(TokenStream::from((10, "}"))).to_span(),
        Span::Some { start: 10, end: 11 }
    );

    type D<I> = Delimiter<LeftBracket<I>, RightBracket<I>, Comma<I>>;

    let (d, _) = <D<_> as Syntax<_, ErrorKind<_>>>::parse(TokenStream::from("{,}")).unwrap();

    assert_eq!(d.to_span(), Span::Some { start: 0, end: 3 });

    type P<I> = Punctuated<LeftBracket<I>, Comma<I>>;

    let (p, _) = <P<_> as Syntax<_, ErrorKind<_>>>::parse(TokenStream::from("{,{")).unwrap();

    assert_eq!(p.to_span(), Span::Some { start: 0, end: 3 });

    let (p, _) = <P<_> as Syntax<_, ErrorKind<_>>>::parse(TokenStream::from("{,{,")).unwrap();

    assert_eq!(p.to_span(), Span::Some { start: 0, end: 4 });
}
