use std::fmt::Debug;

use parserc::{
    errors::{ControlFlow, ErrorKind},
    inputs::{Input, StartWith},
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
#[input(&'a str)]
#[error(ErrorKind<&'a str>)]
pub struct Mock<'a> {
    pub f1: LeftBracket<&'a str>,
    #[fatal]
    pub f2: RightBracket<&'a str>,
}

#[derive(Syntax, PartialEq, Debug)]
#[input(I)]
pub struct Mock2<I>
where
    I: Input + Clone + StartWith<&'static [u8]>,
{
    pub f1: LeftBracketBracket<I>,
    #[fatal]
    pub f2: RightBracket<I>,
}

#[test]
fn test_syntax() {
    assert_eq!(
        Token::parse("{{"),
        Ok((Token::LeftBracketBracket(LeftBracketBracket("{{")), ""))
    );
    assert_eq!(
        Mock::parse("{}"),
        Ok((
            Mock {
                f1: LeftBracket("{"),
                f2: RightBracket("}")
            },
            ""
        ))
    );

    assert_eq!(
        Mock2::parse("{{}"),
        Ok((
            Mock2 {
                f1: LeftBracketBracket("{{"),
                f2: RightBracket("}")
            },
            ""
        ))
    );

    assert_eq!(
        Mock::parse("{{}"),
        Err(ControlFlow::Recovable(ErrorKind::Token(
            "{",
            "{{}".to_string()
        )))
    );

    assert_eq!(
        Mock::parse("{"),
        Err(ControlFlow::Fatal(ErrorKind::Token("}", "".to_string())))
    );

    assert_eq!(
        <NewLine<_> as Syntax<_, ErrorKind<_>>>::parse(b"\r\n".as_slice()),
        Ok((NewLine(b"\r\n".as_slice()), "".as_bytes()))
    );
}

type List<'a> = Punctuated<Mock<'a>, Comma<&'a str>>;

#[test]
fn test_punctuated() {
    assert_eq!(
        List::parse("{},{}"),
        Ok((
            Punctuated {
                pairs: vec![(
                    Mock {
                        f1: LeftBracket("{"),
                        f2: RightBracket("}")
                    },
                    Comma(",")
                )],
                tail: Some(Box::new(Mock {
                    f1: LeftBracket("{"),
                    f2: RightBracket("}")
                }))
            },
            ""
        ))
    );

    assert_eq!(
        List::parse("{},{},"),
        Ok((
            Punctuated {
                pairs: vec![
                    (
                        Mock {
                            f1: LeftBracket("{"),
                            f2: RightBracket("}")
                        },
                        Comma(",")
                    ),
                    (
                        Mock {
                            f1: LeftBracket("{"),
                            f2: RightBracket("}")
                        },
                        Comma(",")
                    )
                ],
                tail: None
            },
            ""
        ))
    );
}

#[test]
fn test_delimiter() {
    type D<'a> = Delimiter<LeftBracket<&'a str>, RightBracket<&'a str>, Comma<&'a str>>;

    assert_eq!(
        <D as Syntax<_, ErrorKind<_>>>::parse("{,}"),
        Ok((
            Delimiter {
                start: LeftBracket("{"),
                end: RightBracket("}"),
                body: Comma(",")
            },
            ""
        ))
    );
}
