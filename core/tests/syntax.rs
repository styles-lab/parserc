use std::fmt::Debug;

use parserc::{
    errors::{ControlFlow, ErrorKind},
    inputs::{Input, StartWith},
    syntax::{Syntax, tokens},
};

tokens!(match Token {
    "{" => LeftBracket,
    "{{" => LeftBracketBracket,
    "}" => RightBracket,
    "}}" => RightBracketBracket,
    "\r\n" => NewLine,
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
