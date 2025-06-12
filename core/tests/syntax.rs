use std::fmt::Debug;

use parserc::{
    errors::{ControlFlow, ErrorKind},
    inputs::{Input, StartWith},
    syntax::{Syntax, def_token_table},
};

def_token_table!(match Token {
    "{" => LeftBracket,
    "{{" => LeftBracketBracket,
    "}" => RightBracket,
    "}}" => RightBracketBracket,
});

#[derive(Syntax, PartialEq, Debug)]
#[input(&'a str)]
pub struct Mock<'a> {
    pub f1: LeftBracket<&'a str>,
    #[fatal]
    pub f2: RightBracket<&'a str>,
}

#[derive(Syntax, PartialEq, Debug)]
#[input(I)]
pub struct Mock2<I>
where
    I: Input + Clone + StartWith<&'static str>,
{
    pub f1: LeftBracketBracket<I>,
    #[fatal]
    pub f2: RightBracket<I>,
}

#[test]
fn test_token_lookahead() {
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
}
