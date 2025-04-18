use parserc::{AsBytes, Input, Parse, Parser, ParserExt, derive_parse, keyword};

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum Error {
    #[error(transparent)]
    Kind(#[from] parserc::Kind),
}

#[derive(PartialEq, Debug)]
pub struct BraceStart<I>(pub I);

impl<I> Parse<I> for BraceStart<I>
where
    I: Input + AsBytes,
{
    type Error = Error;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        Ok(keyword("{").map(|v| Self(v)).parse(input)?)
    }
}

#[derive(PartialEq, Debug)]
pub struct BraceEnd<I>(pub I);

impl<I> Parse<I> for BraceEnd<I>
where
    I: Input + AsBytes,
{
    type Error = Error;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        Ok(keyword("{").map(|v| Self(v)).parse(input)?)
    }
}

#[derive_parse(error = Error,input = I)]
pub struct Delimiter<I>
where
    I: Input + AsBytes + Clone,
{
    pub start: BraceStart<I>,
    pub end: Option<BraceEnd<I>>,
}

#[derive_parse(error = Error,input = I)]
pub struct Delimiter2<I>(pub BraceStart<I>, pub BraceEnd<I>)
where
    I: Input + AsBytes;

#[derive_parse(error = Error, input = I)]
#[derive(PartialEq, Debug)]
pub enum Token<I>
where
    I: Input + AsBytes + Clone,
{
    Start2 {
        first: BraceStart<I>,
        second: BraceStart<I>,
    },
    End2(BraceEnd<I>, BraceEnd<I>),
    Start(BraceStart<I>),
    End(BraceEnd<I>),
    Opt(Option<BraceEnd<I>>),
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::{BraceStart, Token};

    #[test]
    fn test_token() {
        Token::parse("{").unwrap();
        assert_eq!(
            Token::parse("{{"),
            Ok((
                Token::Start2 {
                    first: BraceStart("{"),
                    second: BraceStart("{")
                },
                ""
            ))
        );
    }
}
