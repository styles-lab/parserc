use parserc::{AsBytes, Input, Parse, Parser, ParserExt, derive_parse, keyword};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Kind(#[from] parserc::Kind),
}

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
    I: Input + AsBytes,
{
    pub start: BraceStart<I>,
    pub end: BraceEnd<I>,
}

#[derive_parse(error = Error,input = I)]
pub struct Delimiter2<I>(pub BraceStart<I>, pub BraceEnd<I>)
where
    I: Input + AsBytes;

#[derive_parse(error = Error, input = I)]
pub enum Token<I>
where
    I: Input + AsBytes,
{
    Start(BraceStart<I>),

    Start2 {
        first: BraceStart<I>,
        second: BraceStart<I>,
    },
    End(BraceEnd<I>),
    End2(BraceEnd<I>, BraceEnd<I>),
}
