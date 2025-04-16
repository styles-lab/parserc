use parserc::{Input, derive_parse};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Kind(#[from] parserc::Kind),
}

pub struct BraceStart<I>(pub I);
pub struct BraceEnd<I>(pub I);

#[derive_parse(error = Error,input = I)]
pub struct Delimiter<I>
where
    I: Input,
{
    pub start: BraceStart<I>,
    pub end: BraceEnd<I>,
}

#[derive_parse(error = Error, input = I)]
pub enum Token<I> {
    Start(BraceStart<I>),

    Start2 {
        first: BraceStart<I>,
        second: BraceStart<I>,
    },
    End(BraceEnd<I>),
    End2(BraceEnd<I>, BraceEnd<I>),
}
