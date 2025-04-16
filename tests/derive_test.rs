use parserc::Parse;

pub struct BraceStart<I>(pub I);
pub struct BraceEnd<I>(pub I);

#[derive(Parse)]
pub struct Delimiter<I> {
    pub start: BraceStart<I>,
    pub end: BraceEnd<I>,
}

pub enum Token<I> {
    Start(BraceStart<I>),

    Start2 {
        first: BraceStart<I>,
        second: BraceStart<I>,
    },
    End(BraceEnd<I>),
    End2(BraceEnd<I>, BraceEnd<I>),
}
