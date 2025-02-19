use std::{
    fmt::{Debug, Display},
    iter::Peekable,
    str::CharIndices,
};

use crate::PARSERC_LOG_TARGET;

/// A `span` is a reference to a fragment of the source code.
#[derive(Debug, Default, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Span {
    pub offset: usize,
    pub len: usize,
    pub lines: usize,
    pub cols: usize,
}

impl From<(usize, usize, usize, usize)> for Span {
    fn from(value: (usize, usize, usize, usize)) -> Self {
        Self {
            offset: value.0,
            len: value.1,
            lines: value.2,
            cols: value.3,
        }
    }
}

impl Span {
    /// Create a new `Span` instance.
    ///
    /// A length of `0` indicates that the span reference to the `eof`
    pub fn new(offset: usize, len: usize, lines: usize, cols: usize) -> Self {
        Self {
            offset,
            len,
            lines,
            cols,
        }
    }

    /// Returns true if the `len` is `0`.
    pub fn eof(&self) -> bool {
        self.len == 0
    }

    /// Returns the postion`(lines,cols)` referenced by this span.
    pub fn position(&self) -> (usize, usize) {
        (self.lines, self.cols)
    }

    /// Return the length of this span in bytes.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Extend self to `other`'s start offset.
    pub fn extend_to(self, other: Span) -> Span {
        assert!(
            !(self.offset > other.offset),
            "extend_to: self.offset < other.offset."
        );

        Span {
            offset: self.offset,
            len: other.offset - self.offset,
            lines: self.lines,
            cols: self.cols,
        }
    }

    /// Extend self to `other`'s end offset.
    pub fn extend_to_inclusive(self, other: Span) -> Span {
        assert!(
            !(self.offset > other.offset),
            "extend_to: self.offset < other.offset."
        );

        Span {
            offset: self.offset,
            len: other.offset + other.len - self.offset,
            lines: self.lines,
            cols: self.cols,
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[Ln {}, Col {}]", self.lines, self.cols)
    }
}
/// Get the slice of String represented by `span`.
pub trait FromSpan<'a> {
    fn from_span(&self, span: Span) -> &'a str;
}

impl<'a> FromSpan<'a> for &'a str {
    fn from_span(&self, span: Span) -> &'a str {
        &self[span.offset..span.offset + span.len]
    }
}

/// A seekable source code stream.
pub struct ParseContext<'a> {
    pub(crate) debug: &'static str,
    /// raw source code.
    source: &'a str,
    /// the char stream iterator.
    iter: Peekable<CharIndices<'a>>,
    /// current reading offset. start with '0'
    offset: usize,
    /// tracking the line no. start with '1'
    lines: usize,
    /// tracking the col no. start with `1`
    cols: usize,
}

impl<'a> From<&'a str> for ParseContext<'a> {
    fn from(value: &'a str) -> Self {
        Self {
            debug: PARSERC_LOG_TARGET,
            source: value,
            iter: value.char_indices().peekable(),
            lines: 1,
            cols: 1,
            offset: 0,
        }
    }
}

impl<'a> From<&'a String> for ParseContext<'a> {
    fn from(value: &'a String) -> Self {
        Self::from(value.as_str())
    }
}

impl<'a> ParseContext<'a> {
    /// update the debug label of this `ParseContext`
    pub fn with_debug(mut self, label: &'static str) -> Self {
        self.debug = label;
        self
    }
    /// update the debug label of this `ParseContext`
    pub fn debug(&mut self, label: &'static str) {
        self.debug = label;
    }

    /// Return the [`Span`] of the next char.
    ///
    /// Use [`eof`](Span::eof) fn to check if the eof is reached.
    #[inline]
    pub fn span(&mut self) -> Span {
        let (_, span) = self.peek();

        span
    }

    /// Convert span into &str.
    #[inline(always)]
    pub fn as_str(&self, span: Span) -> &'a str {
        &self.source[span.offset..span.offset + span.len]
    }

    /// Seek to the start of the `span`.
    ///
    /// A seek beyond the end of the stream is not allowed, will cause a panic.
    #[inline(always)]
    pub fn seek<S>(&mut self, span: S)
    where
        Span: From<S>,
    {
        let span: Span = span.into();

        assert!(span.offset <= self.source.len(), "seek: out of range.");
        if self.offset != span.offset {
            self.offset = span.offset;
            self.cols = span.cols;
            self.lines = span.lines;
            self.iter = self.source[span.offset..].char_indices().peekable();
        }
    }

    /// Returns a tuple where the first element is the reading offset, and second element is the total length of the source code.
    #[inline]
    pub fn size_hint(&mut self) -> (usize, usize) {
        (self.offset, self.source.as_bytes().len())
    }

    /// Returns the unparsed length.
    #[inline]
    pub fn remaining(&mut self) -> usize {
        self.source.as_bytes().len() - self.offset
    }

    /// Returns the unparsed length.
    #[inline]
    pub fn unparsed(&mut self) -> &str {
        let unparsed = &self.source[self.offset..];

        if unparsed.len() > 20 {
            &unparsed[..20]
        } else {
            unparsed
        }
    }

    /// peek up next char in the reading stream.
    #[inline(always)]
    pub fn peek(&mut self) -> (Option<char>, Span) {
        if let Some((_, c)) = self.iter.peek() {
            (
                Some(*c),
                Span {
                    offset: self.offset,
                    len: c.len_utf8(),
                    lines: self.lines,
                    cols: self.cols,
                },
            )
        } else {
            (
                None,
                Span {
                    offset: self.offset,
                    len: 0,
                    lines: self.lines,
                    cols: self.cols,
                },
            )
        }
    }

    /// Returns the next character and its corresponding [`Span`].
    #[inline(always)]
    pub fn next(&mut self) -> (Option<char>, Span) {
        if let Some((_, c)) = self.iter.next() {
            let span = Span {
                offset: self.offset,
                len: c.len_utf8(),
                lines: self.lines,
                cols: self.cols,
            };

            // update tracking datas.
            self.offset += c.len_utf8();
            // is newline.
            if c == '\n' {
                self.lines += 1;
                self.cols = 1;
            } else {
                self.cols += 1;
            }

            (Some(c), span)
        } else {
            (
                None,
                Span {
                    offset: self.offset,
                    len: 0,
                    lines: self.lines,
                    cols: self.cols,
                },
            )
        }
    }
}

#[cfg(test)]
mod tests {

    use std::panic::catch_unwind;

    use super::*;

    #[test]
    fn test_size_hint() {
        assert_eq!(ParseContext::from("hello world").size_hint(), (0, 11));
        assert_eq!(
            ParseContext::from("你好").size_hint(),
            (0, "你好".as_bytes().len())
        );
    }

    #[test]
    fn test_span() {
        assert_eq!(ParseContext::from("hello world").span().len(), 1);
        assert!(ParseContext::from("").span().eof());
        assert_eq!(ParseContext::from("你好").span().len(), '你'.len_utf8());
    }

    #[test]
    fn test_seek() {
        ParseContext::from("hello world").seek(Span {
            offset: 11,
            len: 0,
            lines: 1,
            cols: 12,
        });

        let r = catch_unwind(|| {
            ParseContext::from("hello world").seek(Span {
                offset: 12,
                len: 0,
                lines: 1,
                cols: 12,
            });
        });

        assert!(r.is_err());
    }

    #[test]
    fn test_input() {
        let mut input = ParseContext::from("你好\nh");

        assert_eq!(
            input.next(),
            (
                Some('你'),
                Span {
                    offset: 0,
                    len: 3,
                    lines: 1,
                    cols: 1
                }
            )
        );

        assert_eq!(input.next().0, Some('好'));
        assert_eq!(input.next().0, Some('\n'));
        assert_eq!(
            input.next(),
            (
                Some('h'),
                Span {
                    offset: 7,
                    len: 1,
                    lines: 2,
                    cols: 1
                }
            )
        );

        for _ in 0..10 {
            assert_eq!(
                input.next(),
                (
                    None,
                    Span {
                        offset: 8,
                        len: 0,
                        lines: 2,
                        cols: 2
                    }
                )
            );
        }
    }
}
