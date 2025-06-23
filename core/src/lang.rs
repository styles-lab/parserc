//! `Input` and `Span` types for compute language parsing.

use std::{fmt::Debug, iter::Enumerate, str::Bytes};

use memchr::memmem;

use crate::input::*;

/// `Span` type alias for compute language parsing.
pub type Span = super::span::Span<usize>;

/// The `Input` short for compute language parsing.
pub trait LangInput:
    Input<Item = u8, Position = usize>
    + AsBytes
    + AsStr
    + StartWith<&'static str>
    + StartWith<&'static [u8]>
    + Find<&'static str>
    + Find<&'static [u8]>
    + Clone
    + Debug
    + PartialEq
{
}

/// `Input` for compute language parsing.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TokenStream<'a> {
    /// offset in the whole token stream.
    pub offset: usize,
    /// current segement string int the whole token stream.
    pub value: &'a str,
}

impl<'a> From<&'a str> for TokenStream<'a> {
    fn from(value: &'a str) -> Self {
        TokenStream { offset: 0, value }
    }
}

impl<'a> From<(usize, &'a str)> for TokenStream<'a> {
    fn from(value: (usize, &'a str)) -> Self {
        TokenStream {
            offset: value.0,
            value: value.1,
        }
    }
}

impl<'a> Input for TokenStream<'a> {
    type Item = u8;

    type Iter = Bytes<'a>;

    type IterIndices = Enumerate<Self::Iter>;

    type Position = usize;

    fn len(&self) -> usize {
        self.value.len()
    }

    fn split_to(&mut self, at: usize) -> Self {
        let (first, last) = self.value.split_at(at);

        self.value = last;
        let offset = self.offset;
        self.offset += at;

        TokenStream {
            offset,
            value: first,
        }
    }

    fn split_off(&mut self, at: usize) -> Self {
        let (first, last) = self.value.split_at(at);

        self.value = first;

        TokenStream {
            offset: self.offset + at,
            value: last,
        }
    }

    fn iter(&self) -> Self::Iter {
        self.value.bytes()
    }

    fn iter_indices(&self) -> Self::IterIndices {
        self.iter().enumerate()
    }

    fn start(&self) -> usize {
        self.offset
    }

    fn end(&self) -> usize {
        self.offset + self.value.len()
    }
}

impl<'a> AsBytes for TokenStream<'a> {
    fn as_bytes(&self) -> &[u8] {
        self.value.as_bytes()
    }
}

impl<'a> AsStr for TokenStream<'a> {
    fn as_str(&self) -> &str {
        self.value
    }
}

impl<'a> StartWith<&str> for TokenStream<'a> {
    fn starts_with(&self, needle: &str) -> Option<usize> {
        if self.as_bytes().starts_with(needle.as_bytes()) {
            Some(needle.len())
        } else {
            None
        }
    }
}

impl<'a> StartWith<&[u8]> for TokenStream<'a> {
    fn starts_with(&self, needle: &[u8]) -> Option<usize> {
        if self.as_bytes().starts_with(needle) {
            Some(needle.len())
        } else {
            None
        }
    }
}

impl<'a, const N: usize> StartWith<&[u8; N]> for TokenStream<'a> {
    fn starts_with(&self, needle: &[u8; N]) -> Option<usize> {
        if self.as_bytes().starts_with(needle) {
            Some(needle.len())
        } else {
            None
        }
    }
}

impl<'a> Find<&str> for TokenStream<'a> {
    fn find(&self, needle: &str) -> Option<usize> {
        memmem::find(self.as_bytes(), needle.as_bytes())
    }
}

impl<'a> Find<&[u8]> for TokenStream<'a> {
    fn find(&self, needle: &[u8]) -> Option<usize> {
        memmem::find(self.as_bytes(), needle)
    }
}

impl<'a, const N: usize> Find<&[u8; N]> for TokenStream<'a> {
    fn find(&self, needle: &[u8; N]) -> Option<usize> {
        memmem::find(self.as_bytes(), needle)
    }
}

impl<'a> LangInput for TokenStream<'a> {}
