use std::{
    iter::{Copied, Enumerate},
    slice,
    str::{CharIndices, Chars},
};

///! Abstraction of input source code.

/// A region of source code,
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub struct Span {
    pub offset: usize,
    pub len: usize,
}

impl<I> From<I> for Span
where
    I: WithSpan,
{
    fn from(value: I) -> Self {
        Span {
            offset: value.start(),
            len: value.len(),
        }
    }
}

/// Convert self as a reference to [u8]
pub trait AsBytes {
    /// Convert the input type to a byte slice
    fn as_bytes(&self) -> &[u8];
}

/// Convert self as a reference to [`str`]
pub trait AsStr {
    /// Convert the input type to a str slice
    fn as_str(&self) -> &str;
}

/// The item type of the input sequence.
pub trait Item: PartialEq {
    fn len(&self) -> usize;
}

impl Item for u8 {
    #[inline(always)]
    fn len(&self) -> usize {
        1
    }
}

impl Item for char {
    #[inline(always)]
    fn len(&self) -> usize {
        self.len_utf8()
    }
}

/// Parser consumes input type.
pub trait Input {
    /// The current input type is a sequence of that Item type.
    ///
    /// Example: u8 for &[u8] or char for &str
    type Item: Item;

    /// An iterator over the input type, producing the item
    type Iter: Iterator<Item = Self::Item>;

    /// An iterator over the input, producing the item and its byte position.
    type IterIndices: Iterator<Item = (usize, Self::Item)>;

    /// Returns the total length of this input.
    fn len(&self) -> usize;

    /// Split the input into two at the given index.
    ///
    /// Afterwards self contains elements [at, len), and the returned BytesMut contains elements [0, at).
    fn split_to(&mut self, at: usize) -> Self;

    /// Split the input into two at the given index.
    ///
    /// Afterwards self contains elements [0, at), and the returned `Self` contains elements [at, capacity).
    fn split_off(&mut self, at: usize) -> Self;

    fn iter(&self) -> Self::Iter;

    fn iter_indices(&self) -> Self::IterIndices;
}

/// With additional span supports.
pub trait WithSpan: Input {
    /// Returns the start position of this input in the whole source code.
    fn start(&self) -> usize;

    /// Returns the region of this input in the whole source code.
    #[inline(always)]
    fn span(&self) -> Span {
        return Span {
            offset: self.start(),
            len: self.len(),
        };
    }
}

impl<'a> Input for &'a str {
    type Item = char;

    type Iter = Chars<'a>;

    type IterIndices = CharIndices<'a>;

    #[inline(always)]
    fn len(&self) -> usize {
        str::len(self)
    }

    #[inline(always)]
    fn split_to(&mut self, at: usize) -> Self {
        let (first, last) = str::split_at(self, at);

        *self = last;

        first
    }

    #[inline(always)]
    fn split_off(&mut self, at: usize) -> Self {
        let (first, last) = str::split_at(self, at);

        *self = first;

        last
    }
    #[inline(always)]
    fn iter(&self) -> Self::Iter {
        self.chars()
    }
    #[inline(always)]
    fn iter_indices(&self) -> Self::IterIndices {
        self.char_indices()
    }
}

impl<'a> AsBytes for &'a str {
    #[inline(always)]
    fn as_bytes(&self) -> &[u8] {
        str::as_bytes(&self)
    }
}

impl<'a> AsStr for &'a str {
    #[inline(always)]
    fn as_str(&self) -> &str {
        self
    }
}

impl<'a> Input for (usize, &'a str) {
    type Item = char;

    type Iter = Chars<'a>;

    type IterIndices = CharIndices<'a>;

    #[inline(always)]
    fn len(&self) -> usize {
        str::len(self.1)
    }

    #[inline(always)]
    fn split_to(&mut self, at: usize) -> Self {
        let (first, last) = str::split_at(self.1, at);

        self.1 = last;
        let offset = self.0;
        self.0 += first.len();

        (offset, first)
    }

    #[inline(always)]
    fn split_off(&mut self, at: usize) -> Self {
        let (first, last) = str::split_at(self.1, at);

        self.1 = first;

        (self.0 + first.len(), last)
    }

    #[inline(always)]
    fn iter(&self) -> Self::Iter {
        self.1.chars()
    }

    #[inline(always)]
    fn iter_indices(&self) -> Self::IterIndices {
        self.1.char_indices()
    }
}

impl<'a> WithSpan for (usize, &'a str) {
    #[inline(always)]
    fn start(&self) -> usize {
        self.0
    }
}

impl<'a> AsBytes for (usize, &'a str) {
    #[inline(always)]
    fn as_bytes(&self) -> &[u8] {
        self.1.as_bytes()
    }
}

impl<'a> AsStr for (usize, &'a str) {
    #[inline(always)]
    fn as_str(&self) -> &str {
        self.1
    }
}

impl<'a> Input for &'a [u8] {
    type Item = u8;

    type Iter = Copied<slice::Iter<'a, u8>>;

    type IterIndices = Enumerate<Self::Iter>;

    fn len(&self) -> usize {
        <[u8]>::len(*self)
    }

    fn split_to(&mut self, at: usize) -> Self {
        let (first, last) = self.split_at(at);

        *self = last;
        first
    }

    fn split_off(&mut self, at: usize) -> Self {
        let (first, last) = self.split_at(at);

        *self = first;
        last
    }

    fn iter(&self) -> Self::Iter {
        <[u8]>::iter(*self).copied()
    }

    fn iter_indices(&self) -> Self::IterIndices {
        self.iter().enumerate()
    }
}

impl<'a> AsBytes for &'a [u8] {
    fn as_bytes(&self) -> &[u8] {
        self
    }
}

impl<'a> Input for (usize, &'a [u8]) {
    type Item = u8;

    type Iter = Copied<slice::Iter<'a, u8>>;

    type IterIndices = Enumerate<Self::Iter>;

    fn len(&self) -> usize {
        <[u8]>::len(self.1)
    }

    fn split_to(&mut self, at: usize) -> Self {
        let (first, last) = self.1.split_at(at);

        self.1 = last;
        let offset = self.0;
        self.0 += at;

        (offset, first)
    }

    fn split_off(&mut self, at: usize) -> Self {
        let (first, last) = self.1.split_at(at);

        self.1 = first;

        (self.0 + at, last)
    }

    fn iter(&self) -> Self::Iter {
        <[u8]>::iter(self.1).copied()
    }

    fn iter_indices(&self) -> Self::IterIndices {
        self.iter().enumerate()
    }
}

impl<'a> AsBytes for (usize, &'a [u8]) {
    fn as_bytes(&self) -> &[u8] {
        self.1
    }
}
#[cfg(test)]
mod tests {
    use crate::input::{AsBytes, AsStr, Span};

    use super::{Input, WithSpan};

    #[test]
    fn test_split() {
        assert_eq!("100".split_to(1), "1");

        assert_eq!("100".split_off(1), "00");
    }

    #[test]
    fn test_span() {
        assert_eq!((10usize, "hello").span(), Span { offset: 10, len: 5 });

        assert_eq!((10usize, "hello").split_to(2), (10usize, "he"));

        assert_eq!((10usize, "hello").split_off(2), (12usize, "llo"));

        assert_eq!(
            (10usize, b"hello".as_slice()).split_to(2),
            (10usize, b"he".as_slice())
        );

        assert_eq!(
            (10usize, b"hello".as_slice()).split_off(2),
            (12usize, b"llo".as_slice())
        );
    }

    #[test]
    fn test_as_bytes() {
        assert_eq!((10usize, "hello").as_bytes(), "hello".as_bytes());
    }

    #[test]
    fn test_as_str() {
        assert_eq!((10usize, "hello").as_str(), "hello");
    }
}
