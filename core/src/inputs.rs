//! Types for source code abstraction

use std::{
    fmt::Debug,
    iter::{Copied, Enumerate},
    ops, slice,
    str::{Bytes, CharIndices, Chars},
};

use memchr::memmem;

/// A region presentation in the input data.
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Span {
    /// The start position in the source code.
    pub offset: usize,
    /// Span length.
    pub len: usize,
}

impl Span {
    /// Extend span to the start of the `to`.
    pub fn extend_to(self, to: Span) -> Span {
        assert!(to.offset >= self.offset);

        Span {
            offset: self.offset,
            len: to.offset - self.offset + to.len,
        }
    }

    /// Extend span to the end of the `to`.
    pub fn extend_to_inclusive(self, to: Span) -> Span {
        assert!(to.offset >= self.offset);

        Span {
            offset: self.offset,
            len: to.offset - self.offset + to.len,
        }
    }
}

impl ops::Add for Span {
    type Output = Span;

    fn add(self, rhs: Self) -> Self::Output {
        self.extend_to_inclusive(rhs)
    }
}

impl ops::AddAssign for Span {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

/// An extension trait provides extra `starts_with` func to `Input`.
pub trait StartWith<Needle> {
    /// Convert the input type to a byte slice
    fn starts_with(&self, needle: Needle) -> Option<usize>;
}

/// An extension trait providers extra `find` func to `Input`.
pub trait Find<Needle> {
    /// Returns the index of the first occurrence of the given needle.
    fn find(&self, needle: Needle) -> Option<usize>;
}

/// Convert `Input` as `&[u8]`
pub trait AsBytes {
    /// Convert the input type to a byte slice
    fn as_bytes(&self) -> &[u8];
}

/// Convert `Input` as `&str`
pub trait AsStr {
    /// Convert the input type to a str slice
    fn as_str(&self) -> &str;
}

/// Convert `Input` as `&str`
pub trait Diagnosis {
    /// Generate diagnosis information.
    fn debug(&self) -> String;
}

/// The item type of the input sequence.
pub trait Item: PartialEq + Clone + Copy + Debug {
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

/// The abtraction of `parsers` input data.
pub trait Input: Diagnosis + PartialEq + Debug {
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

    /// Returns true if this input length == 0.
    #[inline(always)]
    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Split the input into two at the given index.
    ///
    /// Afterwards self contains elements [at, len), and the returned BytesMut contains elements [0, at).
    fn split_to(&mut self, at: usize) -> Self;

    /// Split the input into two at the given index.
    ///
    /// Afterwards self contains elements [0, at), and the returned `Self` contains elements [at, capacity).
    fn split_off(&mut self, at: usize) -> Self;

    /// Returns an immutable iterator over source code chars.
    fn iter(&self) -> Self::Iter;

    /// Returns an immutable iterator over source code chars.
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

impl<'a> Diagnosis for &'a str {
    fn debug(&self) -> String {
        if self.len() > 10 {
            format!("{}...", &self[..10])
        } else {
            format!("{}", self)
        }
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

impl<'a> StartWith<&str> for &'a str {
    fn starts_with(&self, needle: &str) -> Option<usize> {
        if self.as_bytes().starts_with(needle.as_bytes()) {
            Some(needle.len())
        } else {
            None
        }
    }
}

impl<'a> StartWith<&[u8]> for &'a str {
    fn starts_with(&self, needle: &[u8]) -> Option<usize> {
        if self.as_bytes().starts_with(needle.as_bytes()) {
            Some(needle.len())
        } else {
            None
        }
    }
}

impl<'a, const N: usize> StartWith<&[u8; N]> for &'a str {
    fn starts_with(&self, needle: &[u8; N]) -> Option<usize> {
        if self.as_bytes().starts_with(needle) {
            Some(needle.len())
        } else {
            None
        }
    }
}

impl<'a> Find<&str> for &'a str {
    fn find(&self, needle: &str) -> Option<usize> {
        memmem::find(self.as_bytes(), needle.as_bytes())
    }
}

impl<'a> Find<&[u8]> for &'a str {
    fn find(&self, needle: &[u8]) -> Option<usize> {
        memmem::find(self.as_bytes(), needle)
    }
}

impl<'a, const N: usize> Find<&[u8; N]> for &'a str {
    fn find(&self, needle: &[u8; N]) -> Option<usize> {
        memmem::find(self.as_bytes(), needle)
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

impl<'a> Diagnosis for &'a [u8] {
    fn debug(&self) -> String {
        if self.len() > 10 {
            format!("{:x?}...", &self[..10])
        } else {
            format!("{:x?}", self)
        }
    }
}

impl<'a> AsBytes for &'a [u8] {
    fn as_bytes(&self) -> &[u8] {
        self
    }
}

impl<'a> StartWith<&[u8]> for &'a [u8] {
    fn starts_with(&self, needle: &[u8]) -> Option<usize> {
        if self.as_bytes().starts_with(needle) {
            Some(needle.len())
        } else {
            None
        }
    }
}

impl<'a, const N: usize> StartWith<&[u8; N]> for &'a [u8] {
    fn starts_with(&self, needle: &[u8; N]) -> Option<usize> {
        if self.as_bytes().starts_with(needle) {
            Some(needle.len())
        } else {
            None
        }
    }
}

impl<'a> Find<&str> for &'a [u8] {
    fn find(&self, needle: &str) -> Option<usize> {
        memmem::find(self.as_bytes(), needle.as_bytes())
    }
}

impl<'a> Find<&[u8]> for &'a [u8] {
    fn find(&self, needle: &[u8]) -> Option<usize> {
        memmem::find(self.as_bytes(), needle)
    }
}

impl<'a, const N: usize> Find<&[u8; N]> for &'a [u8] {
    fn find(&self, needle: &[u8; N]) -> Option<usize> {
        memmem::find(self.as_bytes(), needle)
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

impl<'a> Diagnosis for (usize, &'a str) {
    fn debug(&self) -> String {
        if self.len() > 10 {
            format!("{}...", &self.1[..10])
        } else {
            format!("{}", self.1)
        }
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

impl<'a> StartWith<&str> for (usize, &'a str) {
    fn starts_with(&self, needle: &str) -> Option<usize> {
        if self.as_bytes().starts_with(needle.as_bytes()) {
            Some(needle.len())
        } else {
            None
        }
    }
}

impl<'a> StartWith<&[u8]> for (usize, &'a str) {
    fn starts_with(&self, needle: &[u8]) -> Option<usize> {
        if self.as_bytes().starts_with(needle) {
            Some(needle.len())
        } else {
            None
        }
    }
}

impl<'a, const N: usize> StartWith<&[u8; N]> for (usize, &'a str) {
    fn starts_with(&self, needle: &[u8; N]) -> Option<usize> {
        if self.as_bytes().starts_with(needle) {
            Some(needle.len())
        } else {
            None
        }
    }
}

impl<'a> Find<&str> for (usize, &'a str) {
    fn find(&self, needle: &str) -> Option<usize> {
        memmem::find(self.as_bytes(), needle.as_bytes())
    }
}

impl<'a> Find<&[u8]> for (usize, &'a str) {
    fn find(&self, needle: &[u8]) -> Option<usize> {
        memmem::find(self.as_bytes(), needle)
    }
}

impl<'a, const N: usize> Find<&[u8; N]> for (usize, &'a str) {
    fn find(&self, needle: &[u8; N]) -> Option<usize> {
        memmem::find(self.as_bytes(), needle)
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

impl<'a> Diagnosis for (usize, &'a [u8]) {
    fn debug(&self) -> String {
        if self.len() > 10 {
            format!("{:x?}...", &self.1[..10])
        } else {
            format!("{:x?}", self.1)
        }
    }
}

impl<'a> AsBytes for (usize, &'a [u8]) {
    fn as_bytes(&self) -> &[u8] {
        self.1
    }
}

impl<'a> WithSpan for (usize, &'a [u8]) {
    #[inline(always)]
    fn start(&self) -> usize {
        self.0
    }
}

impl<'a> StartWith<&[u8]> for (usize, &'a [u8]) {
    fn starts_with(&self, needle: &[u8]) -> Option<usize> {
        if self.as_bytes().starts_with(needle) {
            Some(needle.len())
        } else {
            None
        }
    }
}

impl<'a, const N: usize> StartWith<&[u8; N]> for (usize, &'a [u8]) {
    fn starts_with(&self, needle: &[u8; N]) -> Option<usize> {
        if self.as_bytes().starts_with(needle) {
            Some(needle.len())
        } else {
            None
        }
    }
}

impl<'a> Find<&str> for (usize, &'a [u8]) {
    fn find(&self, needle: &str) -> Option<usize> {
        memmem::find(self.1.as_bytes(), needle.as_bytes())
    }
}

impl<'a> Find<&[u8]> for (usize, &'a [u8]) {
    fn find(&self, needle: &[u8]) -> Option<usize> {
        memmem::find(self.as_bytes(), needle)
    }
}

impl<'a, const N: usize> Find<&[u8; N]> for (usize, &'a [u8]) {
    fn find(&self, needle: &[u8; N]) -> Option<usize> {
        memmem::find(self.1.as_bytes(), needle)
    }
}

/// Useful `input` implementation for programming languages.
pub mod lang {
    use super::*;
    /// A programming language source code should implement this trait.
    pub trait LangInput:
        Input<Item = u8>
        + AsBytes
        + AsStr
        + StartWith<&'static str>
        + StartWith<&'static [u8]>
        + Find<&'static str>
        + Find<&'static [u8]>
        + Clone
        + WithSpan
        + Debug
        + PartialEq
    {
    }

    /// An implementation for programming language source code.
    #[derive(Debug, PartialEq, Clone)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
    pub struct TokenStream<'a> {
        pub offset: usize,
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
    }

    impl<'a> Diagnosis for TokenStream<'a> {
        fn debug(&self) -> String {
            if self.len() > 10 {
                format!("{}...", &self.value[..10])
            } else {
                format!("{}", self.value)
            }
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

    impl<'a> WithSpan for TokenStream<'a> {
        #[inline(always)]
        fn start(&self) -> usize {
            self.offset
        }
    }

    impl<'a> LangInput for TokenStream<'a> {}
}

#[cfg(test)]
mod tests {
    use crate::inputs::{Find, Input, StartWith};

    fn require_traits<KW, I>(_input: I, _kw: KW)
    where
        I: Input + StartWith<KW> + Find<KW>,
    {
    }

    #[test]
    fn test_extension_traits() {
        let input = "hello".to_string();

        require_traits(input.as_str(), b"hell");
        require_traits("hello", "hell");
        require_traits(b"hello".as_slice(), b"hell");
        require_traits(b"hello".as_slice(), b"hell".as_slice());
    }
}
