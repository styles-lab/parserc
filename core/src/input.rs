//! Types for source code abstraction

use std::fmt::Debug;

use crate::span::Span;

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
pub trait Input: PartialEq + Debug {
    /// The current input type is a sequence of that Item type.
    ///
    /// Example: u8 for &[u8] or char for &str
    type Item: Item;

    /// Position type used by [`Span`]
    type Position: Copy;

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

    /// Returns the start position of this input in the whole source code.
    fn start(&self) -> Self::Position;

    /// Returns the end position of this input in the whole source code.
    fn end(&self) -> Self::Position;

    /// Returns the region of this input in the whole source code.
    #[inline(always)]
    fn to_span(&self) -> Span<Self::Position> {
        Span::Some {
            start: self.start(),
            end: self.end(),
        }
    }
}
