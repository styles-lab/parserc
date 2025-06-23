//! To represents a region of source code or input.

use std::{mem::swap, ops};

/// a region of source code or input.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Span<T> {
    Some {
        /// The start position of the span.
        start: T,
        /// The end position of the span.
        end: T,
    },
    None,
}

impl<T> Span<T> {
    /// Modify with func `f` where self is `some` value.
    pub fn map<F>(self, f: F) -> Self
    where
        F: FnOnce(T, T) -> (T, T),
    {
        if let Self::Some { start, end } = self {
            let (start, end) = f(start, end);
            Self::Some { start, end }
        } else {
            Self::None
        }
    }
}

impl<T> ops::BitXor for Span<T>
where
    T: PartialOrd,
{
    type Output = Span<T>;

    fn bitxor(mut self, rhs: Self) -> Self::Output {
        self ^= rhs;
        self
    }
}

impl<T> ops::BitXorAssign for Span<T>
where
    T: PartialOrd,
{
    fn bitxor_assign(&mut self, mut rhs: Self) {
        match (&mut *self, &mut rhs) {
            (
                Span::Some {
                    start: self_start,
                    end: self_end,
                },
                Span::Some {
                    start: rhs_start,
                    end: rhs_end,
                },
            ) => {
                if (*rhs_start).lt(self_start) {
                    swap(self_start, rhs_start);
                }

                if (*rhs_end).gt(self_end) {
                    swap(self_end, rhs_end);
                }
            }
            (Span::Some { start: _, end: _ }, Span::None) => {}
            (Span::None, Span::Some { start: _, end: _ }) => {
                swap(self, &mut rhs);
            }
            (Span::None, Span::None) => {}
        }
    }
}

/// A trait for converting a value to a `Span`
pub trait ToSpan<T> {
    /// Convert the given value to a `Span`.
    fn to_span(&self) -> Span<T>;
}

impl<T, V> ToSpan<T> for Box<V>
where
    V: ToSpan<T>,
{
    fn to_span(&self) -> Span<T> {
        self.as_ref().to_span()
    }
}

impl<T, V> ToSpan<T> for Option<V>
where
    V: ToSpan<T>,
{
    fn to_span(&self) -> Span<T> {
        match self {
            Some(v) => v.to_span(),
            None => Span::None,
        }
    }
}

impl<T, V> ToSpan<T> for Vec<V>
where
    V: ToSpan<T>,
    T: PartialOrd,
{
    fn to_span(&self) -> Span<T> {
        self.iter()
            .map(|v| v.to_span())
            .fold(Span::None, |lhs, rhs| lhs ^ rhs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_chain_join() {
        assert_eq!(
            Span::Some { start: 0, end: 1 }
                ^ Span::Some { start: 1, end: 2 }
                ^ Span::Some { start: 2, end: 3 },
            Span::Some { start: 0, end: 3 }
        );
    }
}
