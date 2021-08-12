//! Provides a generic interface for flag data types.

use std::ops::{BitAnd, BitAndAssign, Not, Shl};

/// Generic data type for use as flag data.
///
/// This only provides enough information for use with an interior integer type. Most flag data
/// implementations have stricter requirements for the type.
pub trait DataType: Sized + Clone + Send + Sync {
    /// The number of bits the data type contains.
    const BITS: usize;
}

/// A primitive integer.
/// Exactly provides the needed methods, constants and traits for use with the flag data types.
///
/// An external crate is not used because no found crate delivered a trait with all needed
/// information and because of Rusts orphan rule, they can't be extended by this crate.
pub trait Integer:
    DataType
    + Copy
    + Eq
    + Shl<usize, Output = Self>
    + BitAnd<Output = Self>
    + BitAndAssign
    + Not<Output = Self>
{
    /// The maximum value of the type. Is used as initial value for flag data.
    const MAX: Self;
    /// One (1) in the specified type. Used to get rid of conversions.
    const ONE: Self;
    /// Zero (0) in the specified type. Used to get rid of conversions.
    const ZERO: Self;

    /// Returns the number of set bits of the integer.
    fn count_ones(self) -> usize;

    /// Performs a rotation of `n` bits to the left. Bits that are rotated beyond the integer size
    /// "rotate" back to the least significant bit.
    fn rotate_left(self, n: u32) -> Self;
}

impl DataType for u8 {
    const BITS: usize = u8::BITS as usize;
}

impl Integer for u8 {
    const MAX: Self = u8::MAX;
    const ONE: Self = 1;
    const ZERO: Self = 0;

    #[inline]
    fn count_ones(self) -> usize {
        self.count_ones() as usize
    }

    #[inline]
    fn rotate_left(self, n: u32) -> Self {
        self.rotate_left(n)
    }
}

impl DataType for u32 {
    const BITS: usize = u32::BITS as usize;
}

impl Integer for u32 {
    const MAX: Self = u32::MAX;
    const ONE: Self = 1;
    const ZERO: Self = 0;

    #[inline]
    fn count_ones(self) -> usize {
        self.count_ones() as usize
    }

    #[inline]
    fn rotate_left(self, n: u32) -> Self {
        self.rotate_left(n)
    }
}

impl DataType for u64 {
    const BITS: usize = u64::BITS as usize;
}

impl Integer for u64 {
    const MAX: Self = u64::MAX;
    const ONE: Self = 1;
    const ZERO: Self = 0;

    #[inline]
    fn count_ones(self) -> usize {
        self.count_ones() as usize
    }

    #[inline]
    fn rotate_left(self, n: u32) -> Self {
        self.rotate_left(n)
    }
}
