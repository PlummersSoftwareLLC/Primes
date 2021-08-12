use std::ops::{BitAnd, BitAndAssign, Not, Shl};

pub trait DataType: Sized + Clone + Send + Sync {
    const BITS: usize;
}

pub trait Integer:
    DataType
    + Copy
    + Eq
    + Shl<usize, Output = Self>
    + BitAnd<Output = Self>
    + BitAndAssign
    + Not<Output = Self>
{
    const MAX: Self;
    const ONE: Self;
    const ZERO: Self;

    fn count_ones(self) -> usize;

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
