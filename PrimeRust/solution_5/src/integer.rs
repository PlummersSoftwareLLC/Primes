use std::ops::{BitAnd, BitAndAssign, Not, Shl};

pub trait Integer:
    Sized
    + Clone
    + Copy
    + Send
    + Sync
    + Eq
    + Shl<usize, Output = Self>
    + BitAnd<Output = Self>
    + BitAndAssign
    + Not<Output = Self>
{
    const BITS: usize;
    const MAX: Self;
    const ONE: Self;
    const ZERO: Self;

    fn count_ones(self) -> usize;
}

impl Integer for u8 {
    const BITS: usize = u8::BITS as usize;
    const MAX: Self = u8::MAX;
    const ONE: Self = 1;
    const ZERO: Self = 0;

    #[inline]
    fn count_ones(self) -> usize {
        self.count_ones() as usize
    }
}

impl Integer for u32 {
    const BITS: usize = u32::BITS as usize;
    const MAX: Self = u32::MAX;
    const ONE: Self = 1;
    const ZERO: Self = 0;

    #[inline]
    fn count_ones(self) -> usize {
        self.count_ones() as usize
    }
}

impl Integer for u64 {
    const BITS: usize = u64::BITS as usize;
    const MAX: Self = u64::MAX;
    const ONE: Self = 1;
    const ZERO: Self = 0;

    #[inline]
    fn count_ones(self) -> usize {
        self.count_ones() as usize
    }
}
