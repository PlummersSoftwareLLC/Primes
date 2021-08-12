//! Normal sieving of bit-sized flags.

use super::{FlagData, FlagDataBase, FlagDataExecute};
use crate::Integer;

use rayon::prelude::*;

/// Marker for bit handling of flag data.
///
/// Sieving functions as normal, each flag only occupies one bit so the data is effectively
/// compressed to the minimal possible size.
pub struct Bit;

impl<D: Integer> FlagDataExecute<D> for FlagData<Bit, D> {
    const ID_STR: &'static str = "bit";
    const FLAG_SIZE: usize = 1;
    const INIT_VALUE: D = D::MAX;
    const BITS: usize = D::BITS;

    #[inline]
    fn new(size: usize) -> Self {
        Self::allocate(((size + 1) / 2 + D::BITS - 1) / D::BITS)
    }

    #[inline]
    fn fall_through(data: &mut [D], start: usize, interval: usize) {
        let mut i = start;

        while i < data.len() * D::BITS {
            unsafe {
                *data.get_unchecked_mut(i / D::BITS) &= !(D::ONE << (i % D::BITS));
            }
            i += interval;
        }
    }

    #[inline]
    fn is_prime(&self, index: usize) -> bool {
        (self.0[index / D::BITS] & (D::ONE << (index % D::BITS))) != D::ZERO
    }

    fn flag_count(&self) -> usize {
        self.0.len() * D::BITS
    }

    fn count_primes(&self, size: usize) -> usize {
        let overshoot_amount = ((size + 1) / 2) % D::BITS;
        let overshoot = if overshoot_amount != 0 {
            (*self.0.last().unwrap() & (D::MAX << overshoot_amount)).count_ones()
        } else {
            0
        };

        self.0
            .as_parallel_slice()
            .into_par_iter()
            .map(|entry| entry.count_ones())
            .sum::<usize>()
            - overshoot
    }
}
