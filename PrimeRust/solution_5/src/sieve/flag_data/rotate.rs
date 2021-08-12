//! Bit-sieving with a rotating mask.

use super::{FlagData, FlagDataBase, FlagDataExecute};
use crate::Integer;

use rayon::prelude::*;

/// Marker for bit handling with rotating masks.
///
/// This distinguishes itself from the normal bit version by rotating a mask instead of calculating
/// the mask anew for each flag.
pub struct Rotate;

impl<D: Integer> FlagDataExecute<D> for FlagData<Rotate, D> {
    const ID_STR: &'static str = "rotate";
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
        let mut mask = !(D::ONE << (start % D::BITS));

        if interval > D::BITS {
            let roll_amount = (interval * 2 % D::BITS) as u32;
            let limit = (data.len() * D::BITS).saturating_sub(interval);
            let mut mask2 = !(D::ONE << ((start + interval) % D::BITS));
            while i < limit {
                unsafe {
                    *data.get_unchecked_mut(i / D::BITS) &= mask;
                    *data.get_unchecked_mut((i + interval) / D::BITS) &= mask2;
                }
                mask = mask.rotate_left(roll_amount);
                mask2 = mask2.rotate_left(roll_amount);
                i += interval * 2;
            }
        }

        while i < data.len() * D::BITS {
            unsafe {
                *data.get_unchecked_mut(i / D::BITS) &= mask;
            }
            mask = mask.rotate_left(interval as u32);
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
