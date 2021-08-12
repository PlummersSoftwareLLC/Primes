//! Striped bit-sieving using blocks of elements.

use super::{FlagData, FlagDataBase, FlagDataExecute};
use crate::DataType;

/// Marker for striped bit handling of flag data.
///
/// This defines an array of N elements as a block and instead of saving consecutive numbers as
/// different bits of the same element, it saves them as the same bit for different elements.
/// This enables it to sieve through each "stripe" of the block without needing to recalculate the
/// mask. On the other hand, those blocks enforce a much coarser graining for thread work units
/// since only one thread can be active in a block at a time to prevent data collisions.
///
/// This aligns more with the `FlagStorageBitVectorStripedBlocks` struct from `solution_1`, instead
/// of the `FlagStorageBitVectorStriped` struct. The reason is that striping the entire sieve
/// doesn't make sense for this solution since it would basically prevent any multi-threaded access.
pub struct Stripe;

/// The element count of each striped block.
///
/// This size is a compromise between speed and scalability; larger blocks have less overhead and
/// thus perform faster, but smaller blocks allow finer-grained distribution of work to threads.
/// For this reason, a smaller block size than the one used by `solution_1` is selected.
pub const STRIPE_SIZE: usize = 1024;
/// The bit and thus flag count of a striped block.
const STRIPE_BITS: usize = STRIPE_SIZE * u8::BITS as usize;

impl FlagDataExecute<[u8; STRIPE_SIZE]> for FlagData<Stripe, [u8; STRIPE_SIZE]> {
    const ID_STR: &'static str = "stripe";
    const FLAG_SIZE: usize = 1;
    const INIT_VALUE: [u8; STRIPE_SIZE] = [u8::MAX; STRIPE_SIZE];
    const BITS: usize = STRIPE_BITS;

    #[inline]
    fn new(size: usize) -> Self {
        Self::allocate(((size + 1) / 2 + STRIPE_BITS - 1) / STRIPE_BITS)
    }

    #[inline]
    fn fall_through(data: &mut [[u8; STRIPE_SIZE]], start: usize, interval: usize) {
        let unroll_limit = STRIPE_SIZE.saturating_sub(3 * interval);
        let mut bit = start % STRIPE_BITS / STRIPE_SIZE;
        let mut stripe_index = start % STRIPE_SIZE;

        for stripe in data.iter_mut().skip(start / STRIPE_BITS) {
            while bit < u8::BITS as usize {
                let mask = !(1 << bit);

                while stripe_index < unroll_limit {
                    unsafe {
                        *stripe.get_unchecked_mut(stripe_index) &= mask;
                        *stripe.get_unchecked_mut(stripe_index + interval) &= mask;
                        *stripe.get_unchecked_mut(stripe_index + interval * 2) &= mask;
                        *stripe.get_unchecked_mut(stripe_index + interval * 3) &= mask;
                    }

                    stripe_index += interval * 4;
                }

                while stripe_index < STRIPE_SIZE {
                    unsafe {
                        *stripe.get_unchecked_mut(stripe_index) &= mask;
                    }

                    stripe_index += interval;
                }

                stripe_index -= STRIPE_SIZE;
                bit += 1;
            }

            bit = 0;
        }
    }

    #[inline]
    fn is_prime(&self, index: usize) -> bool {
        self.0[index / STRIPE_BITS][index % STRIPE_SIZE]
            & (1 << (index % STRIPE_BITS / STRIPE_SIZE))
            != 0
    }

    fn flag_count(&self) -> usize {
        self.0.len() * STRIPE_BITS
    }

    fn count_primes(&self, size: usize) -> usize {
        (0..(size + 1) / 2).filter(|n| self.is_prime(*n)).count()
    }
}

impl DataType for [u8; STRIPE_SIZE] {
    const BITS: usize = STRIPE_BITS;
}
