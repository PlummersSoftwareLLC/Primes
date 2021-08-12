use super::{FlagData, FlagDataBase, FlagDataExecute};
use crate::DataType;

pub struct Stripe;

pub const STRIPE_SIZE: usize = 1024;
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

        for stripe in start / STRIPE_BITS..data.len() {
            let block = &mut data[stripe];

            while bit < u8::BITS as usize {
                let mask = !(1 << bit);

                while stripe_index < unroll_limit {
                    unsafe {
                        *block.get_unchecked_mut(stripe_index) &= mask;
                        *block.get_unchecked_mut(stripe_index + interval) &= mask;
                        *block.get_unchecked_mut(stripe_index + interval * 2) &= mask;
                        *block.get_unchecked_mut(stripe_index + interval * 3) &= mask;
                    }

                    stripe_index += interval * 4;
                }

                while stripe_index < STRIPE_SIZE {
                    unsafe {
                        *block.get_unchecked_mut(stripe_index) &= mask;
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
