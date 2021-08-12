use super::{calculate_batch_size, calculate_block_offset, Algorithm};
use crate::sieve::{FlagDataExecute, Sieve, SieveBase, SieveExecute};
use crate::DataType;

use rayon::prelude::*;

#[derive(Clone, Copy)]
pub struct Stream;

impl Algorithm for Stream {
    const ID_STR: &'static str = "stream";
}

impl<F: FlagDataExecute<D>, D: DataType> SieveExecute<Stream> for Sieve<Stream, F, D> {
    #[inline]
    fn sieve(&mut self) {
        let slice_size =
            ((self.data.flag_count() * F::FLAG_SIZE + F::BITS - 1) / F::BITS).max(64 * 8 / F::BITS);
        self.data
            .slice()
            .par_chunks_mut(slice_size)
            .for_each(|slice| {
                for n in slice {
                    *n = F::INIT_VALUE;
                }
            });

        let sqrt = (self.size as f64).sqrt() as usize;
        let mut prime = 3;

        while prime <= sqrt {
            let start_index = prime * prime / 2;
            let data_offset = start_index / (F::BITS / Self::FLAG_SIZE);
            let batch_size =
                calculate_batch_size::<D>(self.data.slice().len() - data_offset, usize::MAX);

            self.data.slice()[data_offset..]
                .par_chunks_mut(batch_size)
                .enumerate()
                .for_each(|(i, slice)| {
                    let offset = (data_offset + i * batch_size) * (F::BITS / F::FLAG_SIZE);
                    let start_index = calculate_block_offset(start_index, offset, prime);

                    F::fall_through(slice, start_index, prime);
                });

            prime = (prime / 2 + 1..self.size / 2)
                .find(|n| self.data.is_prime(*n))
                .unwrap()
                * 2
                + 1;
        }

        self.sieved = true;
    }

    fn thread_count(&self) -> usize {
        std::cmp::min(
            (self.size + 1) / 2 * Self::FLAG_SIZE / 64,
            rayon::current_num_threads(),
        )
    }
}
