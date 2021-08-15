//! Multi-threaded sieving passes.

use super::{calculate_batch_size, calculate_block_offset, Algorithm};
use crate::sieve::{FlagDataExecute, Sieve, SieveBase, SieveExecute};
use crate::DataType;

use rayon::prelude::*;

/// Marker for streamed execution.
///
/// This algorithm is a mostly naive implementation of a multi-threaded algorithm for sieving. It
/// searches for the next prime, then performs a multi-threaded pass over the whole sieve to sieve
/// out the multiples. For this, the sieve portion that needs sieving in that pass is divided evenly
/// among the threads.
///
/// This method has very high overhead since for each found prime, the sieve gets divided anew and
/// threads are provided with a new piece of memory they may or may not already have seen. Combined,
/// it is only effective for very large sieve sizes. Lower-than-single-threaded performance is to be
/// expected in the competition, this mostly exists as a showcase.
#[derive(Clone, Copy)]
pub struct Stream;

impl Algorithm for Stream {
    const ID_STR: &'static str = "stream";
}

impl<F: FlagDataExecute<D>, D: DataType> SieveExecute<Stream> for Sieve<Stream, F, D> {
    #[inline]
    fn sieve(&mut self) {
        // flag data initialization
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

        // main loop
        let sqrt = (self.size as f64).sqrt() as usize;
        let mut prime = 3;

        while prime <= sqrt {
            let start_index = prime * prime / 2;
            let data_offset = start_index / (F::BITS / Self::FLAG_SIZE);
            let batch_size =
                calculate_batch_size::<D>(self.data.slice().len() - data_offset, usize::MAX);

            // parallel sieving pass
            self.data.slice()[data_offset..]
                .par_chunks_mut(batch_size)
                .enumerate()
                .for_each(|(i, slice)| {
                    let offset = (data_offset + i * batch_size) * (F::BITS / F::FLAG_SIZE);
                    let start_index = calculate_block_offset(start_index, offset, prime);

                    F::fall_through(slice, start_index, prime);
                });

            // single threaded prime search
            prime = (prime / 2 + 1..self.size / 2)
                .find(|n| self.data.is_prime(*n))
                .unwrap()
                * 2
                + 1;
        }

        self.sieved = true;
    }

    fn thread_count(&self) -> usize {
        let data_size =
            ((self.data.flag_count() * F::FLAG_SIZE + F::BITS - 1) / F::BITS).max(64 * 8 / F::BITS);
        let batch_size = calculate_batch_size::<D>(data_size, usize::MAX);

        println!("Batch size {}", batch_size);

        std::cmp::min(
            (data_size + batch_size - 1) / batch_size,
            rayon::current_num_threads(),
        )
    }
}
