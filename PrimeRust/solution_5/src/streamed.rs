//! Multithreaded algorithm with bad data locality. Performs unsetting prime flags in parallel, but
//! reassigns threads for each new prime. If you're very lucky, a thread could get the same memory
//! location as before, speeding it up.

use crate::{BitSieve, BoolSieve, Sieve, Streamed};

use rayon::prelude::*;
use std::mem::MaybeUninit;

impl Sieve<Streamed> for BitSieve<Streamed> {
    #[inline]
    fn new(size: usize, algorithm: Streamed) -> Self {
        assert!(size >= 2);
        // we know that multiples of two are no primes, so we don't need to save them
        let data_size = ((size + 1) / 2 + usize::BITS as usize - 1) / usize::BITS as usize;

        BitSieve {
            data: Self::initialize_data(data_size),
            size,
            sieved: false,
            algorithm,
        }
    }

    #[inline]
    fn sieve(&mut self) {
        let sqrt = (self.size as f64).sqrt() as usize;
        let mut flag = 1;
        let mut prime = 3;

        while prime <= sqrt {
            let start_number = prime * prime / 2;
            let offset = start_number / usize::BITS as usize;
            // memory to set / thread count, rounding up so no threads needs to pick the last few
            // numbers up
            let batch_size = ((self.data.len() - offset + rayon::current_num_threads() - 1)
                / rayon::current_num_threads())
            .max(1 << 6); // Too many threads shouldn't share too little memory

            self.data[offset..]
                .par_chunks_mut(batch_size)
                .enumerate()
                .for_each(|(i, slice)| {
                    // Gets the first bit of the current slice that is a prime number
                    let offset = (offset + i * batch_size) * usize::BITS as usize;
                    let mut check_number = if offset <= start_number {
                        start_number - offset
                    } else {
                        let reset_offset = (offset + 1 + prime / 2) % prime;
                        if reset_offset == 0 {
                            0
                        } else {
                            prime - reset_offset
                        }
                    };

                    while check_number < slice.len() * usize::BITS as usize {
                        let word =
                            unsafe { slice.get_unchecked_mut(check_number / usize::BITS as usize) };
                        *word &= !(1 << (check_number % usize::BITS as usize));

                        check_number += prime;
                    }
                });

            flag = Self::next_prime_index(&self.data, self.size, flag);
            prime = flag * 2 + 1;
        }

        self.sieved = true;
    }

    fn get_id_string(&self) -> &'static str {
        "streamed-bit"
    }

    fn thread_count() -> usize {
        rayon::current_num_threads()
    }
}

impl Sieve<Streamed> for BoolSieve<Streamed> {
    #[inline]
    fn new(size: usize, algorithm: Streamed) -> Self {
        assert!(size >= 2);
        let data_size = (size + 1) / 2;
        let mut data = vec![MaybeUninit::<bool>::uninit(); data_size].into_boxed_slice();
        data.as_parallel_slice_mut()
            .into_par_iter()
            .for_each(|element| {
                *element = MaybeUninit::new(true);
            });

        BoolSieve {
            data: unsafe { std::mem::transmute(data) },
            size,
            sieved: false,
            algorithm,
        }
    }

    #[inline]
    fn sieve(&mut self) {
        let sqrt = (self.size as f64).sqrt() as usize;
        let mut flag = 1;
        let mut prime = 3;

        while prime <= sqrt {
            let start_number = prime * prime / 2;
            let batch_size = ((self.data.len() - start_number + rayon::current_num_threads() - 1)
                / rayon::current_num_threads())
            .max(1 << 12); // 4096 byte are the minimum working set

            self.data[start_number..]
                .par_chunks_mut(batch_size)
                .enumerate()
                .for_each(|(i, slice)| {
                    // Gets the first bit of the current slice that is a prime number
                    let offset = start_number + i * batch_size;
                    let mut check_number = if offset <= start_number {
                        start_number - offset
                    } else {
                        let reset_offset = (offset + 1 + prime / 2) % prime;
                        if reset_offset == 0 {
                            0
                        } else {
                            prime - reset_offset
                        }
                    };

                    while check_number < slice.len() {
                        unsafe { *slice.get_unchecked_mut(check_number) = false };
                        check_number += prime;
                    }
                });

            flag += self.data[flag + 1..].iter().position(|&flag| flag).unwrap() + 1;
            prime = flag * 2 + 1;
        }

        self.sieved = true;
    }

    fn get_id_string(&self) -> &'static str {
        "streamed-bool"
    }

    fn thread_count() -> usize {
        rayon::current_num_threads()
    }
}

impl BitSieve<Streamed> {
    /// Parallel, but non-tiled initialization. Works great on compute-bound workloads, but this is
    /// memory bound.
    #[inline(always)]
    fn initialize_data(size: usize) -> Box<[usize]> {
        let mut data = vec![MaybeUninit::<usize>::uninit(); size].into_boxed_slice();
        data.as_parallel_slice_mut()
            .into_par_iter()
            .for_each(|element| {
                *element = MaybeUninit::new(usize::MAX);
            });

        unsafe { std::mem::transmute(data) }
    }
}
