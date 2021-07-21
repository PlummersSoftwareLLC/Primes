//! Multithreaded algorithm with bad data locality. Performs unsetting prime flags in parallel, but
//! reassigns threads for each new prime. If you're very lucky, a thread could get the same memory
//! location as before, speeding it up.

use crate::{BitSieve, BoolSieve, Sieve, Streamed};

use rayon::prelude::*;

const USIZE_PER_CACHE_LINE: usize = 64 / std::mem::size_of::<usize>();

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
            let batch_size = (((self.data.len() - offset + rayon::current_num_threads() - 1)
                / rayon::current_num_threads()
                + USIZE_PER_CACHE_LINE
                - 1)
                & !(USIZE_PER_CACHE_LINE - 1))
                // each thread gets at least its own cache line
                .max(USIZE_PER_CACHE_LINE);

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

    fn thread_count(&self) -> usize {
        std::cmp::min(
            self.data.len() / USIZE_PER_CACHE_LINE,
            rayon::current_num_threads(),
        )
    }
}

impl Sieve<Streamed> for BoolSieve<Streamed> {
    #[inline]
    fn new(size: usize, algorithm: Streamed) -> Self {
        assert!(size >= 2);
        // we know that multiples of two are no primes, so we don't need to save them
        let data_size = (size + 1) / 2;
        let mut data = unsafe {
            Vec::from_raw_parts(
                std::alloc::alloc(
                    std::alloc::Layout::from_size_align(
                        std::mem::size_of::<bool>() * data_size,
                        64,
                    )
                    .unwrap(),
                ) as *mut bool,
                data_size,
                data_size,
            )
            .into_boxed_slice()
        };
        data.as_parallel_slice_mut()
            .par_chunks_mut(64)
            .for_each(|slice| {
                slice.fill(true);
            });

        BoolSieve {
            data,
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
            let batch_size = (((self.data.len() - start_number + rayon::current_num_threads()
                - 1)
                / rayon::current_num_threads()
                + 63)
                & !63)
                .max(64);

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

    fn thread_count(&self) -> usize {
        std::cmp::min(self.data.len() / 64, rayon::current_num_threads())
    }
}

impl BitSieve<Streamed> {
    /// Parallel, but non-tiled initialization. Works great on compute-bound workloads, but this is
    /// memory bound.
    #[inline(always)]
    fn initialize_data(size: usize) -> Box<[usize]> {
        let mut data = unsafe {
            Vec::from_raw_parts(
                std::alloc::alloc(
                    std::alloc::Layout::from_size_align(std::mem::size_of::<usize>() * size, 64)
                        .unwrap(),
                ) as *mut usize,
                size,
                size,
            )
            .into_boxed_slice()
        };
        data.as_parallel_slice_mut()
            // for each cache line
            .par_chunks_mut(USIZE_PER_CACHE_LINE)
            .for_each(|slice| {
                slice.fill(usize::MAX);
            });

        // Memory is initialized here
        data
    }
}
