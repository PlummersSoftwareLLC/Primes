//! Multithreaded algorithm with good data locality. Threading efficiency is hampered because work
//! on the sieve tends to move around in memory, while the threads have a static area. Before going
//! parallel, all needed primes for unsetting the correct flags are gathered by a single thread.

use crate::{BitSieve, BoolSieve, Sieve, Tiled};

use rayon::prelude::*;
use std::mem::MaybeUninit;

impl Sieve<Tiled> for BitSieve<Tiled> {
    #[inline]
    fn new(size: usize, algorithm: Tiled) -> Self {
        assert!(size >= 2);
        // we know that multiples of two are no primes, so we don't need to save them
        let data_size = ((size + 1) / 2 + usize::BITS as usize - 1) / usize::BITS as usize;
        let thread_chunk_size =
            ((data_size + rayon::current_num_threads() - 1) / rayon::current_num_threads()).max(1);

        BitSieve {
            data: Self::initialize_data(data_size, thread_chunk_size),
            size,
            sieved: false,
            algorithm,
        }
    }

    #[inline]
    fn sieve(&mut self) {
        let sqrt = (self.size as f64).sqrt() as usize;
        let initial_prime_count = (sqrt + usize::BITS as usize - 1) / usize::BITS as usize;
        let thread_chunk_size =
            ((self.data.len() - initial_prime_count + rayon::current_num_threads() - 1)
                / rayon::current_num_threads())
            // limit working set to supplied memory size
            .min(self.algorithm.0 * 8 / usize::BITS as usize)
            .max(1);
        // Gathering of primes by a single thread
        let base_primes = self.find_primes(initial_prime_count);

        self.data[initial_prime_count..]
            .par_chunks_mut(thread_chunk_size)
            .into_par_iter()
            .enumerate()
            .for_each(|(i, slice)| {
                let offset = (initial_prime_count + i * thread_chunk_size) * usize::BITS as usize;
                for prime in &base_primes {
                    let start_number = prime * prime / 2;
                    Self::fall_through_offset(slice, offset, start_number, *prime);
                }
            });

        self.sieved = true;
    }

    fn get_id_string(&self) -> &'static str {
        "tiled-bit"
    }

    fn thread_count() -> usize {
        rayon::current_num_threads()
    }
}

impl Sieve<Tiled> for BoolSieve<Tiled> {
    #[inline]
    fn new(size: usize, algorithm: Tiled) -> Self {
        assert!(size >= 2);
        // we know that multiples of two are no primes, so we don't need to save them
        let data_size = (size + 1) / 2;
        let thread_chunk_size =
            ((data_size + rayon::current_num_threads() - 1) / rayon::current_num_threads()).max(1);
        let mut data = vec![MaybeUninit::<bool>::uninit(); data_size].into_boxed_slice();
        data.as_parallel_slice_mut()
            .par_chunks_mut(thread_chunk_size)
            .for_each(|slice| {
                slice.fill(MaybeUninit::new(true));
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
        let thread_chunk_size = ((self.data.len() - sqrt + rayon::current_num_threads() - 1)
            / rayon::current_num_threads())
        // limit working set to supplied memory size
        .min(self.algorithm.0)
        .max(1);
        // Gathering of primes by a single thread
        let base_primes = self.find_primes(sqrt);

        self.data[sqrt..]
            .par_chunks_mut(thread_chunk_size)
            .into_par_iter()
            .enumerate()
            .for_each(|(i, slice)| {
                let offset = sqrt + i * thread_chunk_size;
                for prime in &base_primes {
                    // Find the first prime number in the current memory region
                    let start_number = prime * prime / 2;
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
                }
            });

        self.sieved = true;
    }

    fn get_id_string(&self) -> &'static str {
        "tiled-bool"
    }

    fn thread_count() -> usize {
        rayon::current_num_threads()
    }
}

impl BitSieve<Tiled> {
    /// Initializes the array by giving each thread their own area, avoiding cross talk.
    #[inline(always)]
    fn initialize_data(size: usize, thread_chunk_size: usize) -> Box<[usize]> {
        let mut data = vec![MaybeUninit::<usize>::uninit(); size].into_boxed_slice();
        data.as_parallel_slice_mut()
            .par_chunks_mut(thread_chunk_size)
            .for_each(|slice| {
                slice.fill(MaybeUninit::new(usize::MAX));
            });

        // Memory is initialized here
        unsafe { std::mem::transmute(data) }
    }

    /// Basically the same algorithm as the single threaded one, but collects found primes.
    #[inline(always)]
    fn find_primes(&mut self, cutoff: usize) -> Vec<usize> {
        // found this by trying out stuff in libreoffice
        // Maybe replace this with something faster?
        let log_length = ((cutoff * usize::BITS as usize) as f64).log10();
        let space_amount = ((2.0_f64.powf((log_length * 0.04 + 2.49) * log_length) as usize
            + usize::BITS as usize
            - 1)
            / usize::BITS as usize
            + 1)
            / 2;
        let mut primes = Vec::with_capacity(space_amount);

        // The same algorithm
        let data_size = cutoff * usize::BITS as usize;
        let sqrt = ((data_size * 2 + 1) as f64).sqrt() as usize;
        let mut bit = 1;

        while bit <= sqrt {
            let prime = bit * 2 + 1;

            Self::fall_through(&mut self.data[..cutoff], data_size, prime);

            primes.push(prime);
            bit = Self::next_prime_index(&self.data, data_size, bit);
        }

        // Gather all remaining primes of the memory region
        for bit in bit..data_size {
            let word = unsafe { *self.data.get_unchecked_mut(bit / usize::BITS as usize) };
            if (word & (1 << (bit % usize::BITS as usize))) != 0 {
                let prime = bit * 2 + 1;
                primes.push(prime);
            }
        }

        primes
    }

    #[inline(always)]
    fn fall_through_offset(data: &mut [usize], offset: usize, start_number: usize, prime: usize) {
        // Find the first prime number in the current memory region
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

        while check_number < data.len() * usize::BITS as usize {
            let word = unsafe { data.get_unchecked_mut(check_number / usize::BITS as usize) };
            *word &= !(1 << (check_number % usize::BITS as usize));

            check_number += prime;
        }
    }
}

impl BoolSieve<Tiled> {
    /// Basically the same algorithm as the single threaded one, but collects found primes.
    #[inline(always)]
    fn find_primes(&mut self, cutoff: usize) -> Vec<usize> {
        // found this by trying out stuff in libreoffice
        let log_length = (cutoff as f64).log10();
        let space_amount = (2.0_f64.powf((log_length * 0.04 + 2.49) * log_length) as usize + 1) / 2;
        let mut primes = Vec::with_capacity(space_amount);

        // The same algorithm
        let sqrt = (self.size as f64).sqrt() as usize;
        let mut flag = 1;
        let mut prime = flag * 2 + 1;

        while prime <= sqrt {
            let mut i = prime * prime / 2;
            while i < self.data.len() {
                unsafe {
                    *self.data.get_unchecked_mut(i) = false;
                }
                i += prime;
            }

            primes.push(prime);
            flag += self.data[flag + 1..].iter().position(|&flag| flag).unwrap() + 1;
            prime = flag * 2 + 1;
        }

        // Gather all remaining primes of the memory region
        for flag in flag..cutoff {
            if unsafe { *self.data.get_unchecked_mut(flag) } {
                let prime = flag * 2 + 1;
                primes.push(prime);
            };
        }

        primes
    }
}
