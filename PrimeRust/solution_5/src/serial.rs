//! Single threaded algorithm. Might be quite similar to other solutions.

use crate::{BitSieve, BoolSieve, Serial, Sieve};

impl Sieve<Serial> for BitSieve<Serial> {
    #[inline]
    fn new(size: usize, algorithm: Serial) -> Self {
        assert!(size >= 2);
        // we know that multiples of two are no primes, so we don't need to save them
        let data_size = (size + 1) / 2;

        let data =
            vec![usize::MAX; (data_size + usize::BITS as usize - 1) / usize::BITS as usize].into();
        BitSieve {
            data,
            size,
            sieved: false,
            algorithm,
        }
    }

    #[inline]
    fn sieve(&mut self) {
        // gets scaled down so it works with flag bits instead of the real numbers
        let sqrt = ((self.size as f64).sqrt() as usize + 1) / 2;
        let data_size = (self.size + 1) / 2;
        let mut flag = 1;

        while flag <= sqrt {
            let prime = flag * 2 + 1;

            Self::fall_through(&mut self.data, data_size, prime);

            flag = Self::next_prime_index_block(&self.data, flag);
        }

        self.sieved = true;
    }

    fn get_id_string(&self) -> &'static str {
        "serial-bit"
    }

    fn thread_count(&self) -> usize {
        1
    }
}

impl Sieve<Serial> for BoolSieve<Serial> {
    #[inline]
    fn new(size: usize, algorithm: Serial) -> Self {
        assert!(size >= 2);
        // we know that multiples of two are no primes, so we don't need to save them
        let data_size = (size + 1) / 2;

        let data = vec![true; data_size].into();
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
        let mut prime = flag * 2 + 1;

        while prime <= sqrt {
            let mut i = prime * prime / 2;
            while i < self.data.len() {
                // remove bound checks, since they have been guaranteed by the loop
                unsafe {
                    *self.data.get_unchecked_mut(i) = false;
                }
                i += prime;
            }

            flag += self.data[flag + 1..].iter().position(|&flag| flag).unwrap() + 1;
            prime = flag * 2 + 1;
        }

        self.sieved = true;
    }

    fn get_id_string(&self) -> &'static str {
        "serial-bool"
    }

    fn thread_count(&self) -> usize {
        1
    }
}
