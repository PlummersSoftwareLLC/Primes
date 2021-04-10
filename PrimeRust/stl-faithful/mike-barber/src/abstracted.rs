use std::time::{Duration, Instant};

use primes::{print_results, FlagStorage, FlagStorageBitVector, FlagStorageByteVector, PrimeSieve};

pub mod primes {
    use std::{collections::HashMap, time::Duration, usize};

    /// Validator to compare against known primes.
    /// Pulled this out into a separate struct, as it's defined
    /// `const` in C++. There are various ways to do this in Rust, including
    /// lazy_static, etc. Should be able to do the const initialisation in the future.
    pub struct PrimeValidator(HashMap<usize, usize>);
    impl Default for PrimeValidator {
        fn default() -> Self {
            let map = [
                (10, 4),   // Historical data for validating our results - the number of primes
                (100, 25), // to be found under some limit, such as 168 primes under 1000
                (1000, 168),
                (10000, 1229),
                (100000, 9592),
                (1000000, 78498),
                (10000000, 664579),
                (100000000, 5761455),
            ]
            .iter()
            .copied()
            .collect();
            PrimeValidator(map)
        }
    }
    impl PrimeValidator {
        pub fn is_valid(&self, sieve_size: usize, result: usize) -> bool {
            if let Some(&expected) = self.0.get(&sieve_size) {
                result == expected
            } else {
                false
            }
        }

        #[allow(dead_code)]
        pub fn known_results(&self) -> &HashMap<usize, usize> {
            &self.0
        }
    }

    /// Trait defining the interface to different kinds of storage, e.g.
    /// bits within bytes, a vector of bytes, etc.
    pub trait FlagStorage {
        /// create new storage for given number of flags pre-initialised to all true
        fn create_true(size: usize) -> Self;

        /// reset all flags at indices starting at `start` with a stride of `stride`
        fn reset_flags(&mut self, start: usize, skip: usize);

        /// get a specific flag
        fn get(&self, index: usize) -> bool;
    }

    /// Storage using a simple vector of bytes.
    /// Doing the same with bools is equivalent, as bools are currently
    /// represented as bytes in Rust. However, this is not guaranteed to
    /// remain so for all time. To ensure consistent memory use in the future,
    /// we're explicitly using bytes (u8) here.
    pub struct FlagStorageByteVector(Vec<u8>);
    impl FlagStorage for FlagStorageByteVector {
        fn create_true(size: usize) -> Self {
            FlagStorageByteVector(vec![1; size])
        }

        // bounds checks are elided since we're runing up to .len()
        fn reset_flags(&mut self, start: usize, skip: usize) {
            let mut i = start;
            while i < self.0.len() {
                self.0[i] = 0;
                i += skip;
            }
        }

        fn get(&self, index: usize) -> bool {
            if let Some(val) = self.0.get(index) {
                *val == 1
            } else {
                false
            }
        }
    }

    /// Storage using a vector of bytes, but addressing individual bits within each
    pub struct FlagStorageBitVector {
        words: Vec<u8>,
        length_bits: usize,
    }

    const U8_BITS: usize = 8;
    impl FlagStorage for FlagStorageBitVector {
        fn create_true(size: usize) -> Self {
            let num_words = size / U8_BITS + (size % U8_BITS).min(1);
            FlagStorageBitVector {
                words: vec![0xff; num_words],
                length_bits: size,
            }
        }

        fn reset_flags(&mut self, start: usize, skip: usize) {
            let mut i = start;
            while i < self.words.len() * U8_BITS {
                let word_idx = i / U8_BITS;
                let bit_idx = i % U8_BITS;
                // unsafe get_mut_unchecked is superfluous here -- the compiler
                // seems to know that we're within bounds, so it yields no performance
                // benefit.
                *self.words.get_mut(word_idx).unwrap() &= !(1 << bit_idx);
                //unsafe { *self.words.get_unchecked_mut(word_idx) &= !(1 << bit_idx); }
                i += skip;
            }
        }

        fn get(&self, index: usize) -> bool {
            if index >= self.length_bits {
                return false;
            }
            let word = self.words.get(index / U8_BITS).unwrap();
            *word & (1 << (index % U8_BITS)) != 0
        }
    }

    pub struct PrimeSieve<T: FlagStorage> {
        sieve_size: usize,
        flags: T,
    }

    impl<T> PrimeSieve<T>
    where
        T: FlagStorage,
    {
        pub fn new(sieve_size: usize) -> Self {
            let num_flags = sieve_size / 2 + 1;
            PrimeSieve {
                sieve_size,
                flags: T::create_true(num_flags),
            }
        }

        fn is_num_flagged(&self, number: usize) -> bool {
            if number % 2 == 0 {
                return false;
            }
            let index = number / 2;
            self.flags.get(index)
        }

        // count number of primes (not optimal, but doesn't need to be)
        pub fn count_primes(&self) -> usize {
            (1..self.sieve_size)
                .filter(|v| self.is_num_flagged(*v))
                .count()
        }

        // calculate the primes up to the specified limit
        pub fn run_sieve(&mut self) {
            let mut factor = 3;
            let q = (self.sieve_size as f32).sqrt() as usize;

            // note: need to check up to and including q, otherwise we
            // fail to catch cases like sieve_size = 1000
            while factor <= q {
                // find next factor - next still-flagged number
                factor = (factor..self.sieve_size)
                    .find(|n| self.is_num_flagged(*n))
                    .unwrap();

                // reset flags starting at `start`, every `factor`'th flag
                let start = factor * 3 / 2;
                let skip = factor;
                self.flags.reset_flags(start, skip);

                factor += 2;
            }
        }
    }

    pub fn print_results<T: FlagStorage>(
        label: &str,
        prime_sieve: &PrimeSieve<T>,
        show_results: bool,
        duration: Duration,
        passes: usize,
        threads: usize,
        validator: &PrimeValidator,
    ) {
        if show_results {
            print!("2,");
            for num in (3..prime_sieve.sieve_size).filter(|n| prime_sieve.is_num_flagged(*n)) {
                print!("{},", num);
            }
            print!("\n");
        }

        let count = prime_sieve.count_primes();

        println!(
            "{:15} Passes: {}, Threads: {}, Time: {:.10}, Average: {:.10}, Limit: {}, Counts: {}, Valid: {}",
            label,
            passes,
            threads,
            duration.as_secs_f32(),
            duration.as_secs_f32() / passes as f32,
            prime_sieve.sieve_size,
            count,
            match validator.is_valid(prime_sieve.sieve_size, count) {
                true => "Pass",
                false => "Fail",
            }
        );
    }
}

fn main() {
    let limit = 1000000;
    let repetitions = 3;
    let run_duration = Duration::from_secs(5);

    print_header(1, limit, run_duration);
    for _ in 0..repetitions {
        run_implementation::<FlagStorageByteVector>("Byte storage", run_duration, 1, limit);
    }

    println!();
    for _ in 0..repetitions {
        run_implementation::<FlagStorageBitVector>("Bit storage", run_duration, 1, limit);
    }
}

fn print_header(threads: usize, limit: usize, run_duration: Duration) {
    println!();
    println!("Computing primes to {} on {} thread{} for {} second{}.",
        limit,
        threads,
        match threads {
            1 => "",
            _ => "s"
        },
        run_duration.as_secs(),
        match run_duration.as_secs() {
            1 => "",
            _ => "s"
        }
    );
}

fn run_implementation<T: 'static + FlagStorage + Send>(label: &str, run_duration: Duration, num_threads: usize, limit: usize) {
    // spin up N threads; each will terminate itself after `run_duration`, returning
    // the last sieve as well as the total number of counts.
    let start_time = Instant::now();
    let threads: Vec<_> = (0..num_threads)
        .map(|_| {
            std::thread::spawn(move || {
                let mut local_passes = 0;
                let mut last_sieve = None;
                while (Instant::now() - start_time) < run_duration {
                    let mut sieve: PrimeSieve<T> = primes::PrimeSieve::new(limit);
                    sieve.run_sieve();
                    last_sieve.replace(sieve);
                    local_passes += 1;
                }
                // return local pass count and last sieve
                (local_passes, last_sieve)
            })
        })
        .collect();

    // wait for threads to finish, and record end time
    let results: Vec<_> = threads.into_iter().map(|t| t.join().unwrap()).collect();
    let end_time = Instant::now();

    // get totals and print results based on one of the sieves
    let total_passes = results.iter().map(|r| r.0).sum();
    let check_sieve = &results.first().unwrap().1;
    if let Some(sieve) = check_sieve {
        print_results(
            label,
            &sieve,
            false,
            end_time - start_time,
            total_passes,
            num_threads,
            &primes::PrimeValidator::default(),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::primes::{
        FlagStorage, FlagStorageBitVector, FlagStorageByteVector, PrimeSieve, PrimeValidator,
    };

    #[test]
    fn sieve_known_correct_bits() {
        sieve_known_correct::<FlagStorageBitVector>();
    }

    #[test]
    fn sieve_known_correct_bytes() {
        sieve_known_correct::<FlagStorageByteVector>();
    }

    fn sieve_known_correct<T: FlagStorage>() {
        let validator = PrimeValidator::default();
        for (sieve_size, expected_primes) in validator.known_results().iter() {
            let mut sieve: PrimeSieve<T> = primes::PrimeSieve::new(*sieve_size);
            sieve.run_sieve();
            assert_eq!(
                *expected_primes,
                sieve.count_primes(),
                "wrong number of primes for sieve = {}",
                sieve_size
            );
        }
    }
}
