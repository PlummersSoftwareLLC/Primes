use std::time::{Duration, Instant};

mod primes {
    use std::{collections::HashMap, time::Duration};

    // pulled prime validator out into a separate struct, as it's defined
    // `const` in C++. There are various ways to do this in Rust, including
    // lazy_static, etc. Should be able to do the const initialisation in the future.
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

    pub struct PrimeSieve {
        sieve_size: usize,
        bits: Vec<u8>,
    }

    impl PrimeSieve {
        pub fn new(sieve_size: usize) -> Self {
            let num_words = sieve_size / 8 / 2 + 1;
            PrimeSieve {
                sieve_size,
                bits: vec![0xff; num_words],
            }
        }

        // unsafe: ensure `number` is within bounds 1..sieve_size
        unsafe fn clear_bit(&mut self, number: usize) {
            assert!(
                number % 2 != 0,
                "You're setting even bits, which is sub-optimal."
            );
            let index = number / 2;

            debug_assert!(index / 8 < self.bits.len(), "bounds check");
            let word = self.bits.get_unchecked_mut(index / 8);
            *word &= !(1 << (index % 8));
        }

        // unsafe: ensure `number` is within bounds 1..sieve_size
        unsafe fn get_bit(&self, number: usize) -> bool {
            if number % 2 == 0 {
                return false;
            }
            let index = number / 2;

            debug_assert!(index / 8 < self.bits.len(), "bounds check");
            let word = self.bits.get_unchecked(index / 8);
            *word & (1 << (index % 8)) != 0
        }

        // count number of primes (not optimal, but doesn't need to be)
        pub fn count_primes(&self) -> usize {
            (1..self.sieve_size)
                .filter(|v| unsafe { self.get_bit(*v) })
                .count()
        }

        // calculate the primes up to the specified limit
        pub fn run_sieve(&mut self) {
            let mut factor = 3;
            let q = (self.sieve_size as f32).sqrt() as usize;

            // note: need to check up to and including q, otherwise we
            // fail to catch cases like sieve_size = 1000
            while factor <= q {
                for num in factor..self.sieve_size {
                    // length already checked
                    unsafe {
                        if self.get_bit(num) {
                            factor = num;
                            break;
                        }
                    }
                }

                // If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
                // We can then step by factor * 2 because every second one is going to be even by definition
                let mut num = factor * 3;
                while num < self.sieve_size {
                    // length already checked
                    unsafe {
                        self.clear_bit(num);
                    }
                    num += factor * 2;
                }

                factor += 2;
            }
        }

        pub fn print_results(
            &self,
            show_results: bool,
            duration: Duration,
            passes: usize,
            validator: &PrimeValidator,
        ) {
            if show_results {
                print!("2,");

                for num in 3..self.sieve_size {
                    if unsafe { self.get_bit(num) } {
                        print!("{},", num);
                    }
                }
                print!("\n");
            }

            let count = self.count_primes();

            println!(
                "Passes: {}, Time: {}, Avg: {}, Limit: {}, Count: {}, Valid: {}",
                passes,
                duration.as_secs_f32(),
                duration.as_secs_f32() / passes as f32,
                self.sieve_size,
                count,
                validator.is_valid(self.sieve_size, self.count_primes())
            );
        }
    }
}

fn main() {
    let mut passes = 0;
    let mut prime_sieve = None;

    let start_time = Instant::now();
    let run_duration = Duration::from_secs(10);
    while (Instant::now() - start_time) < run_duration {
        let mut sieve = primes::PrimeSieve::new(1000000);
        sieve.run_sieve();
        prime_sieve.replace(sieve);
        passes += 1;
    }
    let end_time = Instant::now();

    if let Some(sieve) = prime_sieve {
        sieve.print_results(
            false,
            end_time - start_time,
            passes,
            &primes::PrimeValidator::default(),
        );
    }
}

#[cfg(test)]
mod tests {
    use crate::primes::PrimeValidator;

    use super::*;

    #[test]
    fn sieve_known_correct() {
        let validator = PrimeValidator::default();
        for (sieve_size, expected_primes) in validator.known_results().iter() {
            let mut sieve = primes::PrimeSieve::new(*sieve_size);
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
