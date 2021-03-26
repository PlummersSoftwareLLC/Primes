use std::{
    collections::HashMap,
    time::{Duration, Instant},
};

use bit_vec::BitVec;

// pulled prime validator out into a separate struct, as it's defined
// `const` in C++. There are various ways to do this in Rust, including
// lazy_static, etc. Should be able to do the const initialisation in the future.
struct PrimeValidator(HashMap<usize, usize>);
impl Default for PrimeValidator {
    fn default() -> Self {
        let map = [
            (10, 1),   // Historical data for validating our results - the number of primes
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
    fn is_valid(&self, sieve_size: usize, result: usize) -> bool {
        if let Some(&expected) = self.0.get(&sieve_size) {
            result == expected
        } else {
            false
        }
    }
}

struct PrimeSieve {
    sieve_size: usize,
    bits: BitVec,
}

impl PrimeSieve {
    fn new(sieve_size: usize) -> Self {
        PrimeSieve {
            sieve_size,
            bits: BitVec::from_elem((sieve_size + 1) / 2, true),
        }
    }

    fn clear_bit(&mut self, number: usize) {
        assert!(number % 2 != 0, "You're setting even bits, which is sub-optimal.");
        let index = number / 2;
        self.bits.set(index, false);
    }

    fn get_bit(&mut self, number: usize) -> bool {
        match number % 2 {
            0 => false,
            _ => self.bits.get(number / 2).expect("index out of bounds")
        }
    }

    // number of primes -> count number of true bits
    fn count_primes(&self) -> usize {
        self.bits.iter().filter(|x| *x).count()
    }

    fn validate_results(&self, validator: &PrimeValidator) -> bool {
        validator.is_valid(self.sieve_size, self.count_primes())
    }

    // Calculate the primes up to the specified limit
    fn run_sieve(&mut self) {
        let mut factor = 3;
        let q = (self.sieve_size as f32).sqrt() as usize;

        while factor < q {
            for num in factor..self.sieve_size {
                if self.get_bit(num) {
                    factor = num;
                    break;
                }
            }

            // If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
            // We can then step by factor * 2 because every second one is going to be even by definition
            let start = factor * 3;
            for num in (start..self.sieve_size).step_by(factor * 2) {
                self.clear_bit(num);
            }

            factor += 2;
        }
    }

    fn print_results(
        &self,
        show_results: bool,
        duration: Duration,
        passes: usize,
        validator: &PrimeValidator,
    ) {
        if show_results {
            print!("2, ");
            self.bits.iter();
            print!("\n");
        }

        let count = self.count_primes();

        println!(
            "Passes: {}, Time: {}, Avg: {}, Limit: {}, Count: {}, Valid: {}",
            passes,
            duration.as_secs_f32(),
            duration.as_micros() / passes as u128,
            self.sieve_size,
            count,
            self.validate_results(validator)
        );
    }
}

fn main() {
    // let start = Instant::now();
    // let end = Instant::now();
    // let dur = end-start;

    let mut passes = 0;
    let mut prime_sieve = None;

    let start_time = Instant::now();
    let run_duration = Duration::from_secs(10);
    while (Instant::now() - start_time) < run_duration {
        let mut sieve = PrimeSieve::new(1000000);
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
            &PrimeValidator::default(),
        );
    }
}
