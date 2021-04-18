use std::collections::HashMap;
pub use std::time::{Duration, Instant};

pub struct PrimeSieve {
    sieve_size: usize,
    raw_bits: Vec<bool>,
    prime_counts: HashMap<usize, usize>,
}

impl PrimeSieve {
    pub fn new(limit: usize) -> PrimeSieve {
        PrimeSieve {
            sieve_size: limit,
            raw_bits: vec![true; limit],
            prime_counts: [
                (10, 4),
                (100, 25),
                (1000, 168),
                (10000, 1229),
                (100000, 9592),
                (1000000, 78498),
                (10000000, 664579),
                (100000000, 5761455),
                (1000000000, 50847534),
                (10000000000, 455052511),
            ]
            .iter()
            .cloned()
            .collect(),
        }
    }

    pub fn run_sieve(&self) {
        let mut raw_bits: Vec<bool> = vec![true; self.sieve_size];

        let q = (self.sieve_size as f32).sqrt().round() as usize;
        let mut factor = 3;

        while factor < q {
            for num in factor..self.sieve_size {
                if raw_bits[num] {
                    factor = num;
                    break;
                }
            }
            for num in (factor * factor)..self.sieve_size {
                raw_bits[num] = false;
            }
            factor += 2;
        }
    }

    fn count_primes(&self) -> usize {
        let mut count: usize = 0;
        for i in &self.raw_bits[3..] {
            if *i {
                count += 1;
            }
        }
        return count;
    }

    fn validate_results(&self) -> bool {
        if self.prime_counts.contains_key(&self.sieve_size) {
            return self.prime_counts.get(&self.sieve_size) == Some(&self.count_primes());
        }
        return false;
    }

    pub fn print_results(self, show_results: bool, duration: Duration, passes: u32) {
        if show_results {
            print!("2, ");
        }
        let mut count = 1;

        for num in 3..self.sieve_size {
            if self.raw_bits[num] && show_results {
                print!("{}, ", num);
            }
            count += 1;
        }
        println!("");
        print!("Passes: {}, ", passes);
        print!("Time: {:?}, ", duration);
        print!("Avg: {:?}, ", duration / passes);
        print!("Limit: {}, ", self.sieve_size);
        print!("Count1: {}, ", count);
        print!("Count2: {}, ", self.count_primes());
        println!("Valid: {}", self.validate_results());
    }
}
