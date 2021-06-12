use std::collections::HashMap;
pub use std::time::{Duration, Instant};

pub struct PrimeSieve {
    sieve_size: usize,
    raw_bits: Vec<bool>,
}

impl PrimeSieve {
    pub fn new(limit: usize) -> PrimeSieve {
        PrimeSieve {
            sieve_size: limit,
            raw_bits: vec![true; limit],
        }
    }

    pub fn run_sieve(&mut self) {
        let q = (self.sieve_size as f32).sqrt() as usize;
        let mut factor = 3;

        while factor < q {
            let mut num = factor;
            while num < q {
                if self.raw_bits[num] {
                    factor = num;
                    break;
                }
                num += 2;
            }

            num = factor * factor;

            while num < self.sieve_size {
                self.raw_bits[num] = false;
                num += factor * 2;
            }
            factor += 2;
        }
    }

    fn count_primes(&self) -> usize {
        let mut count: usize = 1;
        let mut i = 3;

        loop {
            if self.raw_bits[i] {
                count += 1;
            }
            i += 2;

            if i > (self.sieve_size) {
                break;
            }
        }
        return count;
    }

    fn validate_results(&self) -> bool {
        let historical_data: HashMap<usize, usize> = [
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
        .collect();

        if historical_data.contains_key(&self.sieve_size) {
            return historical_data.get(&self.sieve_size) == Some(&self.count_primes());
        }
        return false;
    }

    pub fn print_results(self, show_results: bool, duration: Duration, passes: u32) {
        if show_results {
            print!("2, ");
        }
        let mut count = 1;
        let mut num = 3;

        loop {
            if self.raw_bits[num] {
                if show_results {
                    print!("{}, ", num);
                }
                count += 1;
            }

            num += 2;

            if num > (self.sieve_size) {
                break;
            }
        }

        println!("");
        print!("Passes: {}, ", passes);
        print!("Time: {:?}, ", duration);
        print!("Avg: {:?}, ", duration / passes);
        print!("Limit: {}, ", self.sieve_size);
        print!("Count1: {}, ", count);
        print!("Count2: {}, ", self.count_primes());
        println!("Valid: {}", self.validate_results());

        // Following 2 lines added by rbergen to conform to drag race output format
        println!("");
        println!("Azgrom;{};{:?};1;algorithm=base,faithful=yes", passes, duration.as_secs_f32());
    }
}
