use std::collections::HashMap;

pub struct PrimeSieve {
    sieve_size: usize,
    bits: Vec<bool>,
    known_prime_counts: HashMap<usize, usize>,
}

impl PrimeSieve {
    pub fn new(size: usize) -> Self {
        let mut bits = Vec::with_capacity(size);
        for _ in 0..size {
            bits.push(true);
        }

        let mut known_prime_counts = HashMap::with_capacity(8);
        known_prime_counts.insert(10, 4);
        known_prime_counts.insert(100, 25);
        known_prime_counts.insert(1_000, 168);
        known_prime_counts.insert(10_000, 1_229);
        known_prime_counts.insert(100_000, 9_592);
        known_prime_counts.insert(1_000_000, 78_498);
        known_prime_counts.insert(10_000_000, 664_579);
        known_prime_counts.insert(100_000_000, 5_761_455);

        PrimeSieve {
            sieve_size: size,
            bits,
            known_prime_counts
        }
    }

    pub fn none() -> Self {
        PrimeSieve {
            sieve_size: 0,
            bits: Vec::new(),
            known_prime_counts: HashMap::new(),
        }
    }

    pub fn run_sieve(&mut self) {
        let mut factor = 3;
        let q = (self.sieve_size as f64).sqrt() as i32;

        while factor <= q {
            for num in (factor..(self.sieve_size as i32)).step_by(2) {
                if self.bits[num as usize] {
                    factor = num;
                    break;
                }
            }

            for num in ((factor * factor)..(self.sieve_size as i32)).step_by((factor * 2) as usize) {
                self.bits[num as usize] = false;
            }

            factor = factor + 2;
        }
    }

    pub fn validate_results(&self) -> bool {
        match self.known_prime_counts.get(&(self.sieve_size)) {
            Some(c) => *c == self.count_primes(),
            _ => false
        }
    }

    pub fn print_results(&self, show_results: bool, duration: f32, passes: i32) {
        if show_results {
            print!("2, ");
        }

        let mut count = match self.sieve_size {
            2..=usize::MAX => 1,
            _ => 0
        };

        for num in (3..=self.bits.len()).step_by(2) {
            if self.bits[num as usize] {
                count = count + 1;
                if show_results {
                    print!("{}", num);
                }
            }
        }

        if show_results {
            println!("");
        }

        println!("Passes: {}, Time: {}, Avg: {}, Limit: {}, Count: {}, Valid: {}", passes, duration, duration / passes as f32, self.sieve_size, count, self.validate_results());
    }

    fn count_primes(&self) -> usize {
        let mut count = match self.sieve_size {
            2..=usize::MAX => 1,
            _ => 0
        };

        for i in (3..=self.bits.len()).step_by(2) {
            if self.bits[i] {
                count = count + 1;
            }
        }

        count
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn under_ten_yields_valid_results() {
        let mut sieve = PrimeSieve::new(10);
        sieve.run_sieve();
        assert!(sieve.validate_results());
    }

    #[test]
    fn under_one_hundred_yields_valid_results() {
        let mut sieve = PrimeSieve::new(100);
        sieve.run_sieve();
        assert!(sieve.validate_results());
    }

    #[test]
    fn under_one_thousand_yields_valid_results() {
        let mut sieve = PrimeSieve::new(1_000);
        sieve.run_sieve();
        assert!(sieve.validate_results());
    }

    #[test]
    fn under_ten_thousand_yields_valid_results() {
        let mut sieve = PrimeSieve::new(10_000);
        sieve.run_sieve();
        assert!(sieve.validate_results());
    }

    #[test]
    fn under_one_hundred_thousand_yields_valid_results() {
        let mut sieve = PrimeSieve::new(100_000);
        sieve.run_sieve();
        assert!(sieve.validate_results());
    }

    #[test]
    fn under_one_million_yields_valid_results() {
        let mut sieve = PrimeSieve::new(1_000_000);
        sieve.run_sieve();
        assert!(sieve.validate_results());
    }

    #[test]
    fn under_ten_million_yields_valid_results() {
        let mut sieve = PrimeSieve::new(10_000_000);
        sieve.run_sieve();
        assert!(sieve.validate_results());
    }

    #[test]
    fn under_one_hundred_million_yields_valid_results() {
        let mut sieve = PrimeSieve::new(100_000_000);
        sieve.run_sieve();
        assert!(sieve.validate_results());
    }
}