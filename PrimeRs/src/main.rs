use std::{
    collections::HashMap,
    time::{Duration, Instant},
};
struct PrimeCS {
    sieve_size: usize,
    bit_array: Vec<bool>,
    my_dict: HashMap<usize, usize>,
}

impl PrimeCS {
    fn generate_dictionary() -> HashMap<usize, usize> {
        [
            (10, 4),
            (100, 25),
            (1000, 168),
            (10000, 1229),
            (100000, 9592),
            (1000000, 78498),
            (10000000, 664579),
            (100000000, 5761455),
            (10000000000, 455052511),
        ]
        .iter()
        .copied()
        .collect()
    }

    pub fn new(size: usize) -> Self {
        Self {
            sieve_size: size,
            bit_array: vec![true; size],
            my_dict: Self::generate_dictionary(),
        }
    }
    pub fn validate_results(&self) -> bool {
        self.my_dict
            .get(&self.sieve_size)
            .map(|n| *n == self.count_primes())
            .unwrap_or(false)
    }

    pub fn run_sieve(&mut self) {
        let mut factor = 3;
        let q = f64::sqrt(self.sieve_size as f64) as usize;
        while factor <= q {
            for num in (factor..self.sieve_size).step_by(2) {
                match self.bit_array.get(num) {
                    Some(b) => {
                        if *b {
                            factor = num;
                            break;
                        }
                    }
                    None => {}
                }
            }

            self.bit_array
                .get_mut(factor * factor..self.sieve_size)
                .unwrap_or_default()
                .iter_mut()
                .step_by(factor * 2)
                .for_each(|b| *b = false);
            factor += 2;
        }
    }

    pub fn print_results(&mut self, show_results: bool, duration: Duration, passes: i32) {
        if show_results {
            print!("2, ");
        }
        let mut count = 1;
        for num in (3..=self.sieve_size).step_by(2) {
            self.bit_array.get(num).map(|v| {
                if *v {
                    count += 1;
                    if show_results {
                        print!("{}, ", num);
                    }
                }
            });
        }
        if show_results {
            println!();
        }
        println!(
            "Passes: {}, Time: {}, Avg: {}, Limit: {}, Count1: {}, Count2: {}, Valid: {}",
            passes,
            duration.as_secs_f32(),
            duration.as_secs_f32() / passes as f32,
            self.sieve_size,
            count,
            self.count_primes(),
            self.validate_results()
        )
    }

    pub fn count_primes(&self) -> usize {
        self.bit_array
            .iter()
            .skip(3)
            .step_by(2)
            .filter(|b| **b)
            .count()
            + 1
    }
}

fn main() {
    let mut passes = 0;
    let t_start = Instant::now();
    loop {
        let mut sieve = PrimeCS::new(1000000);
        sieve.run_sieve();
        passes += 1;
        let elapsed = t_start.elapsed();
        if elapsed >= Duration::from_secs(5) {
            sieve.print_results(false, elapsed, passes);
            break;
        }
    }
}
