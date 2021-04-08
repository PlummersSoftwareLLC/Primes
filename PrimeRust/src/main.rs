use std::{collections::HashMap, time};

pub struct prime_sieve {
    sieveSize: i64,
    bits: Vec<bool>,
    resultsDictionary: HashMap<i64, i32>,
}
impl prime_sieve {
    pub fn new(n: i64) -> Self {
        prime_sieve {
            sieveSize: n,
            bits: vec![true; n as usize],
            resultsDictionary: vec![
                (10, 4),   // Historical data for validating our results - the number of primes
                (100, 25), // to be found under some limit, such as 168 primes under 1000
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
            .map(|(f, s)| (f.to_owned(), s.to_owned()))
            .collect::<HashMap<i64, i32>>(),
        }
    }

    fn validateResults(&self) -> bool {
        let result = match self.resultsDictionary.get(&self.sieveSize) {
            Some(r) => r,
            None => return false,
        }
        .to_owned();

        return result == self.countPrimes();
    }

    pub fn runSieve(&mut self) {
        let mut factor = 3;
        let mut q = f64::sqrt(self.sieveSize as f64);

        while factor as f64 <= q {
            {
                let mut num = factor;
                while num < self.sieveSize {
                    if match self.bits.get(num as usize) {
                        Some(c) => c.to_owned(),
                        None => false,
                    } {
                        factor = num;
                        break;
                    }

                    num += 2;
                }
            }
            {
                let mut num: usize = (factor * factor) as usize;
                while num < self.sieveSize as usize {
                    self.bits[num] = false;
                    num += factor as usize * 2;
                }
            }
            factor += 2;
        }
    }

    pub fn printResults(&self, showResults: bool, duration: time::Duration, passes: i32) {
        if showResults {
            print!("2, ");
        }

        let mut count = if self.sieveSize >= 2 { 1 } else { 0 };
        {
            let mut num: usize = 3;
            while num <= self.sieveSize as usize {
                if match self.bits.get(num) {
                    Some(b) => b.to_owned(),
                    None => false,
                } {
                    if showResults {
                        print!("{}, ", num);
                    }
                    count += 1;
                }
                num += 2;
            }
        }

        if showResults {
            print!("\n");
        }

        let time = duration.as_secs();
        println!(
            "Passes: {}, Time: {}, Avg: {}, Limit: {}, Count1: {}, Count2: {}, Valid: {}",
            passes,
            time,
            time as f32 / passes as f32,
            self.sieveSize,
            count,
            self.countPrimes(),
            self.validateResults()
        )
    }

    pub fn countPrimes(&self) -> i32 {
        let mut count = if self.sieveSize >= 2 { 1 } else { 0 };
        {
            let mut i: usize = 3;
            while i < self.sieveSize as usize {
                if match self.bits.get(i) {
                    Some(b) => b.to_owned(),
                    None => false,
                } {
                    count += 1;
                }
                i += 2;
            }
        }
        return count;
    }
}

fn main() {
    let mut passes = 0;
    let tStart = time::Instant::now();

    loop {
        let mut sieve: prime_sieve = prime_sieve::new(1000000);
        sieve.runSieve();
        passes += 1;
        if tStart.elapsed().as_secs() >= 5 {
            sieve.printResults(false, tStart.elapsed(), passes);
            break;
        }
    }
}
