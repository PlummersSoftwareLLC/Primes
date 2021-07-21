//by Sycration
//this code must be run on nightly or beta

#![feature(const_eval_limit)]
#![const_eval_limit = "0"]

use std::time::*;
use std::usize;

//a const generic; sort of like constexpr in c++
pub struct PrimeSieve<const N: usize> {
    sieve_size: usize,
    bits: [bool; N],
}

impl<const N: usize> PrimeSieve<N> {
    //constructor
    pub const fn new() -> Self {
        Self {
            sieve_size: N,
            bits: [true; N],
        }
    }

    //this function is const generic and runs at compile time too
    pub const fn run_sieve() -> [bool; N] {
        let mut bits = [true; N];
        let mut factor = 3;

        //it doesn't actually loop, it always breaks on the first go
        let q: usize = loop {
            //this ugly code is just the integer square root inline
            if 0 == N {
                break 0;
            }
            let mut n: usize = (N / 2) + 1;
            let mut n1: usize = (n + (N / n)) / 2;
            while n1 < n {
                n = n1;
                n1 = (n + (N / n)) / 2;
            }
            break n;
        };

        //array[idx]

        while factor <= q {
            let mut num = factor;
            while num < N {
                if bits[num] {
                    factor = num;
                    break;
                }
                num += 2;
            }
            let mut num = factor * factor;
            while num < N {
                bits[num] = false;
                num += factor * 2;
            }

            factor += 2;
        }
        bits
    }

    pub fn gen_results(
        &mut self,
        show_results: bool,
        duration: f64,
        passes: usize,
    ) -> (i64, Option<bool>, usize, f64, usize, usize) {
        let primecount = self.count_primes();
        let valid = self.validate_results();

        let mut count = 1; // Starting count (2 is prime)
        for num in (3..=self.sieve_size).step_by(2) {
            if self.bits.len() >= num && self.bits[num] {
                count += 1;
            }
        }

        //print!(
        //    "Passes: {}, Time: {}, Avg: {}, Limit: {}, Count1: {}, Count2: {}",
        //    passes,
        //    duration,
        //    duration / passes as f64,
        //    self.sieve_size,
        //    count,
        //    primecount,
        //);
        let nonzero_digits = format!("{}", N).chars().filter(|x| *x != '0').count();
        let first_digit = format!("{}", N).chars().nth(0);
        let valid = if nonzero_digits > 1 || first_digit != Some('1') {
            None
        } else {
            Some(valid)
        };
        return (primecount, valid, self.sieve_size, duration, passes, count);
    }

    pub const PRIME_COUNTS: [(i64, i64); 10] = [
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
    ];

    pub fn validate_results(&mut self) -> bool {
        let result =
            Self::PRIME_COUNTS[((self.sieve_size as f64).log10() - 1.0).floor() as usize].1;
        let is = self.count_primes();
        result == is
    }

    pub fn count_primes(&self) -> i64 {
        let mut count = 1;
        let mut i = 3;
        while i < self.sieve_size {
            if self.bits[i] {
                count += 1;
            }
            i += 2;
        }

        return count;
    }
}

fn main() {
    println!("running on eight cores");
    //if this is not a power of 10 the program will be unable to validate it
    //and if it is greater than about 2 million the compiler will crash
    //dbg!((0..20).map(|_| optimize()).sum::<usize>() / 20);

    const SIZE: usize = 1_000_000;
    let t_start = Instant::now();
    //(i64, Option<bool>, usize, f64)
    let mut threads = (0..8)
        .map(|_| {
            let t_start = t_start.clone();
            std::thread::spawn(move || {
                let mut passes = 0;
                loop {
                    let mut sieve = PrimeSieve::<SIZE>::new();
                    const THIS_RESULT: [bool; SIZE] = PrimeSieve::run_sieve();
                    sieve.bits = THIS_RESULT;
                    if std::time::Duration::as_secs(&(Instant::now() - t_start)) >= 5 {
                        let now = Instant::now();
                        break sieve.gen_results(false, (now - t_start).as_secs_f64(), passes);
                    }
                    passes += 1;
                }
            })
        })
        .collect::<Vec<_>>();
    let (primecount, valid, size, duration, passes, count) = threads
        .into_iter()
        .map(|handle| handle.join().unwrap().clone())
        .fold((0, Some(true), 0, 0., 0, 0), |acc, x| {
            (
                x.0,
                {
                    match (acc.1, x.1) {
                        (None, None) => None,
                        (None, Some(_)) => None,
                        (Some(_), None) => None,
                        (Some(_), Some(false)) => Some(false),
                        (Some(false), Some(_)) => Some(false),
                        (Some(true), Some(true)) => Some(true),
                    }
                },
                x.2,
                {
                    if acc.3 < x.3 {
                        x.3
                    } else {
                        acc.3
                    }
                },
                acc.4 + x.4,
                x.5,
            )
        });

    print!(
        "Passes: {}, Time: {}, Avg: {}, Limit: {}, Count1: {}, Count2: {}",
        passes,
        duration,
        duration / passes as f64,
        size,
        count,
        primecount,
    );

    if let Some(valid) = valid {
        println!(", Valid: {}", valid);
    } else {
        println!(", unable to validate")
    }

    println!(
        "SycrationMultithreaded;{};{};8;algorithm=base,faithful=no\n",
        passes, duration,
    );
}
