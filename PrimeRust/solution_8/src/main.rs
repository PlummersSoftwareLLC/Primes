use std::env;
use std::thread;
use std::time::{Duration, Instant};

// Pre-halved steps for 8-of-30 wheel (numbers coprime to 2, 3, 5):
// The 8 candidates per 30 numbers are: 1, 7, 11, 13, 17, 19, 23, 29.
// Distances: 6, 4, 2, 4, 2, 4, 6, 2.
// Halved steps: 3, 2, 1, 2, 1, 2, 3, 1 (sum = 15).
const STEPS: [usize; 8] = [3, 2, 1, 2, 1, 2, 3, 1];

/// PrimeSieve encapsulates the sieve state and candidate bit buffer,
/// conforming strictly to the "faithful" rules of the Dave's Garage Drag Race.
pub struct PrimeSieve {
    sieve_size: usize,
    bits: Vec<u64>,
}

impl PrimeSieve {
    /// Dynamically allocates the bit buffer corresponding to the sieve size at runtime.
    #[inline(always)]
    pub fn new(size: usize) -> Self {
        let num_odds = size >> 1;
        let num_words = (num_odds + 63) >> 6;
        Self {
            sieve_size: size,
            bits: vec![0u64; num_words],
        }
    }

    /// Executes the 8-of-30 wheel sieve.
    /// Only numbers coprime to 2, 3, 5 are considered, and only their coprime multiples are crossed off.
    #[inline(always)]
    pub fn run_sieve(&mut self) {
        let maxintsh = self.sieve_size >> 1;
        let q = (self.sieve_size as f64).sqrt() as usize;
        let qh = q >> 1;
        let ptr = self.bits.as_mut_ptr();

        let mut step = 1usize; // Start at prime 7 (7 -> 11 is step 1)
        let mut inc = STEPS[step];
        let mut factorh = 7usize >> 1; // 7 / 2 = 3

        while factorh <= qh {
            let is_composite = unsafe {
                let w = *ptr.add(factorh >> 6);
                (w & (1u64 << (factorh & 63))) != 0
            };

            if !is_composite {
                let factor = (factorh << 1) + 1;
                let mut istep = step;
                let mut i = (factor * factor) >> 1;

                unsafe {
                    let s0 = factor * STEPS[istep];
                    let s1 = factor * STEPS[(istep + 1) & 7];
                    let s2 = factor * STEPS[(istep + 2) & 7];
                    let s3 = factor * STEPS[(istep + 3) & 7];
                    let s4 = factor * STEPS[(istep + 4) & 7];
                    let s5 = factor * STEPS[(istep + 5) & 7];
                    let s6 = factor * STEPS[(istep + 6) & 7];
                    let s7 = factor * STEPS[(istep + 7) & 7];
                    let cycle = factor * 15;

                    while i + cycle < maxintsh {
                        *ptr.add(i >> 6) |= 1u64 << (i & 63);
                        i += s0;
                        *ptr.add(i >> 6) |= 1u64 << (i & 63);
                        i += s1;
                        *ptr.add(i >> 6) |= 1u64 << (i & 63);
                        i += s2;
                        *ptr.add(i >> 6) |= 1u64 << (i & 63);
                        i += s3;
                        *ptr.add(i >> 6) |= 1u64 << (i & 63);
                        i += s4;
                        *ptr.add(i >> 6) |= 1u64 << (i & 63);
                        i += s5;
                        *ptr.add(i >> 6) |= 1u64 << (i & 63);
                        i += s6;
                        *ptr.add(i >> 6) |= 1u64 << (i & 63);
                        i += s7;
                    }
                    while i < maxintsh {
                        *ptr.add(i >> 6) |= 1u64 << (i & 63);
                        i += factor * STEPS[istep];
                        istep = (istep + 1) & 7;
                    }
                }
            }

            factorh += inc;
            step = (step + 1) & 7;
            inc = STEPS[step];
        }
    }

    /// Counts the total number of primes identified by the sieve.
    pub fn count_primes(&self) -> usize {
        let maxints = self.sieve_size;
        let mut count = 3; // Accounts for pre-sieved primes 2, 3, 5
        let mut factor = 7usize;
        let mut step = 1usize;
        let mut inc = STEPS[step] << 1;
        let ptr = self.bits.as_ptr();

        while factor <= maxints {
            let half = factor >> 1;
            let is_composite = unsafe {
                let w = *ptr.add(half >> 6);
                (w & (1u64 << (half & 63))) != 0
            };
            if !is_composite {
                count += 1;
            }
            factor += inc;
            step = (step + 1) & 7;
            inc = STEPS[step] << 1;
        }
        count
    }

    pub fn validate_results(&self) -> bool {
        match self.sieve_size {
            10 => self.count_primes() == 4,
            100 => self.count_primes() == 25,
            1_000 => self.count_primes() == 168,
            10_000 => self.count_primes() == 1_229,
            100_000 => self.count_primes() == 9_592,
            1_000_000 => self.count_primes() == 78_498,
            10_000_000 => self.count_primes() == 664_579,
            _ => false,
        }
    }
}

fn run_single_thread(seconds: u64, limit: usize) -> (usize, f64) {
    let target = Duration::from_secs(seconds);
    let start = Instant::now();
    let mut passes = 0;
    loop {
        let mut sieve = PrimeSieve::new(limit);
        sieve.run_sieve();
        passes += 1;
        if start.elapsed() >= target {
            break;
        }
    }
    (passes, start.elapsed().as_secs_f64())
}

fn run_multi_thread(seconds: u64, limit: usize, num_threads: usize) -> (usize, f64) {
    let target = Duration::from_secs(seconds);
    let start = Instant::now();

    let mut handles = Vec::with_capacity(num_threads);
    for _ in 0..num_threads {
        handles.push(thread::spawn(move || {
            let mut local_passes = 0;
            let thread_start = Instant::now();
            loop {
                let mut sieve = PrimeSieve::new(limit);
                sieve.run_sieve();
                local_passes += 1;
                if thread_start.elapsed() >= target {
                    break;
                }
            }
            local_passes
        }));
    }

    let mut total_passes = 0;
    for h in handles {
        total_passes += h.join().unwrap();
    }
    (total_passes, start.elapsed().as_secs_f64())
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let seconds: u64 = args
        .get(1)
        .and_then(|s| s.parse().ok())
        .unwrap_or(5);
    let limit: usize = args
        .get(2)
        .and_then(|s| s.parse().ok())
        .unwrap_or(1_000_000);

    let mut validator = PrimeSieve::new(limit);
    validator.run_sieve();
    if !validator.validate_results() {
        eprintln!(
            "[ERROR] Validation failed! Sieve limit={}, Count={}",
            limit,
            validator.count_primes()
        );
        std::process::exit(1);
    }

    // 1. Single-threaded benchmark
    let (s_passes, s_time) = run_single_thread(seconds, limit);
    println!(
        "ndt0208-rust-wheel8;{};{:.6};1;algorithm=wheel,faithful=yes,bits=1",
        s_passes, s_time
    );

    // 2. Multi-threaded benchmark
    let num_cpus = thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);
    let (m_passes, m_time) = run_multi_thread(seconds, limit, num_cpus);
    println!(
        "ndt0208-rust-wheel8-par;{};{:.6};{};algorithm=wheel,faithful=yes,bits=1",
        m_passes, m_time, num_cpus
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prime_counts() {
        let cases = [
            (10, 4),
            (100, 25),
            (1_000, 168),
            (10_000, 1_229),
            (100_000, 9_592),
            (1_000_000, 78_498),
        ];
        for (limit, expected) in cases {
            let mut sieve = PrimeSieve::new(limit);
            sieve.run_sieve();
            assert_eq!(
                sieve.count_primes(),
                expected,
                "Failed count for limit {}",
                limit
            );
            assert!(sieve.validate_results());
        }
    }
}
