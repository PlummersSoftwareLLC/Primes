use primes::{
    print_results_stderr, report_results_stdout, FlagStorage, FlagStorageBitVector,
    FlagStorageByteVector, FlagStorageBitVectorRotate, PrimeSieve,
};
use std::{thread, time::{Duration, Instant}};
use structopt::StructOpt;

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
        // Return Some(true) or Some(false) if we know the answer, or None if we don't have
        // an entry for the given sieve_size.
        pub fn is_valid(&self, sieve_size: usize, result: usize) -> Option<bool> {
            if let Some(&expected) = self.0.get(&sieve_size) {
                Some(result == expected)
            } else {
                None
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
        #[inline(always)]
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

    /// Storage using a vector of 32-bit words, but addressing individual bits within each. Bits are
    /// reset by applying a mask created by a shift on every iteration, similar to the C++ implementation.
    pub struct FlagStorageBitVector {
        words: Vec<u32>,
        length_bits: usize,
    }

    const U32_BITS: usize = 32;
    impl FlagStorage for FlagStorageBitVector {
        fn create_true(size: usize) -> Self {
            let num_words = size / U32_BITS + (size % U32_BITS).min(1);
            FlagStorageBitVector {
                words: vec![0xffffffff; num_words],
                length_bits: size,
            }
        }

        #[inline(always)]
        fn reset_flags(&mut self, start: usize, skip: usize) {
            let mut i = start;
            while i < self.words.len() * U32_BITS {
                let word_idx = i / U32_BITS;
                let bit_idx = i % U32_BITS;
                // Note: Unsafe usage to ensure that we elide the bounds check reliably.
                //       We have ensured that word_index < self.words.len().
                unsafe {
                    *self.words.get_unchecked_mut(word_idx) &= !(1 << bit_idx);
                }
                i += skip;
            }
        }

        fn get(&self, index: usize) -> bool {
            if index >= self.length_bits {
                return false;
            }
            let word = self.words.get(index / U32_BITS).unwrap();
            *word & (1 << (index % U32_BITS)) != 0
        }
    }

    /// Storage using a vector of 32-bit words, but addressing individual bits within each. Bits are 
    /// reset by rotating the mask left instead of modulo+shift.
    pub struct FlagStorageBitVectorRotate {
        words: Vec<u32>,
        length_bits: usize,
    }

    impl FlagStorage for FlagStorageBitVectorRotate {
        fn create_true(size: usize) -> Self {
            let num_words = size / U32_BITS + (size % U32_BITS).min(1);
            FlagStorageBitVectorRotate {
                words: vec![0xffffffff; num_words],
                length_bits: size,
            }
        }

        #[inline(always)]
        fn reset_flags(&mut self, start: usize, skip: usize) {
            let mut i = start;
            let initial_bit_idx = start % U32_BITS;
            let mut rolling_mask: u32 = !(1 << initial_bit_idx);
            let roll_bits = skip as u32;
            while i < self.words.len() * U32_BITS {
                let word_idx = i / U32_BITS;
                // Note: Unsafe usage to ensure that we elide the bounds check reliably.
                //       We have ensured that word_index < self.words.len().
                unsafe {
                    *self.words.get_unchecked_mut(word_idx) &= rolling_mask;
                }
                i += skip;
                rolling_mask = rolling_mask.rotate_left(roll_bits);
            }
        }

        fn get(&self, index: usize) -> bool {
            if index >= self.length_bits {
                return false;
            }
            let word = self.words.get(index / U32_BITS).unwrap();
            *word & (1 << (index % U32_BITS)) != 0
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
                let start = factor * factor / 2;
                let skip = factor;
                self.flags.reset_flags(start, skip);

                factor += 2;
            }
        }
    }

    /// print results to console stderr for good feedback
    pub fn print_results_stderr<T: FlagStorage>(
        label: &str,
        prime_sieve: &PrimeSieve<T>,
        show_results: bool,
        duration: Duration,
        passes: usize,
        threads: usize,
        validator: &PrimeValidator,
    ) {
        if show_results {
            eprint!("2,");
            for num in (3..prime_sieve.sieve_size).filter(|n| prime_sieve.is_num_flagged(*n)) {
                print!("{},", num);
            }
            eprint!("\n");
        }

        let count = prime_sieve.count_primes();

        eprintln!(
            "{:15} Passes: {}, Threads: {}, Time: {:.10}, Average: {:.10}, Limit: {}, Counts: {}, Valid: {}",
            label,
            passes,
            threads,
            duration.as_secs_f32(),
            duration.as_secs_f32() / passes as f32,
            prime_sieve.sieve_size,
            count,
            match validator.is_valid(prime_sieve.sieve_size, count) {
                Some(true) => "Pass",
                Some(false) => "Fail",
                None => "Unknown"
            }
        );
    }

    /// print correctly-formatted results to `stderr` as per CONTRIBUTING.md
    /// - format is <name>;<iterations>;<total_time>;<num_threads>
    pub fn report_results_stdout(label: &str, bits_per_prime: usize, duration: Duration, passes: usize, threads: usize) {
        println!(
            "mike-barber_{};{};{:.10};{};algorithm=base,faithful=yes,bits={}",
            label,
            passes,
            duration.as_secs_f32(),
            threads,
            bits_per_prime
        );
    }
}

/// Rust program to calculate number of primes under a given limit.
#[derive(StructOpt, Debug)]
#[structopt(name = "abstracted")]
struct CommandLineOptions {
    /// Number of threads. If not specified, do two runs for both
    /// single threaded case and maximum concurrency.
    #[structopt(short, long)]
    threads: Option<usize>,

    /// Run duration
    #[structopt(short, long, default_value = "5")]
    seconds: u64,

    /// Prime sieve limit -- count primes that occur under or equal to this number.
    /// If you want this compared with known results, pick an order of 10: 10,100,...100000000
    #[structopt(short, long, default_value = "1000000")]
    limit: usize,

    /// Number of times to run the experiment
    #[structopt(short, long, default_value = "1")]
    repetitions: usize,

    /// Print out all primes found
    #[structopt(short, long)]
    print: bool,

    /// Run variant that uses bit-level storage
    #[structopt(long)]
    bits: bool,

    /// Run variant that uses bit-level storage, applied using rotate
    #[structopt(long)]
    bits_rotate: bool,
    
    /// Run variant that uses byte-level storage
    #[structopt(long)]
    bytes: bool,
}

fn main() {
    // command line options are handled by the `structopt` and `clap` crates, which
    // makes life very pleasant indeed.
    let opt = CommandLineOptions::from_args();

    let limit = opt.limit;
    let repetitions = opt.repetitions;
    let run_duration = Duration::from_secs(opt.seconds);

    let thread_options = match opt.threads {
        Some(t) => vec![t],
        None => vec![1, num_cpus::get()],
    };

    let (run_bits, run_bits_rotate, run_bytes) = match (opt.bits, opt.bits_rotate, opt.bytes) {
        (false, false, false) => (true, true, true),
        (bits, bits_rotate, bytes) => (bits, bits_rotate, bytes),
    };

    for threads in thread_options {
        if run_bytes {
            thread::sleep(Duration::from_secs(1));
            print_header(threads, limit, run_duration);
            for _ in 0..repetitions {
                run_implementation::<FlagStorageByteVector>(
                    "byte-storage",
                    8,
                    run_duration,
                    threads,
                    limit,
                    opt.print,
                );
            }
        }

        if run_bits {
            thread::sleep(Duration::from_secs(1));
            print_header(threads, limit, run_duration);
            for _ in 0..repetitions {
                run_implementation::<FlagStorageBitVector>(
                    "bit-storage",
                    1,
                    run_duration,
                    threads,
                    limit,
                    opt.print,
                );
            }
        }

        if run_bits_rotate {
            thread::sleep(Duration::from_secs(1));
            print_header(threads, limit, run_duration);
            for _ in 0..repetitions {
                run_implementation::<FlagStorageBitVectorRotate>(
                    "bit-storage-rotate",
                    1,
                    run_duration,
                    threads,
                    limit,
                    opt.print,
                );
            }
        }
    }
}

fn print_header(threads: usize, limit: usize, run_duration: Duration) {
    eprintln!();
    eprintln!(
        "Computing primes to {} on {} thread{} for {} second{}.",
        limit,
        threads,
        match threads {
            1 => "",
            _ => "s",
        },
        run_duration.as_secs(),
        match run_duration.as_secs() {
            1 => "",
            _ => "s",
        }
    );
}

fn run_implementation<T: 'static + FlagStorage + Send>(
    label: &str,
    bits_per_prime: usize,
    run_duration: Duration,
    num_threads: usize,
    limit: usize,
    print_primes: bool,
) {
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
        let duration = end_time - start_time;
        // print results to stderr for convenience
        print_results_stderr(
            label,
            &sieve,
            print_primes,
            duration,
            total_passes,
            num_threads,
            &primes::PrimeValidator::default(),
        );
        // and report results to stdout for reporting
        report_results_stdout(label, bits_per_prime, duration, total_passes, num_threads);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::primes::{FlagStorage, FlagStorageBitVector, FlagStorageBitVectorRotate, FlagStorageByteVector, PrimeSieve, PrimeValidator};

    #[test]
    fn sieve_known_correct_bits() {
        sieve_known_correct::<FlagStorageBitVector>();
    }

    #[test]
    fn sieve_known_correct_bits_rolling() {
        sieve_known_correct::<FlagStorageBitVectorRotate>();
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
