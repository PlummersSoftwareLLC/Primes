//! # Rust solution 5 by Kulasko
//!
//! As it's written in the readme, this solution focuses in different multithreading algorithms.
//! There are currently three algorithms, each run with a bit vector and a bool vector.
//!
//! The algorithms are implemented as a trait specialisation of the corresponding data struct. They
//! are a bit unwieldy because of the verbose instantiation, this could be improved by taking the
//! constructor out of the trait.

mod serial;
mod streamed;
mod tiled;

use rayon::prelude::*;
use std::time::{Duration, Instant};
use structopt::StructOpt;

/// Marker for the serial algorithm.
#[derive(Clone, Copy)]
pub struct Serial;
/// Marker for the streaming multithread algorithm.
#[derive(Clone, Copy)]
pub struct Streamed;
/// Marker for the tiling multithread algorithm. The field contains the cache size bound in bytes.
#[derive(Clone, Copy)]
pub struct Tiled(usize);

/// A sieve that saves each number status as a bool value. Only odd numbers are saved, since even
/// numbers can't be prime numbers by definition, two being the exception.
pub struct BoolSieve<T> {
    /// The data container.
    data: Box<[bool]>,
    /// The amount of numbers the sieve data represents.
    size: usize,
    /// If the sieve has been run already.
    sieved: bool,
    /// An object that can carry execution parameters.
    algorithm: T,
}

/// A sieve that saves each number status as a single bit. Only odd numbers are saved, since even
/// numbers can't be prime numbers by definition, two being the exception.
pub struct BitSieve<T> {
    /// The data container. Each element contains `usize::BITS` flags for prime numbers.
    data: Box<[usize]>,
    /// The amount of numbers the sieve data represents.
    size: usize,
    /// If the sieve has been run already.
    sieved: bool,
    /// An object that can carry execution parameters.
    algorithm: T,
}

/// Most things are hardcoded. Performs one bench for each combination of algorithm and data
/// structure.
pub fn main() {
    let arguments = Arguments::from_args();

    eprintln!("Starting benchmark");
    eprintln!("Working set size is {} kB", arguments.set_size);
    perform_bench::<BitSieve<Serial>, Serial>(Serial, arguments.sieve_size, arguments.duration);
    perform_bench::<BitSieve<Streamed>, Streamed>(
        Streamed,
        arguments.sieve_size,
        arguments.duration,
    );
    perform_bench::<BitSieve<Tiled>, Tiled>(
        Tiled(1024 * arguments.set_size),
        arguments.sieve_size,
        arguments.duration,
    );
    perform_bench::<BoolSieve<Serial>, Serial>(Serial, arguments.sieve_size, arguments.duration);
    perform_bench::<BoolSieve<Streamed>, Streamed>(
        Streamed,
        arguments.sieve_size,
        arguments.duration,
    );
    perform_bench::<BoolSieve<Tiled>, Tiled>(
        Tiled(1024 * arguments.set_size),
        arguments.sieve_size,
        arguments.duration,
    );
}

/// Executes a specific bench and prints the result.
fn perform_bench<T: Sieve<A> + SieveDisplay, A: Copy>(
    algorithm: A,
    sieve_size: usize,
    duration: usize,
) {
    let mut passes = 0;
    let mut sieve = T::new(2, algorithm);
    let mut elapsed = Duration::from_secs(0);

    eprintln!(
        "Running {} with {} primes for {} seconds",
        sieve.get_id_string(),
        sieve_size,
        duration
    );

    let start = Instant::now();

    while elapsed < Duration::from_secs(duration as u64) {
        sieve = T::new(sieve_size, algorithm);
        sieve.sieve();

        passes += 1;
        elapsed = Instant::now() - start;
    }

    let result = sieve.count_primes();

    eprintln!(
        "Time: {}, Passes: {}, Per second: {}, Average time: {}, Threads: {}, Prime count: {}",
        elapsed.as_secs_f64(),
        passes,
        passes as f64 / elapsed.as_secs_f64(),
        elapsed.as_secs_f64() / passes as f64,
        sieve.thread_count(),
        result
    );
    if let Ok(index) = PRIMES_IN_SIEVE.binary_search_by_key(&sieve_size, |(key, _)| *key) {
        if PRIMES_IN_SIEVE[index].1 == result {
            eprintln!("This result is verified to be correct");
        } else {
            eprintln!("ERROR: Incorrect sieve result!");
        }
    }

    println!(
        "kulasko-rust-{};{};{};{};algorithm=base,faithful=yes,bits={}",
        sieve.get_id_string(),
        passes,
        elapsed.as_secs_f64(),
        sieve.thread_count(),
        T::flag_size()
    );
}

/// Methods for building and executing a sieve, also some algorithm-specific information. Each
/// algorithm and structure combination is implemented by a implementation of this template.
pub trait Sieve<T> {
    /// A stock-standard constructor.
    fn new(size: usize, algorithm: T) -> Self;
    /// Executes the sieve.
    fn sieve(&mut self);
    /// A string literal for identification.
    fn get_id_string(&self) -> &'static str;
    /// How many thread are used by the algorithm.
    fn thread_count(&self) -> usize;
}

/// Methods for getting the sieve results. Implemented once for each data structure.
trait SieveDisplay {
    /// The actual result which can be checked for correctness.
    fn count_primes(&self) -> usize;
    /// Prints all found primes. Only really used for debugging.
    fn print_primes(&self);
    /// How many bits the representation uses for storing a single flag.
    fn flag_size() -> usize;
}

/// Contains the arguments of the program.
#[derive(Debug, StructOpt)]
#[structopt(name = "kulasko-rust")]
struct Arguments {
    /// The amount of numbers in a sieve.
    #[structopt(short, long, default_value = "1000000")]
    sieve_size: usize,
    /// The test duration in seconds.
    #[structopt(short, long, default_value = "5")]
    duration: usize,
    /// The size of the working set in kibibytes. Is used by the tiling algorithm. Should not
    /// exceed your memory layer of choice.
    #[structopt(
        short,
        long,
        help = "The working set size in kibibytes",
        default_value = "16"
    )]
    set_size: usize,
}

/// Known prime counts for specific sieve sizes.
const PRIMES_IN_SIEVE: [(usize, usize); 11] = [
    (2, 1),
    (3, 2),
    (4, 2),
    (10, 4),
    (100, 25),
    (1000, 168),
    (10000, 1229),
    (100000, 9592),
    (1000000, 78498),
    (10000000, 664579),
    (100000000, 5761455),
];

impl<T> BitSieve<T> {
    /// Looking up the next still-set prime flag. Can be used by multiple algorithm implementations.
    #[inline(always)]
    pub fn next_prime_index(data: &[usize], size: usize, last_bit: usize) -> usize {
        debug_assert!((last_bit / usize::BITS as usize) < data.len());
        (last_bit + 1..size)
            .find(|&num| {
                let word = unsafe { data.get_unchecked(num / usize::BITS as usize) };
                (*word & (1 << (num % usize::BITS as usize))) != 0
            })
            .unwrap()
    }

    /// The actual sieving, unsets non-prime numbers. Is used by multiple implementations.
    #[inline(always)]
    pub fn fall_through(data: &mut [usize], data_size: usize, prime: usize) {
        let mut check_number = prime * prime / 2;
        while check_number <= data_size {
            let word = unsafe { data.get_unchecked_mut(check_number / usize::BITS as usize) };
            *word &= !(1 << (check_number % usize::BITS as usize));

            check_number += prime;
        }
    }

    /// Looking up the next prime number, but checks whole blocks first before searching a single
    /// bit. Has about the same performance as the other one on my machine, so I include both.
    #[inline(always)]
    pub fn next_prime_index_block(data: &[usize], last_bit: usize) -> usize {
        debug_assert!((last_bit / usize::BITS as usize) < data.len());
        let index = last_bit / usize::BITS as usize;
        // +1 because we want to start after the current one
        let offset = (last_bit + 1) % usize::BITS as usize;

        // mask all bits up to our current one
        let word = unsafe { *data.get_unchecked(index) };
        let current_block = word & (usize::MAX << offset);

        // a offset of zero should be `usize::BITS`, so it means the next block
        if (current_block != 0) & (offset != 0) {
            index * usize::BITS as usize + current_block.trailing_zeros() as usize
        } else {
            let index = (index + 1..data.len()).find(|&block| block != 0).unwrap();
            index * usize::BITS as usize
                + unsafe { data.get_unchecked(index).trailing_zeros() as usize }
        }
    }
}

impl<T> SieveDisplay for BitSieve<T> {
    /// Bitfield can only have a size of a multiple of the underlying number type. As such, when
    /// counting set bits, bits beyond the actual sieve size get subtracted from the result again.
    fn count_primes(&self) -> usize {
        let overshoot_amount = ((self.size + 1) / 2) % usize::BITS as usize;
        let overshoot = if overshoot_amount != 0 {
            (self.data.last().unwrap() & (usize::MAX << overshoot_amount)).count_ones() as usize
        } else {
            0
        };

        self.data
            .as_parallel_slice()
            .into_par_iter()
            .map(|entry| entry.count_ones() as usize)
            .sum::<usize>()
            - overshoot
    }

    fn print_primes(&self) {
        eprintln!("Primes up to {}:", self.size);
        eprint!("2");
        for num in 1..(self.size + 1) / 2 {
            if (self.data[num / usize::BITS as usize] & (1 << (num % usize::BITS as usize))) != 0 {
                eprint!(", {}", num * 2 + 1);
            }
        }
        eprintln!();
    }

    fn flag_size() -> usize {
        1
    }
}

impl<T> SieveDisplay for BoolSieve<T> {
    fn count_primes(&self) -> usize {
        self.data.iter().copied().filter(|&flag| flag).count()
    }

    fn print_primes(&self) {
        eprintln!("Primes up to {}:", ((self.data.len() - 1) * 2 + 1).max(2));
        eprint!("2");
        for (i, _) in self
            .data
            .iter()
            .enumerate()
            .skip(1)
            .filter(|(_, &flag)| flag)
        {
            eprint!(", {}", i * 2 + 1);
        }
        eprintln!();
    }

    fn flag_size() -> usize {
        std::mem::size_of::<bool>() * 8
    }
}

#[cfg(test)]
mod test {
    use crate::{
        BitSieve, BoolSieve, Serial, Sieve, SieveDisplay, Streamed, Tiled, PRIMES_IN_SIEVE,
    };

    /// Generic performing function to reduce code redundancy.
    fn run_test<T: Sieve<A> + SieveDisplay, A: Copy>(algorithm: A) {
        for (numbers, primes) in PRIMES_IN_SIEVE {
            let mut sieve = T::new(numbers, algorithm);
            sieve.sieve();
            assert_eq!(sieve.count_primes(), primes, "Primes {}", numbers);
        }
    }

    #[test]
    fn serial_bit() {
        run_test::<BitSieve<Serial>, Serial>(Serial);
    }

    #[test]
    fn serial_byte() {
        run_test::<BoolSieve<Serial>, Serial>(Serial);
    }

    #[test]
    fn streamed_bit() {
        run_test::<BitSieve<Streamed>, Streamed>(Streamed);
    }

    #[test]
    fn streamed_byte() {
        run_test::<BoolSieve<Streamed>, Streamed>(Streamed);
    }

    #[test]
    fn tiled_bit() {
        run_test::<BitSieve<Tiled>, Tiled>(Tiled(1 << 16));
    }

    #[test]
    fn tiled_byte() {
        run_test::<BoolSieve<Tiled>, Tiled>(Tiled(1 << 16));
    }
}
