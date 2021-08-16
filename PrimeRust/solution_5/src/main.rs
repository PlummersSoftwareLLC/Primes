//! # Rust solution 5 by Kulasko
//!
//! As it's written in the readme, this solution focuses in different multithreading algorithms.
//! Each algorithm is run with a set of different prime flag storage types.

#![warn(missing_docs)]

mod data_type;
mod sieve;

pub use data_type::{DataType, Integer};

use sieve::flag_data::{FlagData, STRIPE_SIZE};
use sieve::{algorithm, flag_data, Algorithm, Sieve, SieveExecute};

use std::time::{Duration, Instant};
use structopt::StructOpt;

/// Most things are hardcoded. Performs one bench for each combination of algorithm and data
/// structure that makes sense.
pub fn main() {
    let arguments = Arguments::from_args();

    eprintln!("Starting benchmark");
    eprintln!("Working set size is {} kB", arguments.set_size);
    benches!(
        arguments.sieve_size;
        arguments.duration;
        <algorithm::Stream, flag_data::Bool, u8>(algorithm::Stream);
        <algorithm::Stream, flag_data::Bit, u8>(algorithm::Stream);
        <algorithm::Stream, flag_data::Bit, u32>(algorithm::Stream);
        <algorithm::Stream, flag_data::Rotate, u8>(algorithm::Stream);
        <algorithm::Stream, flag_data::Rotate, u32>(algorithm::Stream);
        <algorithm::Stream, flag_data::Stripe, [u8; STRIPE_SIZE]>(algorithm::Stream);
        <algorithm::Tile, flag_data::Bool, u8>(algorithm::Tile(arguments.set_size * 1024));
        <algorithm::Tile, flag_data::Bit, u8>(algorithm::Tile(arguments.set_size * 1024));
        <algorithm::Tile, flag_data::Bit, u32>(algorithm::Tile(arguments.set_size * 1024));
        <algorithm::Tile, flag_data::Rotate, u8>(algorithm::Tile(arguments.set_size * 1024));
        <algorithm::Tile, flag_data::Rotate, u32>(algorithm::Tile(arguments.set_size * 1024));
        <algorithm::Tile, flag_data::Stripe, [u8; STRIPE_SIZE]>(
            algorithm::Tile(arguments.set_size * 1024)
        );
    );
}

/// Compresses calls to the bench function by only taking sieve size and duration once, as well as
/// stripping away redundant type declarations and structures acting as types from each call.
///
/// The reason this is a macro instead of a function is that types can't guarantee that
/// `SieveExecute` is implemented on `Sieve` for any algorithm and `FlagDataExecute` is implemented
/// on `FlagData` for any flag data type and element type.
///
/// This is meant to be private, but needs to be public when defined inside the crate root.
///
///
/// # Example
///
/// ```
/// benches!(
///     1_000_000; // sieve size
///     5; // duration in seconds
///     <algorithm::Stream, flag_data::Bool, u8>(algorithm::Stream);
///     <algorithm::Stream, flag_data::Bit, u8>(algorithm::Stream);
/// );
/// ```
#[macro_export]
macro_rules! benches {
    ($size: expr; $duration: expr; $(<$A: ty, $T: ty, $D: ty>($algorithm: expr);)+) => {
        $(
            perform_bench::<Sieve<$A, FlagData<$T, $D>, $D>, $A>($algorithm, $size, $duration);
        )+
    };
}

/// Executes a specific bench and prints the result.
fn perform_bench<S: SieveExecute<A>, A: Algorithm>(
    algorithm: A,
    sieve_size: usize,
    duration: usize,
) {
    let mut passes = 0;
    let mut last_sieve = None;
    let mut elapsed = Duration::from_secs(0);
    let id_string = format!("{}-{}-u{}", A::ID_STR, S::ID_STR, S::BITS);

    eprintln!();
    eprintln!(
        "Running {} with {} primes for {} seconds",
        id_string, sieve_size, duration
    );

    let start = Instant::now();

    while elapsed < Duration::from_secs(duration as u64) {
        let mut sieve = S::new(sieve_size, algorithm);
        sieve.sieve();

        last_sieve.replace(sieve);
        passes += 1;
        elapsed = Instant::now() - start;
    }

    let sieve = last_sieve.expect("Used a duration of zero!");
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
        id_string,
        passes,
        elapsed.as_secs_f64(),
        sieve.thread_count(),
        S::FLAG_SIZE
    );
}

/// Contains the arguments of the program.
///
/// This is filled at program startup by the `structopt` crate.
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

#[cfg(test)]
mod test {
    use crate::sieve::flag_data::{FlagData, STRIPE_SIZE};
    use crate::sieve::{algorithm, flag_data, Algorithm, Sieve, SieveExecute};
    use crate::PRIMES_IN_SIEVE;

    /// Generic performing function to reduce code redundancy.
    fn run_test<S: SieveExecute<A>, A: Algorithm>(algorithm: A) {
        for (numbers, primes) in PRIMES_IN_SIEVE {
            let mut sieve = S::new(numbers, algorithm);
            sieve.sieve();
            assert_eq!(
                sieve.count_primes(),
                primes,
                "Numbers {}, expected {}",
                numbers,
                primes
            );
        }
    }

    /// Strips away redundant type parameters and static struct types from a [`run_test`] call.
    macro_rules! test {
        (<$A: ty, $T: ty, $D: ty>($algorithm: expr)) => {
            run_test::<Sieve<$A, FlagData<$T, $D>, $D>, $A>($algorithm);
        };
    }

    #[test]
    fn serial_bool_u8() {
        test!(<algorithm::Serial, flag_data::Bool, u8>(algorithm::Serial));
    }

    #[test]
    fn serial_bool_u32() {
        test!(<algorithm::Serial, flag_data::Bool, u32>(algorithm::Serial));
    }

    #[test]
    fn serial_bit_u8() {
        test!(<algorithm::Serial, flag_data::Bit, u8>(algorithm::Serial));
    }

    #[test]
    fn serial_bit_u32() {
        test!(<algorithm::Serial, flag_data::Bit, u32>(algorithm::Serial));
    }

    #[test]
    fn serial_rotate_u8() {
        test!(<algorithm::Serial, flag_data::Rotate, u8>(algorithm::Serial));
    }

    #[test]
    fn serial_rotate_u32() {
        test!(<algorithm::Serial, flag_data::Rotate, u32>(algorithm::Serial));
    }

    #[test]
    fn serial_stripe() {
        test!(<algorithm::Serial, flag_data::Stripe, [u8; STRIPE_SIZE]>(algorithm::Serial));
    }

    #[test]
    fn stream_bool_u8() {
        test!(<algorithm::Stream, flag_data::Bool, u8>(algorithm::Stream));
    }

    #[test]
    fn stream_bool_u32() {
        test!(<algorithm::Stream, flag_data::Bool, u32>(algorithm::Stream));
    }

    #[test]
    fn stream_bit_u8() {
        test!(<algorithm::Stream, flag_data::Bit, u8>(algorithm::Stream));
    }

    #[test]
    fn stream_bit_u32() {
        test!(<algorithm::Stream, flag_data::Bit, u32>(algorithm::Stream));
    }

    #[test]
    fn stream_rotate_u8() {
        test!(<algorithm::Stream, flag_data::Rotate, u8>(algorithm::Stream));
    }

    #[test]
    fn stream_rotate_u32() {
        test!(<algorithm::Stream, flag_data::Rotate, u32>(algorithm::Stream));
    }

    #[test]
    fn stream_stripe() {
        test!(<algorithm::Stream, flag_data::Stripe, [u8; STRIPE_SIZE]>(algorithm::Stream));
    }

    #[test]
    fn tile_bool_u8() {
        test!(<algorithm::Tile, flag_data::Bool, u8>(algorithm::Tile(1 << 14)));
    }

    #[test]
    fn tile_bool_u32() {
        test!(<algorithm::Tile, flag_data::Bool, u32>(algorithm::Tile(1 << 14)));
    }

    #[test]
    fn tile_bit_u8() {
        test!(<algorithm::Tile, flag_data::Bit, u8>(algorithm::Tile(1 << 14)));
    }

    #[test]
    fn tile_bit_u32() {
        test!(<algorithm::Tile, flag_data::Bit, u32>(algorithm::Tile(1 << 14)));
    }

    #[test]
    fn tile_rotate_u8() {
        test!(<algorithm::Tile, flag_data::Rotate, u8>(algorithm::Tile(1 << 14)));
    }

    #[test]
    fn tile_rotate_u32() {
        test!(<algorithm::Tile, flag_data::Rotate, u32>(algorithm::Tile(1 << 14)));
    }

    #[test]
    fn tile_stripe() {
        test!(<algorithm::Tile, flag_data::Stripe, [u8; STRIPE_SIZE]>(algorithm::Tile(1 << 14)));
    }
}
