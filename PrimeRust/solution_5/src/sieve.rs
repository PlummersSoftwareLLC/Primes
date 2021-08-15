//! Provides the sieve, including all generic components.

pub mod algorithm;
pub mod flag_data;

use crate::DataType;
pub use algorithm::Algorithm;
pub use flag_data::FlagDataExecute;

use std::marker::PhantomData;

/// Sieve methods and constants that have the same implementation for each algorithm and therefore
/// only need to be implemented once.
pub trait SieveBase<A: Algorithm> {
    /// Identification string for printing.
    const ID_STR: &'static str;
    /// How many bits each flag occupies.
    const FLAG_SIZE: usize;
    /// How many bits each data element in the sieve has. Used for printing.
    const BITS: usize;

    /// Provides a new Sieve instance.
    ///
    /// # Important
    ///
    /// The flag data in the sieve is **not** initialized. Each algorithm is responsible for doing
    /// that.
    fn new(size: usize, algorithm: A) -> Self;

    /// Returns how many primes were found.
    fn count_primes(&self) -> usize;

    /// Prints all found primes.
    fn print_primes(&self);
}

/// Sieve methods that are implemented once for each algorithm.
pub trait SieveExecute<A: Algorithm>: SieveBase<A> {
    /// Performs the sieving. Other methods can only rely on correct data after this was called.
    fn sieve(&mut self);

    /// Returns the amount of used threads.
    fn thread_count(&self) -> usize;
}

/// A generic sieve that is used by all [`FlagData`](flag_data::FlagData) and [`Algorithm`] types.
///
/// The trait bounds don't really need to be declared here, but it helps with error messages if
/// something goes wrong.
pub struct Sieve<A: Algorithm, F: FlagDataExecute<D>, D: DataType> {
    /// The data container.
    data: F,
    /// The amount of numbers the sieve data represents.
    size: usize,
    /// If the sieve has been run already.
    sieved: bool,
    /// Can carry execution parameters
    algorithm: A,
    /// Only needed because Rust would otherwise refuse to compile because of an unconstrained type
    /// parameter. It does not yet understand that `D` is needed for `F`.
    data_type: PhantomData<D>,
}

impl<A, F, D> SieveBase<A> for Sieve<A, F, D>
where
    A: Algorithm,
    F: FlagDataExecute<D>,
    D: DataType,
{
    const ID_STR: &'static str = F::ID_STR;
    const FLAG_SIZE: usize = F::FLAG_SIZE;
    const BITS: usize = F::BITS;

    #[inline]
    fn new(size: usize, algorithm: A) -> Self {
        Sieve {
            data: F::new(size),
            size,
            sieved: false,
            algorithm,
            data_type: PhantomData,
        }
    }

    fn count_primes(&self) -> usize {
        self.data.count_primes(self.size)
    }

    fn print_primes(&self) {
        eprintln!("Primes up to {}:", self.size);
        eprint!("2");
        for num in 1..(self.size + 1) / 2 {
            if self.data.is_prime(num) {
                eprint!(", {}", num * 2 + 1);
            }
        }
        eprintln!();
    }
}
