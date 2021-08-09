pub mod algorithm;
pub mod flag_data;

use crate::Integer;
pub use algorithm::Algorithm;
pub use flag_data::FlagDataExecute;

use std::marker::PhantomData;

pub trait SieveBase<A: Algorithm> {
    const ID_STR: &'static str;
    const FLAG_SIZE: usize;
    const BITS: usize;

    fn new(size: usize, algorithm: A) -> Self;

    fn count_primes(&self) -> usize;

    fn print_primes(&self);
}

pub trait SieveExecute<A: Algorithm>: SieveBase<A> {
    fn sieve(&mut self);

    fn thread_count(&self) -> usize;
}

/// A generic sieve that is used by all [`FlagData`] and [`Algorithm`] types.
///
/// The trait bounds don't really need to be declared here, but it helps with error messages if
/// something goes wrong.
pub struct Sieve<A: Algorithm, F: FlagDataExecute<D>, D: Integer> {
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
    D: Integer,
{
    const ID_STR: &'static str = F::ID_STR;
    const FLAG_SIZE: usize = F::FLAG_SIZE;
    const BITS: usize = D::BITS;

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
