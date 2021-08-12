//! The [`FlagData`] interface, including all implementations.

mod bit;
mod bool;
mod rotate;
mod stripe;

pub use self::bool::Bool;
pub use bit::Bit;
pub use rotate::Rotate;
pub use stripe::{Stripe, STRIPE_SIZE};

use crate::data_type::DataType;
use std::marker::PhantomData;

/// Flag Data methods that are the same for every data type and therefore only need to be
/// implemented once.
pub trait FlagDataBase<D: DataType> {
    /// Instantiate the flag data without initialising it. `data_size` is the amount of elements.
    fn allocate(data_size: usize) -> Self;
    /// Returns a mutable slice of the flag data.
    fn slice(&mut self) -> &mut [D];
}

/// Flag data methods and constants that are implemented once by each flag data type.
pub trait FlagDataExecute<D: DataType>: FlagDataBase<D> {
    /// The identification string. Used for printing.
    const ID_STR: &'static str;
    /// The amount of bits each flag occupies.
    const FLAG_SIZE: usize;
    /// The initial data value when initialising the flag data.
    const INIT_VALUE: D;
    /// The amount of bits each element contains.
    const BITS: usize;

    /// Creates a new instance of the flag data. Takes the sieve size.
    fn new(size: usize) -> Self;

    /// Performs a sieving pass.
    ///
    /// Is a associated function instead of a method so an algorithm can split the sieve at its
    /// convenience. The start value must compensate for the offset a data slice might have.
    fn fall_through(data: &mut [D], start: usize, interval: usize);

    /// If the number at the flag index is a prime number.
    fn is_prime(&self, index: usize) -> bool;

    /// How many flags it holds.
    fn flag_count(&self) -> usize;

    /// How many primes there are in the struct, truncated at the sieve size.
    fn count_primes(&self, size: usize) -> usize;
}

/// A generic flag data struct that is used for each storage and element data type.
pub struct FlagData<T, D: DataType>(Box<[D]>, PhantomData<T>);

impl<T, D: DataType> FlagDataBase<D> for FlagData<T, D> {
    #[inline]
    fn allocate(data_size: usize) -> Self {
        let data = unsafe {
            Vec::from_raw_parts(
                std::alloc::alloc(std::alloc::Layout::from_size_align_unchecked(
                    data_size * std::mem::size_of::<D>(),
                    4096,
                )) as *mut D,
                data_size,
                data_size,
            )
            .into_boxed_slice()
        };

        FlagData(data, PhantomData)
    }

    #[inline]
    fn slice(&mut self) -> &mut [D] {
        &mut self.0
    }
}
