mod bit;
mod bool;

pub use self::bool::Bool;
pub use bit::Bit;

use crate::integer::Integer;

use std::marker::PhantomData;

pub trait FlagDataBase<D: Integer> {
    fn allocate(data_size: usize) -> Self;
    fn slice(&mut self) -> &mut [D];
}

pub trait FlagDataExecute<D: Integer>: FlagDataBase<D> {
    const ID_STR: &'static str;
    const FLAG_SIZE: usize;
    const INIT_VALUE: D;

    fn new(size: usize) -> Self;

    fn fall_through(data: &mut [D], start: usize, interval: usize);

    fn is_prime(&self, index: usize) -> bool;

    fn flag_count(&self) -> usize;

    fn count_primes(&self, size: usize) -> usize;
}

pub struct FlagData<T, D: Integer>(Box<[D]>, PhantomData<T>);

impl<T, D: Integer> FlagDataBase<D> for FlagData<T, D> {
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
