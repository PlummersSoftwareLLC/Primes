use helper_macros::{extreme_reset, generic_dispatch};

use crate::{
    primes::FlagStorage,
    unrolled::{patterns::pattern_equivalent_skip, ResetterSparseU8},
};

/// Storage structure implementing standard linear bit storage, but with a hybrid bit setting strategy:
/// - dense resetting for small skip factors
/// - sparse resetting for larger skip factors
/// This algorithm is functionally equivalent to [`crate::unrolled::FlagStorageUnrolledHybrid`], but we use a procedural
/// macro to write the dense reset functions instead of relying on the compiler to do so implicitly via const-generics.
/// Performance, as a result, is very similar. This method has a slight edge over the const-generics, and is
/// primarily included to demonstrate how this approach can be used in Rust.
pub struct FlagStorageExtremeHybrid {
    words: Box<[u64]>,
    length_bits: usize,
}

impl FlagStorage for FlagStorageExtremeHybrid {
    fn create_true(size: usize) -> Self {
        let num_words = size / 64 + (size % 64).min(1);
        Self {
            words: vec![0; num_words].into_boxed_slice(),
            length_bits: size,
        }
    }

    /// As with [`crate::unrolled::FlagStorageUnrolledHybrid`], this method dispatches
    /// to a dense "extreme" resetter for skip factors below <= 129, and otherwise calls the same
    /// sparse resetter for higher skip factors. The only difference is that we use
    /// the "extreme" dense resetter: [`helper_macros::extreme_reset`]
    #[inline(always)]
    fn reset_flags(&mut self, skip: usize) {
        // sparse resets for skip factors larger than those covered by dense resets
        if skip > 129 {
            let equivalent_skip = pattern_equivalent_skip(skip, 8);
            generic_dispatch!(
                equivalent_skip,
                3,
                2,
                17,
                ResetterSparseU8::<N>::reset_sparse(self.words.as_mut(), skip),
                debug_assert!(
                    false,
                    "this case should not occur skip {} equivalent {}",
                    skip, equivalent_skip
                )
            );
            return;
        }

        // dense resets for all odd numbers in {3, 5, ... =129}
        let words = self.words.as_mut();
        extreme_reset!(skip);
    }

    #[inline(always)]
    fn get(&self, index: usize) -> bool {
        if index >= self.length_bits {
            return false;
        }
        let word = self.words.get(index / 64).unwrap();
        *word & (1 << (index % 64)) == 0
    }
}
