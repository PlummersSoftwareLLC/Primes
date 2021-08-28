use helper_macros::{extreme_reset, generic_dispatch};

use crate::{
    primes::FlagStorage,
    unrolled::{patterns::pattern_equivalent_skip, ResetterSparseU8},
};

// pub fn test() {
//     let skip = 123_usize;
//     let mut storage = vec![0u64; 10000];
//     let words = &mut storage[..];

//     extreme_reset!(skip, {
//         println!("Fallback");
//     })
// }

pub struct FlagStorageExtremeHybrid {
    words: Vec<u64>,
    length_bits: usize,
}

impl FlagStorage for FlagStorageExtremeHybrid {
    fn create_true(size: usize) -> Self {
        let num_words = size / 64 + (size % 64).min(1);
        Self {
            words: vec![0; num_words],
            length_bits: size,
        }
    }

    #[inline(always)]
    fn reset_flags(&mut self, skip: usize) {
        let words = &mut self.words[..];
        extreme_reset!(skip, {
            // fallback to sparse resetter, and dispatch to the correct one
            // given the equivalent skip
            let equivalent_skip = pattern_equivalent_skip(skip, 8);
            generic_dispatch!(
                equivalent_skip,
                3,
                2,
                17,
                ResetterSparseU8::<N>::reset_sparse(words, skip),
                debug_assert!(
                    false,
                    "this case should not occur skip {} equivalent {}",
                    skip, equivalent_skip
                )
            );
        });
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
