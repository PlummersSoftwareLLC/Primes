use primes::{
    print_results_stderr, report_results_stdout, FlagStorage, FlagStorageBitVector,
    FlagStorageBitVectorRotate, FlagStorageBitVectorStriped, FlagStorageBitVectorStripedBlocks,
    FlagStorageByteVector, PrimeSieve, BLOCK_SIZE_DEFAULT, BLOCK_SIZE_SMALL,
};

use std::{
    thread,
    time::{Duration, Instant},
};
use structopt::StructOpt;

use crate::{unrolled::FlagStorageUnrolledHybrid, unrolled_extreme::FlagStorageExtremeHybrid};

mod unrolled;
mod unrolled_extreme;

pub mod primes {
    use std::{collections::HashMap, time::Duration, usize};

    /// Shorthand for the `u8` bit count to avoid additional conversions.
    const U8_BITS: usize = u8::BITS as usize;

    /// Shorthand for the `u32` bit count to avoid additional conversions.
    const U32_BITS: usize = u32::BITS as usize;

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

        #[cfg(test)]
        pub fn known_results_iter(&self) -> impl Iterator<Item = (&usize, &usize)> {
            self.0.iter()
        }
    }

    /// Trait defining the interface to different kinds of storage, e.g.
    /// bits within bytes, a vector of bytes, etc.
    pub trait FlagStorage {
        /// create new storage for given number of flags pre-initialised to all true
        fn create_true(size: usize) -> Self;

        /// reset all flags for the given `skip` factor (prime); start is implied
        /// from the factor, and handled by the specific storage implementation
        fn reset_flags(&mut self, skip: usize);

        /// get a specific flag
        fn get(&self, index: usize) -> bool;
    }

    /// Recommended start for resetting bits -- at the square of the factor
    pub const fn square_start(skip_factor: usize) -> usize {
        skip_factor * skip_factor / 2
    }

    /// Minimum start for resetting bits -- this is the earliest reset
    /// we can apply without clobbering the factor itself
    pub const fn minimum_start(skip_factor: usize) -> usize {
        skip_factor / 2 + skip_factor
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

        #[inline(always)]
        fn reset_flags(&mut self, skip: usize) {
            let mut i = square_start(skip);

            // unrolled loop - there's a small benefit
            let end_unrolled = self.0.len().saturating_sub(skip * 3);
            while i < end_unrolled {
                // Safety: We have ensured that (i+skip*3) < self.0.len().
                // The compiler will not elide these bounds checks,
                // so there is a performance benefit to using get_unchecked_mut here.
                unsafe {
                    *self.0.get_unchecked_mut(i) = 0;
                    *self.0.get_unchecked_mut(i + skip) = 0;
                    *self.0.get_unchecked_mut(i + skip * 2) = 0;
                    *self.0.get_unchecked_mut(i + skip * 3) = 0;
                }
                i += skip * 4;
            }

            // bounds checks are elided
            while i < self.0.len() {
                self.0[i] = 0;
                i += skip;
            }
        }

        #[inline(always)]
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

    impl FlagStorage for FlagStorageBitVector {
        fn create_true(size: usize) -> Self {
            let num_words = size / U32_BITS + (size % U32_BITS).min(1);
            FlagStorageBitVector {
                words: vec![u32::MAX; num_words],
                length_bits: size,
            }
        }

        #[inline(always)]
        fn reset_flags(&mut self, skip: usize) {
            let mut i = square_start(skip);
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

        #[inline(always)]
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
                words: vec![u32::MAX; num_words],
                length_bits: size,
            }
        }

        #[inline(always)]
        fn reset_flags(&mut self, skip: usize) {
            let start = square_start(skip);
            let mut i = start;
            let roll_bits = skip as u32;
            let mut rolling_mask1 = !(1 << (start % U32_BITS));
            let mut rolling_mask2 = !(1 << ((start + skip) % U32_BITS));

            // if the skip is larger than the word size, we're clearing bits in different
            // words each time: we can unroll the loop
            if skip > U32_BITS {
                let roll_bits_double = roll_bits * 2;
                let unrolled_end = (self.words.len() * U32_BITS).saturating_sub(skip);
                while i < unrolled_end {
                    let word_idx1 = i / U32_BITS;
                    let word_idx2 = (i + skip) / U32_BITS;
                    // Safety: We have ensured that (i+skip) < self.words.len() * U32_BITS.
                    // The compiler will not elide these bounds checks,
                    // so there is a performance benefit to using get_unchecked_mut here.
                    unsafe {
                        *self.words.get_unchecked_mut(word_idx1) &= rolling_mask1;
                        *self.words.get_unchecked_mut(word_idx2) &= rolling_mask2;
                    }
                    rolling_mask1 = rolling_mask1.rotate_left(roll_bits_double);
                    rolling_mask2 = rolling_mask2.rotate_left(roll_bits_double);
                    i += skip * 2;
                }
            }

            while i < self.words.len() * U32_BITS {
                let word_idx = i / U32_BITS;
                // Safety: We have ensured that word_index < self.words.len().
                // Unsafe required to ensure that we elide the bounds check reliably.
                unsafe {
                    *self.words.get_unchecked_mut(word_idx) &= rolling_mask1;
                }
                i += skip;
                rolling_mask1 = rolling_mask1.rotate_left(roll_bits);
            }
        }

        #[inline(always)]
        fn get(&self, index: usize) -> bool {
            if index >= self.length_bits {
                return false;
            }
            let word = self.words.get(index / U32_BITS).unwrap();
            *word & (1 << (index % U32_BITS)) != 0
        }
    }

    /// Storage using a vector of (8-bit) bytes, but individually addressing bits within
    /// each byte for bit-level storage. This is a fun variation I made up myself, but
    /// I'm pretty sure it's not original: someone must have done this before, and it
    /// probably has a name. If you happen to know, let me know :)
    ///
    /// The idea here is to store bits in a different order. First we make use of all the
    /// _first_ bits in each word. Then we come back to the start of the array and
    /// proceed to use the _second_ bit in each word, and so on.
    ///
    /// There is a computation / memory bandwidth tradeoff here. This works well
    /// only for sieves that fit inside the processor cache. For processors with
    /// smaller caches or larger sieves, this algorithm will result in a lot of
    /// cache thrashing due to multiple passes. It really doesn't work well on something
    /// like a raspberry pi.
    ///
    /// [`FlagStorageBitVectorStripedBlocks`] takes a more cache-friendly approach.
    pub struct FlagStorageBitVectorStriped {
        words: Vec<u8>,
        length_bits: usize,
    }

    impl FlagStorage for FlagStorageBitVectorStriped {
        fn create_true(size: usize) -> Self {
            let num_words = size / U8_BITS + (size % U8_BITS).min(1);
            Self {
                words: vec![u8::MAX; num_words],
                length_bits: size,
            }
        }

        #[inline(always)]
        fn reset_flags(&mut self, skip: usize) {
            // determine start bit, and first word
            let start = square_start(skip);
            let words_len = self.words.len();
            let mut bit_idx = start / words_len;
            let mut word_idx = start % words_len;

            while bit_idx < U8_BITS {
                // calculate effective end position: we might have a shorter stripe on the last iteration
                let stripe_start_position = bit_idx * words_len;
                let effective_len = words_len.min(self.length_bits - stripe_start_position);

                // get mask for this bit position
                let mask = !(1 << bit_idx);

                // unrolled loop
                while word_idx < effective_len.saturating_sub(skip) {
                    // Safety: we have ensured that (word_idx + skip*N) < length
                    unsafe {
                        *self.words.get_unchecked_mut(word_idx) &= mask;
                        *self.words.get_unchecked_mut(word_idx + skip) &= mask;
                    }
                    word_idx += skip * 2;
                }

                // remainder
                while word_idx < effective_len {
                    // safety: we have ensured that word_idx < length
                    unsafe {
                        *self.words.get_unchecked_mut(word_idx) &= mask;
                    }
                    word_idx += skip;
                }

                // early termination: this is the last stripe
                if effective_len != words_len {
                    return;
                }

                // bit/stripe complete; advance to next bit
                bit_idx += 1;
                word_idx -= words_len;
            }
        }

        #[inline(always)]
        fn get(&self, index: usize) -> bool {
            if index > self.length_bits {
                return false;
            }
            let word_index = index % self.words.len();
            let bit_index = index / self.words.len();
            let word = self.words.get(word_index).unwrap();
            *word & (1 << bit_index) != 0
        }
    }

    /// This is a variation of [`FlagStorageBitVectorStriped`] that has better locality.
    /// The striped storage is divided up into smaller blocks, and we do multiple
    /// passes over the smaller block rather than the entire sieve.
    ///
    /// The implementation is generic over two parameters, making use of Rust's new
    /// const generics.
    /// - `BLOCK_SIZE` is the size of the blocks, in words (bytes seem to work best)
    /// - `HYBRID` is a boolean specifying whether to enable a slightly different
    ///   algorithm for resetting smaller factors.
    ///   - `false` disables the algorithm, falling back on only the original striped block resets
    ///   - `true` enables the new algorithm for smaller skip factors (under 8)
    ///
    /// Since there are a lot of bits to reset for smaller factors, there is a moderate
    /// performance gain from using the `HYBRID` approach.
    pub struct FlagStorageBitVectorStripedBlocks<const BLOCK_SIZE: usize, const HYBRID: bool> {
        blocks: Vec<[u8; BLOCK_SIZE]>,
        length_bits: usize,
    }

    /// This is the optimal block size for [`FlagStorageBitVectorStriped`] for CPUs
    /// with a fair amount of L1 cache, and works well on AMD Ryzen.
    pub const BLOCK_SIZE_DEFAULT: usize = 16 * 1024;

    /// This is a good block size for [`FlagStorageBitVectorStriped`] for CPUs with
    /// less L1 cache available. It's also useful when running many sieves
    /// in parallel.
    pub const BLOCK_SIZE_SMALL: usize = 4 * 1024;

    impl<const BLOCK_SIZE: usize, const HYBRID: bool>
        FlagStorageBitVectorStripedBlocks<BLOCK_SIZE, HYBRID>
    {
        const BLOCK_SIZE_BITS: usize = BLOCK_SIZE * U8_BITS;

        /// Returns `1` if the `index` for a given `start`, and `skip`
        /// should be reset; `0` otherwise.
        fn should_reset(index: usize, start: usize, skip: usize) -> u8 {
            let rel = index as isize - start as isize;
            if rel % skip as isize == 0 {
                1
            } else {
                0
            }
        }

        /// This is the new algorithm for resetting words in a different
        /// order from the original `reset_flags_general` algorithm below.
        ///
        /// In the original algorithm, we reset all the first bits in the block,
        /// then all the second bits, and so on. This works really well in the
        /// general case, when we have a large skip factor, as we don't touch
        /// most words.
        ///
        /// We use this algorithm when the skip factors are small, say less
        /// than 8. In this case, we're typically touching every word in the block,
        /// and we can expect each word to have multiple bits that need to be reset.
        /// So we proceed in a different order, one word at a time. And for each
        /// word, we reset the bits one by one.
        ///
        /// Note that the algorithm is generic over the `SKIP` factor, which
        /// allows the compiler to do some extra optimisation. Each skip factor
        /// we specify will result in specific code.
        #[inline(always)]
        fn reset_flags_dense<const SKIP: usize>(&mut self) {
            // earliest start to avoid resetting the factor itself
            let start = SKIP / 2 + SKIP;
            debug_assert!(
                start < BLOCK_SIZE,
                "algorithm only correct for small skip factors"
            );
            for (block_idx, block) in self.blocks.iter_mut().enumerate() {
                // Preserve the first bit of one word we know we're going to overwrite
                // with the masks. Its cheaper to put it back afterwards than break the loop
                // into two sections with different rules. Only applicable on the first block:
                // this is the factor itself, and we don't want to reset that flag.
                let preserved_word_mask = if block_idx == 0 {
                    block[SKIP / 2] & 1
                } else {
                    0
                };

                // Calculate the masks we're going to apply first. Note that each mask
                // will reset only a single bit, which is why we have 8 separate masks.
                // Note that we _could_ calculate a single mask word and apply it in a
                // single operation, but I believe that would be against the rules as
                // we would be resetting multiple bits in one operation if we did that.
                let mut mask_set = [[0u8; U8_BITS]; SKIP];
                #[allow(clippy::needless_range_loop)]
                for word_idx in 0..SKIP {
                    for bit in 0..8 {
                        let block_index_offset = block_idx * BLOCK_SIZE * U8_BITS;
                        let bit_index_offset = bit * BLOCK_SIZE;
                        let index = block_index_offset + bit_index_offset + word_idx;
                        mask_set[word_idx][bit] = !(Self::should_reset(index, start, SKIP) << bit);
                    }
                }
                // rebind as immutable
                let mask_set = mask_set;

                /// apply all 8 masks - one for each bit - using a fold, mostly
                /// because folds are fun
                fn apply_masks(word: &mut u8, masks: &[u8; U8_BITS]) {
                    *word = masks.iter().fold(*word, |w, mask| w & mask);
                }

                // run through all exact `SKIP` size chunks - the compiler is able to
                // optimise known sizes quite well.
                block.chunks_exact_mut(SKIP).for_each(|words| {
                    words
                        .iter_mut()
                        .zip(mask_set.iter().copied())
                        .for_each(|(word, masks)| {
                            apply_masks(word, &masks);
                        });
                });

                // run through the remaining stub of fewer than SKIP items
                block
                    .chunks_exact_mut(SKIP)
                    .into_remainder()
                    .iter_mut()
                    .zip(mask_set.iter().copied())
                    .for_each(|(word, masks)| {
                        apply_masks(word, &masks);
                    });

                // restore the first bit on the preserved word in the first block,
                // as noted above
                if block_idx == 0 {
                    block[SKIP / 2] |= preserved_word_mask;
                }
            }
        }

        /// This is the original striped-blocks algorithm, and proceeds to
        /// set the first bit in every applicable word, then the second bit
        /// and so on. This works really well for larger skip sizes, as the
        /// words we need to reset are generally quite far apart.
        #[inline(always)]
        fn reset_flags_general(&mut self, skip: usize) {
            // determine first block, start bit, and first word
            let start = square_start(skip);
            let block_idx_start = start / Self::BLOCK_SIZE_BITS;
            let offset_idx = start % Self::BLOCK_SIZE_BITS;
            let mut bit_idx = offset_idx / BLOCK_SIZE;
            let mut word_idx = offset_idx % BLOCK_SIZE;

            for block_idx in block_idx_start..self.blocks.len() {
                // Safety: we have ensured the block_idx < length
                let block = unsafe { self.blocks.get_unchecked_mut(block_idx) };
                while bit_idx < U8_BITS {
                    // calculate effective end position: we might have a shorter stripe on the last iteration
                    let stripe_start_position =
                        block_idx * Self::BLOCK_SIZE_BITS + bit_idx * BLOCK_SIZE;
                    let effective_len = BLOCK_SIZE.min(self.length_bits - stripe_start_position);

                    // get mask for this bit position
                    let mask = !(1 << bit_idx);

                    // unrolled loop
                    while word_idx < effective_len.saturating_sub(skip * 3) {
                        // Safety: we have ensured that (word_idx + skip*N) < length
                        unsafe {
                            *block.get_unchecked_mut(word_idx) &= mask;
                            *block.get_unchecked_mut(word_idx + skip) &= mask;
                            *block.get_unchecked_mut(word_idx + skip * 2) &= mask;
                            *block.get_unchecked_mut(word_idx + skip * 3) &= mask;
                        }
                        word_idx += skip * 4;
                    }

                    // remainder
                    while word_idx < effective_len {
                        // safety: we have ensured that word_idx < length
                        unsafe {
                            *block.get_unchecked_mut(word_idx) &= mask;
                        }
                        word_idx += skip;
                    }

                    // early termination: this is the last stripe
                    if effective_len != BLOCK_SIZE {
                        return;
                    }

                    // bit/stripe complete; advance to next bit
                    bit_idx += 1;
                    word_idx -= BLOCK_SIZE;
                }

                // block complete; reset bit index and proceed with the next block
                bit_idx = 0;
            }
        }
    }

    impl<const BLOCK_SIZE: usize, const HYBRID: bool> FlagStorage
        for FlagStorageBitVectorStripedBlocks<BLOCK_SIZE, HYBRID>
    {
        fn create_true(size: usize) -> Self {
            let num_blocks = size / Self::BLOCK_SIZE_BITS + (size % Self::BLOCK_SIZE_BITS).min(1);
            Self {
                length_bits: size,
                blocks: vec![[u8::MAX; BLOCK_SIZE]; num_blocks],
            }
        }

        /// Reset flags specified by the sieve. We use the optional
        /// hybrid/dense reset methods for small factors if the
        /// `HYBRID` type parameter is true, with the general
        /// algorithm for higher skip factors. If `HYBRID` is false,
        /// we rely only on the general approach for all skip factors.
        #[inline(always)]
        fn reset_flags(&mut self, skip: usize) {
            if HYBRID {
                match skip {
                    // We only really gain an advantage from dense
                    // resetting up to skip factors under 8, as after
                    // that, we're expecting the resets to be sparse.
                    // We only get called for odd skip factors, so there's
                    // no point adding cases for even numbers.
                    1 => self.reset_flags_dense::<1>(),
                    3 => self.reset_flags_dense::<3>(),
                    5 => self.reset_flags_dense::<5>(),
                    7 => self.reset_flags_dense::<7>(),
                    _ => self.reset_flags_general(skip),
                }
            } else {
                self.reset_flags_general(skip);
            }
        }

        #[inline(always)]
        fn get(&self, index: usize) -> bool {
            if index > self.length_bits {
                return false;
            }
            let block = index / Self::BLOCK_SIZE_BITS;
            let offset = index % Self::BLOCK_SIZE_BITS;
            let bit_index = offset / BLOCK_SIZE;
            let word_index = offset % BLOCK_SIZE;
            let word = self.blocks.get(block).unwrap().get(word_index).unwrap();
            *word & (1 << bit_index) != 0
        }
    }

    /// The actual sieve implementation, generic over the storage. This allows us to
    /// include the storage type we want without re-writing the algorithm each time.
    pub struct PrimeSieve<T: FlagStorage> {
        sieve_size: usize,
        flags: T,
    }

    impl<T> PrimeSieve<T>
    where
        T: FlagStorage,
    {
        #[inline(always)]
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
        #[inline(always)]
        pub fn run_sieve(&mut self) {
            let mut factor = 3;
            let q = (self.sieve_size as f32).sqrt() as usize;

            loop {
                // find next factor - next still-flagged number
                factor = (factor / 2..self.sieve_size / 2)
                    .find(|n| self.flags.get(*n))
                    .unwrap()
                    * 2
                    + 1;

                // check for termination _before_ resetting flags;
                // note: need to check up to and including q, otherwise we
                // fail to catch cases like sieve_size = 1000
                if factor > q {
                    break;
                }

                // reset flags starting at `start`, every `factor`'th flag
                let skip = factor;
                self.flags.reset_flags(skip);

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
            eprintln!();
        }

        let count = prime_sieve.count_primes();
        eprintln!(
            "{:30} Passes: {}, Threads: {}, Time: {:.10}, Passes / sec: {:.2}, Limit: {}, Counts: {}, Valid: {}",
            label,
            passes,
            threads,
            duration.as_secs_f32(),
            passes as f32 / duration.as_secs_f32(),
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
    pub fn report_results_stdout(
        label: &str,
        bits_per_prime: usize,
        duration: Duration,
        passes: usize,
        threads: usize,
    ) {
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

    /// Run variant that uses bit-level storage, using striped storage
    #[structopt(long)]
    bits_striped: bool,

    /// Run variant that uses bit-level storage, using striped storage in blocks
    #[structopt(long)]
    bits_striped_blocks: bool,

    /// Run variant that uses bit-level storage, using striped storage in blocks with
    /// hybrid dense resets for smaller skip factors
    #[structopt(long)]
    bits_striped_hybrid: bool,

    /// Run variant that uses normal, linear bit-level storage, but uses a smarter
    /// `unrolled` algorithm. Collaboration with @GordonBGood.
    #[structopt(long)]
    bits_unrolled: bool,

    /// Run variant that uses normal, linear bit-level storage, but uses a procedural
    /// macro to write the code for the dense resets directly. Collaboration with @GordonBGood.
    #[structopt(long)]
    bits_extreme: bool,

    /// Run variant that uses byte-level storage
    #[structopt(long)]
    bytes: bool,
}

fn main() {
    // command line options are handled by the `structopt` and `clap` crates, which
    // makes life very pleasant indeed. At the cost of a bit of compile time :)
    let opt = CommandLineOptions::from_args();

    let limit = opt.limit;
    let repetitions = opt.repetitions;
    let run_duration = Duration::from_secs(opt.seconds);

    let thread_options = match opt.threads {
        Some(t) => vec![t],
        None => vec![1, num_cpus::get()],
    };

    // run default implementations if no options are specified
    let run_all = [
        opt.bits,
        opt.bits_rotate,
        opt.bits_striped,
        opt.bits_striped_blocks,
        opt.bits_striped_hybrid,
        opt.bits_unrolled,
        opt.bits_extreme,
        opt.bytes,
    ]
    .iter()
    .all(|b| !*b);

    for threads in thread_options {
        print_header(threads, limit, run_duration);

        // not run by default
        if opt.bytes {
            run_implementation::<FlagStorageByteVector>(
                "byte",
                8,
                run_duration,
                threads,
                limit,
                opt.print,
                repetitions,
            );
        }

        // not run by default
        if opt.bits {
            run_implementation::<FlagStorageBitVector>(
                "bit",
                1,
                run_duration,
                threads,
                limit,
                opt.print,
                repetitions,
            );
        }

        if opt.bits_rotate || run_all {
            run_implementation::<FlagStorageBitVectorRotate>(
                "bit-rotate",
                1,
                run_duration,
                threads,
                limit,
                opt.print,
                repetitions,
            );
        }

        // not run by default
        if opt.bits_striped {
            run_implementation::<FlagStorageBitVectorStriped>(
                "bit-striped",
                1,
                run_duration,
                threads,
                limit,
                opt.print,
                repetitions,
            );
        }

        if opt.bits_striped_blocks || run_all {
            run_implementation::<FlagStorageBitVectorStripedBlocks<BLOCK_SIZE_DEFAULT, false>>(
                "bit-striped-blocks16k",
                1,
                run_duration,
                threads,
                limit,
                opt.print,
                repetitions,
            );

            run_implementation::<FlagStorageBitVectorStripedBlocks<BLOCK_SIZE_SMALL, false>>(
                "bit-striped-blocks4k",
                1,
                run_duration,
                threads,
                limit,
                opt.print,
                repetitions,
            );
        }

        if opt.bits_striped_hybrid || run_all {
            run_implementation::<FlagStorageBitVectorStripedBlocks<BLOCK_SIZE_DEFAULT, true>>(
                "bit-striped-hybrid-blocks16k",
                1,
                run_duration,
                threads,
                limit,
                opt.print,
                repetitions,
            );

            run_implementation::<FlagStorageBitVectorStripedBlocks<BLOCK_SIZE_SMALL, true>>(
                "bit-striped-hybrid-blocks4k",
                1,
                run_duration,
                threads,
                limit,
                opt.print,
                repetitions,
            );
        }

        if opt.bits_unrolled || run_all {
            run_implementation::<FlagStorageUnrolledHybrid>(
                "bit-unrolled-hybrid",
                1,
                run_duration,
                threads,
                limit,
                opt.print,
                repetitions,
            );
        }

        if opt.bits_extreme || run_all {
            run_implementation::<FlagStorageExtremeHybrid>(
                "bit-extreme-hybrid",
                1,
                run_duration,
                threads,
                limit,
                opt.print,
                repetitions,
            );
        }
    }
}

fn print_header(threads: usize, limit: usize, run_duration: Duration) {
    eprintln!();
    eprintln!("-------------------------------------------------------");
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
    eprintln!("-------------------------------------------------------");
    eprintln!();
}

/// Run sieve on specific implementation of storage given in T. It will
/// call either the single- or multi-threaded runner, given the number
/// of threads requested.
fn run_implementation<T: 'static + FlagStorage + Send>(
    label: &str,
    bits_per_prime: usize,
    run_duration: Duration,
    num_threads: usize,
    limit: usize,
    print_primes: bool,
    repetitions: usize,
) {
    for _ in 0..repetitions {
        // delay prior to start to allow processor to cool down
        thread::sleep(Duration::from_secs(5));
        match num_threads {
            1 => {
                run_implementation_st::<T>(label, bits_per_prime, run_duration, limit, print_primes)
            }
            _ => run_implementation_mt::<T>(
                label,
                bits_per_prime,
                run_duration,
                num_threads,
                limit,
                print_primes,
            ),
        };
    }
}

/// Single-threaded runner: simpler than spinning up a single thread
/// to do the work.
#[inline(never)]
fn run_implementation_st<T: 'static + FlagStorage + Send>(
    label: &str,
    bits_per_prime: usize,
    run_duration: Duration,
    limit: usize,
    print_primes: bool,
) {
    // run sieves
    let start_time = Instant::now();
    let mut local_passes = 0;
    let mut last_sieve = None;
    while (Instant::now() - start_time) < run_duration {
        last_sieve.take(); // drop prior sieve before creating new one
        let mut sieve: PrimeSieve<T> = primes::PrimeSieve::new(limit);
        sieve.run_sieve();
        last_sieve.replace(sieve);
        local_passes += 1;
    }

    // record end time
    let end_time = Instant::now();

    // get totals and print results based on one of the sieves
    if let Some(sieve) = last_sieve {
        let duration = end_time - start_time;
        // print results to stderr for convenience
        print_results_stderr(
            label,
            &sieve,
            print_primes,
            duration,
            local_passes,
            1,
            &primes::PrimeValidator::default(),
        );
        // and report results to stdout for reporting
        report_results_stdout(label, bits_per_prime, duration, local_passes, 1);
        eprintln!();
    }
}

/// Multithreaded runner
#[inline(never)]
fn run_implementation_mt<T: 'static + FlagStorage + Send>(
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
    #[allow(clippy::needless_collect)]
    let threads: Vec<_> = (0..num_threads)
        .map(|_| {
            std::thread::spawn(move || {
                let mut local_passes = 0;
                let mut last_sieve = None;
                while (Instant::now() - start_time) < run_duration {
                    last_sieve.take(); // drop prior sieve before creating new one
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
            sieve,
            print_primes,
            duration,
            total_passes,
            num_threads,
            &primes::PrimeValidator::default(),
        );
        // and report results to stdout for reporting
        report_results_stdout(label, bits_per_prime, duration, total_passes, num_threads);
        eprintln!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        primes::{minimum_start, square_start, PrimeValidator},
        unrolled_extreme::FlagStorageExtremeHybrid,
    };

    #[test]
    fn sieve_known_correct_bits() {
        sieve_known_correct::<FlagStorageBitVector>();
    }

    #[test]
    fn sieve_known_correct_bits_rolling() {
        sieve_known_correct::<FlagStorageBitVectorRotate>();
    }

    #[test]
    fn sieve_known_correct_bits_striped() {
        sieve_known_correct::<FlagStorageBitVectorStriped>();
    }

    #[test]
    fn sieve_known_correct_bits_striped_blocks_general() {
        // check both sizes
        sieve_known_correct::<FlagStorageBitVectorStripedBlocks<BLOCK_SIZE_DEFAULT, false>>();
        sieve_known_correct::<FlagStorageBitVectorStripedBlocks<BLOCK_SIZE_SMALL, false>>();
    }

    #[test]
    fn sieve_known_correct_bits_striped_blocks_hybrid() {
        // check both sizes
        sieve_known_correct::<FlagStorageBitVectorStripedBlocks<BLOCK_SIZE_DEFAULT, true>>();
        sieve_known_correct::<FlagStorageBitVectorStripedBlocks<BLOCK_SIZE_SMALL, true>>();
    }

    #[test]
    fn sieve_known_correct_bytes() {
        sieve_known_correct::<FlagStorageByteVector>();
    }

    #[test]
    fn sieve_known_correct_unrolled_bits() {
        sieve_known_correct::<FlagStorageUnrolledHybrid>();
    }

    #[test]
    fn sieve_known_correct_extreme_bits() {
        sieve_known_correct::<FlagStorageExtremeHybrid>();
    }

    fn sieve_known_correct<T: FlagStorage>() {
        let validator = PrimeValidator::default();
        for (sieve_size, expected_primes) in validator.known_results_iter() {
            // skip really large sieves -- tests are taking unnecessarily long
            if *sieve_size > 10_000_000 {
                continue;
            }
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

    #[test]
    fn storage_byte_correct() {
        basic_storage_correct::<FlagStorageByteVector>();
    }

    #[test]
    fn storage_bit_correct() {
        basic_storage_correct::<FlagStorageBitVector>();
    }

    #[test]
    fn storage_bit_rotate_correct() {
        basic_storage_correct::<FlagStorageBitVectorRotate>();
    }

    #[test]
    fn storage_bit_striped_correct() {
        basic_storage_correct::<FlagStorageBitVectorStriped>();
    }

    #[test]
    fn storage_bit_striped_block_general_correct() {
        // test small as well as default block sizes
        basic_storage_correct::<FlagStorageBitVectorStripedBlocks<7, false>>();
        basic_storage_correct::<FlagStorageBitVectorStripedBlocks<1024, false>>();
        basic_storage_correct::<FlagStorageBitVectorStripedBlocks<BLOCK_SIZE_SMALL, false>>();
        basic_storage_correct::<FlagStorageBitVectorStripedBlocks<BLOCK_SIZE_DEFAULT, false>>();
    }

    #[test]
    fn storage_bit_striped_block_hybrid_correct() {
        // test small as well as default block sizes
        basic_storage_correct::<FlagStorageBitVectorStripedBlocks<50, true>>();
        basic_storage_correct::<FlagStorageBitVectorStripedBlocks<1024, true>>();
        basic_storage_correct::<FlagStorageBitVectorStripedBlocks<BLOCK_SIZE_SMALL, true>>();
        basic_storage_correct::<FlagStorageBitVectorStripedBlocks<BLOCK_SIZE_DEFAULT, true>>();
    }

    #[test]
    fn storage_bit_unrolled_correct() {
        basic_storage_correct::<FlagStorageUnrolledHybrid>();
    }

    #[test]
    fn storage_bit_extreme_correct() {
        basic_storage_correct::<FlagStorageExtremeHybrid>();
    }

    fn basic_storage_correct<T: FlagStorage>() {
        let size = 100_000;
        let mut storage = T::create_true(size);
        for i in 0..size {
            assert!(storage.get(i), "expected initially true for index {}", i);
        }

        // use realistic start values, as hybrid storage makes assumptions
        // about where to start resetting
        storage.reset_flags(5);
        let s5 = square_start(5);
        let m5 = minimum_start(5);
        for i in 0..size {
            // we're agnostic of these values; storage could set them either way
            // depending on the layout
            if i >= m5 && i <= s5 && (i - m5) % 5 == 0 {
                continue;
            }
            let expected_inv = (i >= s5) && ((i - s5) % 5 == 0);
            assert_eq!(
                storage.get(i),
                !expected_inv,
                "expected value incorrect for index {}",
                i
            );
        }

        storage.reset_flags(13);
        let s13 = square_start(13);
        let m13 = minimum_start(13);
        for i in 0..size {
            // we're agnostic of these values; storage could set them either way
            // depending on the layout
            if i >= m5 && i <= s5 && (i - m5) % 5 == 0 {
                continue;
            }
            if i >= m13 && i <= s13 && (i - m13) % 13 == 0 {
                continue;
            }
            let first = (i >= s5) && ((i - s5) % 5 == 0);
            let second = (i >= s13) && ((i - s13) % 13 == 0);
            let expected_inv = first || second;
            assert_eq!(
                storage.get(i),
                !expected_inv,
                "expected value incorrect for index {}",
                i
            );
        }
    }
}
