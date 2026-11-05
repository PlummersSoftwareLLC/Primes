//! Dependency-free faithful 1-bit base sieve for the Primes software drag race.
//!
//! Representation:
//! - Odds-only candidate mapping: bit `i` represents odd integer `2*i + 1`.
//! - Storage: 1 bit per candidate flag in packed 64-bit words (`u64`).
//! - A cleared bit (`0`) indicates a prime candidate; a set bit (`1`) indicates a composite.
//!
//! Base Sieve Compliance:
//! - Sequentially scans odd numbers starting at 3 up to `floor(sqrt(limit))`.
//! - Small primes (factor <= 129): dense word-parallel reset using compile-time generated masks.
//! - Sparse primes (factor > 129): unrolled 8-step cycle clearing individual composites with immediate bitmasks.

use core::fmt::{self, Display, Formatter};
use core::slice::from_raw_parts_mut;
use std::error::Error;

/// Precomputed bitmap dimensions for an inclusive upper limit.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SieveSize {
    limit: usize,
    total_bits: usize,
    num_words: usize,
    root: usize,
    tail_padding_mask: u64,
}

/// The requested limit cannot be represented natively.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SizeError;

impl SieveSize {
    /// Validate limit and compute buffer dimensions.
    pub fn new(limit: usize) -> Result<Self, SizeError> {
        if limit < 2 {
            return Err(SizeError);
        }
        let total_bits = limit.checked_add(1).ok_or(SizeError)? / 2;
        let num_words = total_bits.div_ceil(64);
        let root = limit.isqrt();

        let rem_bits = total_bits % 64;
        let tail_padding_mask = if rem_bits == 0 {
            0
        } else {
            !((1u64 << rem_bits) - 1)
        };

        Ok(Self {
            limit,
            total_bits,
            num_words,
            root,
            tail_padding_mask,
        })
    }

    /// The largest represented integer.
    pub const fn limit(self) -> usize {
        self.limit
    }

    /// Total bytes allocated for the bitmap.
    pub const fn bytes(self) -> usize {
        self.num_words * 8
    }

    /// Total candidate bits represented.
    pub const fn total_bits(self) -> usize {
        self.total_bits
    }
}

impl Display for SizeError {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter.write_str("limit is invalid or leaves insufficient address space")
    }
}

impl Error for SizeError {}

// =========================================================================
// Dense Resetters (factor <= 129): Word-parallel Chunk Processing
// =========================================================================

const fn compute_word_mask(p: usize, w: usize) -> u64 {
    let mut mask = 0u64;
    let mut b = 0;
    while b < 64 {
        let n = 2 * (w * 64 + b) + 1;
        if n.is_multiple_of(p) {
            mask |= 1u64 << b;
        }
        b += 1;
    }
    mask
}

const fn generate_masks<const P: usize>() -> [u64; P] {
    let mut masks = [0u64; P];
    let mut w = 0;
    while w < P {
        masks[w] = compute_word_mask(P, w);
        w += 1;
    }
    masks
}

struct FastDenseResetter<const P: usize>;

impl<const P: usize> FastDenseResetter<P> {
    const MASKS: [u64; P] = generate_masks::<P>();

    #[inline(never)]
    pub fn reset(words: &mut [u64]) {
        let (chunks, rem) = words.as_chunks_mut::<P>();
        for chunk in chunks {
            let mut w = 0;
            while w < P {
                chunk[w] |= Self::MASKS[w];
                w += 1;
            }
        }

        let mut w = 0;
        while w < rem.len() {
            rem[w] |= Self::MASKS[w];
            w += 1;
        }

        // Restore prime bit
        let factor_idx = P / 2;
        if let Some(w) = words.get_mut(factor_idx / 64) {
            *w &= !(1u64 << (factor_idx % 64));
        }
    }
}

const fn compute_pattern_indices<const P: usize>() -> [usize; 64] {
    let start = P / 2;
    let mut pattern = [0usize; 64];
    let mut i = 0;
    while i < 64 {
        let rel_bit = start + i * P;
        pattern[i] = rel_bit / 64;
        i += 1;
    }
    pattern
}

const fn compute_pattern_masks<const P: usize>() -> [u64; 64] {
    let start = P / 2;
    let mut pattern = [0u64; 64];
    let mut i = 0;
    while i < 64 {
        let rel_bit = start + i * P;
        pattern[i] = 1u64 << (rel_bit % 64);
        i += 1;
    }
    pattern
}

struct FastDenseIndexedResetter<const P: usize>;

impl<const P: usize> FastDenseIndexedResetter<P> {
    const INDICES: [usize; 64] = compute_pattern_indices::<P>();
    const MASKS: [u64; 64] = compute_pattern_masks::<P>();

    #[inline(never)]
    pub fn reset(words: &mut [u64]) {
        let (chunks, rem) = words.as_chunks_mut::<P>();
        for chunk in chunks {
            const CHUNK_SIZE: usize = 16;
            let (idx_chunks, _) = Self::INDICES.as_chunks::<CHUNK_SIZE>();
            let (mask_chunks, _) = Self::MASKS.as_chunks::<CHUNK_SIZE>();
            let mut c = 0;
            while c < idx_chunks.len() {
                let idxs = &idx_chunks[c];
                let masks = &mask_chunks[c];
                let mut i = 0;
                while i < CHUNK_SIZE {
                    chunk[idxs[i]] |= masks[i];
                    i += 1;
                }
                c += 1;
            }
        }

        let mut i = 0;
        while i < 64 {
            let idx = Self::INDICES[i];
            if idx < rem.len() {
                rem[idx] |= Self::MASKS[i];
            } else {
                break;
            }
            i += 1;
        }

        // Restore prime bit
        let factor_idx = P / 2;
        if let Some(w) = words.get_mut(factor_idx / 64) {
            *w &= !(1u64 << (factor_idx % 64));
        }
    }
}

#[inline(always)]
fn reset_dense_prime(factor: usize, words: &mut [u64]) {
    match factor {
        3 => FastDenseResetter::<3>::reset(words),
        5 => FastDenseResetter::<5>::reset(words),
        7 => FastDenseResetter::<7>::reset(words),
        11 => FastDenseResetter::<11>::reset(words),
        13 => FastDenseResetter::<13>::reset(words),
        17 => FastDenseResetter::<17>::reset(words),
        19 => FastDenseResetter::<19>::reset(words),
        23 => FastDenseResetter::<23>::reset(words),
        29 => FastDenseResetter::<29>::reset(words),
        31 => FastDenseResetter::<31>::reset(words),
        37 => FastDenseResetter::<37>::reset(words),
        41 => FastDenseResetter::<41>::reset(words),
        43 => FastDenseResetter::<43>::reset(words),
        47 => FastDenseResetter::<47>::reset(words),
        53 => FastDenseResetter::<53>::reset(words),
        59 => FastDenseResetter::<59>::reset(words),
        61 => FastDenseResetter::<61>::reset(words),
        67 => FastDenseIndexedResetter::<67>::reset(words),
        71 => FastDenseIndexedResetter::<71>::reset(words),
        73 => FastDenseIndexedResetter::<73>::reset(words),
        79 => FastDenseIndexedResetter::<79>::reset(words),
        83 => FastDenseIndexedResetter::<83>::reset(words),
        89 => FastDenseIndexedResetter::<89>::reset(words),
        97 => FastDenseIndexedResetter::<97>::reset(words),
        101 => FastDenseIndexedResetter::<101>::reset(words),
        103 => FastDenseIndexedResetter::<103>::reset(words),
        107 => FastDenseIndexedResetter::<107>::reset(words),
        109 => FastDenseIndexedResetter::<109>::reset(words),
        113 => FastDenseIndexedResetter::<113>::reset(words),
        _ => FastDenseIndexedResetter::<127>::reset(words),
    }
}

/// Complete faithful 1-bit base sieve.
#[derive(Debug)]
pub struct PrimeSieve {
    size: SieveSize,
    raw_words: Vec<u64>,
}

impl PrimeSieve {
    /// Allocate and execute the sieve up to the validated inclusive limit.
    pub fn new(size: SieveSize) -> Self {
        let mut sieve = Self {
            size,
            raw_words: vec![0u64; size.num_words],
        };
        sieve.run();
        sieve
    }

    /// Execute the faithful base sieve algorithm.
    #[allow(
        unsafe_code,
        reason = "bit operations over validated allocation bounds"
    )]
    fn run(&mut self) {
        let words = self.raw_words.as_mut_slice();
        let total_bits = self.size.total_bits;
        let q = self.size.root;

        let bytes = unsafe { from_raw_parts_mut(words.as_mut_ptr() as *mut u8, words.len() * 8) };

        let mut factor = 3;
        while let Some(next_n) = (factor / 2..total_bits).find(|&n| {
            let w = unsafe { *words.get_unchecked(n / 64) };
            (w & (1u64 << (n % 64))) == 0
        }) {
            factor = next_n * 2 + 1;

            if factor > q {
                break;
            }

            if factor <= 129 {
                reset_dense_prime(factor, words);
            } else {
                let start_bit = (factor * factor) >> 1;
                unsafe {
                    clear_factor_unrolled(bytes.as_mut_ptr(), start_bit, factor, total_bits);
                }
            }

            factor += 2;
        }

        // Mask out padding bits beyond the limit in the final word.
        if self.size.tail_padding_mask != 0
            && let Some(last) = words.last_mut()
        {
            *last |= self.size.tail_padding_mask;
        }
    }

    /// Test primality of a number within the sieve limit.
    pub fn is_prime(&self, number: usize) -> bool {
        if number > self.size.limit || number < 2 {
            return false;
        }
        if number == 2 {
            return true;
        }
        if number & 1 == 0 {
            return false;
        }
        let bit = (number - 1) >> 1;
        let w = self.raw_words[bit / 64];
        (w & (1u64 << (bit % 64))) == 0
    }

    /// Count all primes found by the sieve.
    pub fn count_primes(&self) -> usize {
        let mut count = if self.size.limit >= 2 { 1 } else { 0 }; // prime 2
        for (i, &w) in self.raw_words.iter().enumerate() {
            let mut inv = !w;
            if i == 0 {
                inv &= !1; // odd number 1 is not prime
            }
            count += inv.count_ones() as usize;
        }
        count
    }

    /// Sieve size metadata.
    pub const fn size(&self) -> SieveSize {
        self.size
    }
}

// =========================================================================
// Sparse Resetters (factor > 129): 8-step Unrolled Cycle with Byte Immediate Masks
// =========================================================================

const MASKS_MOD1: [u8; 8] = [0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80];
const MASKS_MOD3: [u8; 8] = [0x10, 0x80, 0x04, 0x20, 0x01, 0x08, 0x40, 0x02];
const MASKS_MOD5: [u8; 8] = [0x10, 0x02, 0x40, 0x08, 0x01, 0x20, 0x04, 0x80];
const MASKS_MOD7: [u8; 8] = [0x01, 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02];

#[allow(unsafe_code, reason = "pointers bounded within total_bits allocation")]
#[inline(always)]
unsafe fn clear_factor_unrolled(data: *mut u8, start_bit: usize, factor: usize, total_bits: usize) {
    match (factor & 7) as u8 {
        1 => unsafe { clear_factor_const_masks::<1>(data, start_bit, factor, total_bits) },
        3 => unsafe { clear_factor_const_masks::<3>(data, start_bit, factor, total_bits) },
        5 => unsafe { clear_factor_const_masks::<5>(data, start_bit, factor, total_bits) },
        _ => unsafe { clear_factor_const_masks::<7>(data, start_bit, factor, total_bits) },
    }
}

#[allow(
    unsafe_code,
    reason = "pointer offsets stay strictly within checked total_bits limit"
)]
#[inline(always)]
unsafe fn clear_factor_const_masks<const PATTERN: u8>(
    data: *mut u8,
    start_bit: usize,
    factor: usize,
    total_bits: usize,
) {
    let masks = match PATTERN {
        1 => MASKS_MOD1,
        3 => MASKS_MOD3,
        5 => MASKS_MOD5,
        _ => MASKS_MOD7,
    };

    let step8_bits = factor << 3;
    if start_bit + step8_bits > total_bits {
        let mut bit_idx = start_bit;
        unsafe {
            while bit_idx < total_bits {
                *data.add(bit_idx >> 3) |= 1 << (bit_idx & 7);
                bit_idx += factor;
            }
        }
        return;
    }

    let base_byte = start_bit >> 3;
    let o0 = 0;
    let o1 = ((start_bit + factor) >> 3) - base_byte;
    let o2 = ((start_bit + 2 * factor) >> 3) - base_byte;
    let o3 = ((start_bit + 3 * factor) >> 3) - base_byte;
    let o4 = ((start_bit + 4 * factor) >> 3) - base_byte;
    let o5 = ((start_bit + 5 * factor) >> 3) - base_byte;
    let o6 = ((start_bit + 6 * factor) >> 3) - base_byte;
    let o7 = ((start_bit + 7 * factor) >> 3) - base_byte;

    let num_cycles = (total_bits - 1 - start_bit) / step8_bits;
    unsafe {
        let mut ptr = data.add(base_byte);
        let end_ptr = ptr.add(num_cycles * factor);

        while ptr < end_ptr {
            *ptr.add(o0) |= masks[0];
            *ptr.add(o1) |= masks[1];
            *ptr.add(o2) |= masks[2];
            *ptr.add(o3) |= masks[3];
            *ptr.add(o4) |= masks[4];
            *ptr.add(o5) |= masks[5];
            *ptr.add(o6) |= masks[6];
            *ptr.add(o7) |= masks[7];
            ptr = ptr.add(factor);
        }

        let mut bit_idx = start_bit + num_cycles * step8_bits;
        while bit_idx < total_bits {
            *data.add(bit_idx >> 3) |= 1 << (bit_idx & 7);
            bit_idx += factor;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_standard_historical_sizes() {
        let cases = [
            (10, 4),
            (100, 25),
            (1_000, 168),
            (10_000, 1_229),
            (100_000, 9_592),
            (1_000_000, 78_498),
            (10_000_000, 664_579),
        ];
        for (limit, expected) in cases {
            let size = SieveSize::new(limit).unwrap();
            let sieve = PrimeSieve::new(size);
            assert_eq!(
                sieve.count_primes(),
                expected,
                "Failed count for limit {limit}"
            );
        }
    }

    #[test]
    fn test_small_primes() {
        let size = SieveSize::new(100).unwrap();
        let sieve = PrimeSieve::new(size);
        let expected = [
            2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83,
            89, 97,
        ];
        for n in 0..=100 {
            assert_eq!(
                sieve.is_prime(n),
                expected.contains(&n),
                "is_prime mismatch at {n}"
            );
        }
    }
}
