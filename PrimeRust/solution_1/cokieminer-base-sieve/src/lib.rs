//! Dependency-free faithful 1-bit base sieve for the Primes software drag race.
//!
//! Representation:
//! - Odds-only candidate mapping: bit `i` represents odd integer `2*i + 1`.
//! - Storage: 1 bit per candidate flag in packed 64-bit words (`u64`).
//! - A cleared bit (`0`) indicates a prime candidate; a set bit (`1`) indicates a composite.
//! - The bitmap has a canonical byte order: logical bit `i` lives at memory byte
//!   `i / 8`, bit `i % 8`. On little-endian targets this matches the native
//!   `u64` layout. On big-endian targets the byte order within each word is
//!   reversed, and `logical_bit_mask` maps logical bits onto native word bits at
//!   compile time.
//!
//! Base Sieve Compliance:
//! - Sequentially scans odd numbers starting at 3 up to `floor(sqrt(limit))`.
//! - Small primes (factor <= 129): dense word-parallel reset using compile-time generated masks.
//! - Sparse primes (factor > 129): unrolled 8-step byte cycle using the canonical
//!   byte order, so the same fast path works on every target endianness. For
//!   small bitmaps, all sparse factors are applied one 32 KB block at a time,
//!   keeping each block in L1 cache while every factor visits it.

use core::fmt::{self, Display, Formatter};
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
        let tail_padding_mask = tail_padding_mask(rem_bits);

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

        // On small bitmaps, collect the sparse factors and apply them block by
        // block afterwards. This keeps one 32 KB block in L1 while all factors
        // visit it. Requiring q >= 137 means at least two sparse factors exist,
        // so the block sweep has reuse to exploit. Deferring the marks cannot
        // hide a candidate factor: the size cap limits the scan to q <= 1448,
        // while a composite with both factors above 129 is at least
        // 131^2 = 17161.
        let block_sparse = q >= 137 && self.size.bytes() <= 128 * 1024;
        let mut sparse_factors = if block_sparse {
            Vec::with_capacity(256)
        } else {
            Vec::new()
        };

        let mut factor = 3;
        while let Some(next_n) = (factor / 2..total_bits).find(|&n| {
            let w = unsafe { *words.get_unchecked(n / 64) };
            (w & logical_bit_mask(n)) == 0
        }) {
            factor = next_n * 2 + 1;

            if factor > q {
                break;
            }

            if factor <= 129 {
                reset_dense_prime(factor, words);
            } else if block_sparse {
                sparse_factors.push(factor);
            } else {
                let start_bit = (factor * factor) >> 1;
                clear_factor_unrolled(words, start_bit, factor, total_bits);
            }

            factor += 2;
        }

        if block_sparse {
            mark_sparse_blocked(words, &sparse_factors, total_bits);
        }

        // Mask out padding bits beyond the limit in the final word.
        if self.size.tail_padding_mask != 0 {
            if let Some(last) = words.last_mut() {
                *last |= self.size.tail_padding_mask;
            }
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
        (w & logical_bit_mask(bit)) == 0
    }

    /// Count all primes found by the sieve.
    pub fn count_primes(&self) -> usize {
        let mut count = if self.size.limit >= 2 { 1 } else { 0 }; // prime 2
        for (i, &w) in self.raw_words.iter().enumerate() {
            let mut inv = !w;
            if i == 0 {
                inv &= !logical_bit_mask(0); // odd number 1 is not prime
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
// Dense Resetters (factor <= 129): Word-parallel Chunk Processing
// =========================================================================

#[inline(always)]
fn reset_dense_prime(factor: usize, words: &mut [u64]) {
    // One arm for every odd stride in 3..=129, prime or not. The arms only
    // select a block width; they do not encode which of these numbers are prime.
    match factor {
        3 => FastDenseResetter::<3>::reset(words),
        5 => FastDenseResetter::<5>::reset(words),
        7 => FastDenseResetter::<7>::reset(words),
        9 => FastDenseResetter::<9>::reset(words),
        11 => FastDenseResetter::<11>::reset(words),
        13 => FastDenseResetter::<13>::reset(words),
        15 => FastDenseResetter::<15>::reset(words),
        17 => FastDenseResetter::<17>::reset(words),
        19 => FastDenseResetter::<19>::reset(words),
        21 => FastDenseResetter::<21>::reset(words),
        23 => FastDenseResetter::<23>::reset(words),
        25 => FastDenseResetter::<25>::reset(words),
        27 => FastDenseResetter::<27>::reset(words),
        29 => FastDenseResetter::<29>::reset(words),
        31 => FastDenseResetter::<31>::reset(words),
        33 => FastDenseResetter::<33>::reset(words),
        35 => FastDenseResetter::<35>::reset(words),
        37 => FastDenseResetter::<37>::reset(words),
        39 => FastDenseResetter::<39>::reset(words),
        41 => FastDenseResetter::<41>::reset(words),
        43 => FastDenseResetter::<43>::reset(words),
        45 => FastDenseResetter::<45>::reset(words),
        47 => FastDenseResetter::<47>::reset(words),
        49 => FastDenseResetter::<49>::reset(words),
        51 => FastDenseResetter::<51>::reset(words),
        53 => FastDenseResetter::<53>::reset(words),
        55 => FastDenseResetter::<55>::reset(words),
        57 => FastDenseResetter::<57>::reset(words),
        59 => FastDenseResetter::<59>::reset(words),
        61 => FastDenseResetter::<61>::reset(words),
        63 => FastDenseIndexedResetter::<63>::reset(words),
        65 => FastDenseIndexedResetter::<65>::reset(words),
        67 => FastDenseIndexedResetter::<67>::reset(words),
        69 => FastDenseIndexedResetter::<69>::reset(words),
        71 => FastDenseIndexedResetter::<71>::reset(words),
        73 => FastDenseIndexedResetter::<73>::reset(words),
        75 => FastDenseIndexedResetter::<75>::reset(words),
        77 => FastDenseIndexedResetter::<77>::reset(words),
        79 => FastDenseIndexedResetter::<79>::reset(words),
        81 => FastDenseIndexedResetter::<81>::reset(words),
        83 => FastDenseIndexedResetter::<83>::reset(words),
        85 => FastDenseIndexedResetter::<85>::reset(words),
        87 => FastDenseIndexedResetter::<87>::reset(words),
        89 => FastDenseIndexedResetter::<89>::reset(words),
        91 => FastDenseIndexedResetter::<91>::reset(words),
        93 => FastDenseIndexedResetter::<93>::reset(words),
        95 => FastDenseIndexedResetter::<95>::reset(words),
        97 => FastDenseIndexedResetter::<97>::reset(words),
        99 => FastDenseIndexedResetter::<99>::reset(words),
        101 => FastDenseIndexedResetter::<101>::reset(words),
        103 => FastDenseIndexedResetter::<103>::reset(words),
        105 => FastDenseIndexedResetter::<105>::reset(words),
        107 => FastDenseIndexedResetter::<107>::reset(words),
        109 => FastDenseIndexedResetter::<109>::reset(words),
        111 => FastDenseIndexedResetter::<111>::reset(words),
        113 => FastDenseIndexedResetter::<113>::reset(words),
        115 => FastDenseIndexedResetter::<115>::reset(words),
        117 => FastDenseIndexedResetter::<117>::reset(words),
        119 => FastDenseIndexedResetter::<119>::reset(words),
        121 => FastDenseIndexedResetter::<121>::reset(words),
        123 => FastDenseIndexedResetter::<123>::reset(words),
        125 => FastDenseIndexedResetter::<125>::reset(words),
        127 => FastDenseIndexedResetter::<127>::reset(words),
        129 => FastDenseIndexedResetter::<129>::reset(words),
        _ => unreachable!("dense reset called for factor {factor} outside 3..=129"),
    }
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
            *w &= !logical_bit_mask(factor_idx);
        }
    }
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

const fn compute_word_mask(p: usize, w: usize) -> u64 {
    let mut mask = 0u64;
    let mut b = 0;
    while b < 64 {
        let n = 2 * (w * 64 + b) + 1;
        if n.is_multiple_of(p) {
            mask |= logical_bit_mask(b);
        }
        b += 1;
    }
    mask
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
            *w &= !logical_bit_mask(factor_idx);
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
        pattern[i] = logical_bit_mask(rel_bit);
        i += 1;
    }
    pattern
}

// =========================================================================
// Sparse Resetters (factor > 129): Cache-Blocked Marking
// =========================================================================

/// Apply every sparse factor to one 32 KB bitmap block at a time.
///
/// The unrolled marker assumes marks begin at `factor * factor / 2`, which is
/// not block-aligned. Leading multiples that would break its 8-step cycle are
/// marked directly, then the unrolled loop resumes on the correct phase.
fn mark_sparse_blocked(words: &mut [u64], factors: &[usize], total_bits: usize) {
    const BLOCK_WORDS: usize = 4096; // 32 KB
    let block_bits = BLOCK_WORDS * 64;
    let mut block_start = 0;
    while block_start < total_bits {
        let block_end = (block_start + block_bits).min(total_bits);
        for &factor in factors {
            let start = factor * factor / 2;
            let mut first = start;
            if first < block_start {
                first += (block_start - first).div_ceil(factor) * factor;
            }
            if first >= block_end {
                continue;
            }
            let rem = ((first - start) / factor) & 7;
            if rem != 0 {
                let mut extra = 8 - rem;
                while extra > 0 && first < block_end {
                    words[first >> 6] |= logical_bit_mask(first);
                    first += factor;
                    extra -= 1;
                }
            }
            if first < block_end {
                clear_factor_unrolled(words, first, factor, block_end);
            }
        }
        block_start = block_end;
    }
}

// =========================================================================
// Sparse Resetters (factor > 129): 8-step Unrolled Byte Cycle
// =========================================================================

// The bitmap uses canonical byte order on every target, so the same 8-step
// byte cycle works on both little-endian and big-endian hardware.
const MASKS_MOD1: [u8; 8] = [0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80];
const MASKS_MOD3: [u8; 8] = [0x10, 0x80, 0x04, 0x20, 0x01, 0x08, 0x40, 0x02];
const MASKS_MOD5: [u8; 8] = [0x10, 0x02, 0x40, 0x08, 0x01, 0x20, 0x04, 0x80];
const MASKS_MOD7: [u8; 8] = [0x01, 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02];

#[allow(
    unsafe_code,
    reason = "raw byte pointer is derived from the exclusive word slice and bounded by total_bits"
)]
#[inline(always)]
fn clear_factor_unrolled(words: &mut [u64], start_bit: usize, factor: usize, total_bits: usize) {
    // Derive a byte pointer for the unrolled marking loop. No byte reference is
    // created, so there is no aliasing between byte and word views.
    let data = words.as_mut_ptr().cast::<u8>();
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
    let mut byte_offset = base_byte;
    let mut remaining_cycles = num_cycles;
    unsafe {
        while remaining_cycles > 0 {
            let ptr = data.add(byte_offset);
            *ptr.add(o0) |= masks[0];
            *ptr.add(o1) |= masks[1];
            *ptr.add(o2) |= masks[2];
            *ptr.add(o3) |= masks[3];
            *ptr.add(o4) |= masks[4];
            *ptr.add(o5) |= masks[5];
            *ptr.add(o6) |= masks[6];
            *ptr.add(o7) |= masks[7];
            byte_offset += factor;
            remaining_cycles -= 1;
        }

        let mut bit_idx = start_bit + num_cycles * step8_bits;
        while bit_idx < total_bits {
            *data.add(bit_idx >> 3) |= 1 << (bit_idx & 7);
            bit_idx += factor;
        }
    }
}

// =========================================================================
// Bit Layout and Padding Helpers
// =========================================================================

/// Padding bits beyond the represented candidates in the final word.
const fn tail_padding_mask(rem_bits: usize) -> u64 {
    if rem_bits == 0 {
        return 0;
    }
    let mut mask = 0u64;
    let mut bit = rem_bits;
    while bit < 64 {
        mask |= logical_bit_mask(bit);
        bit += 1;
    }
    mask
}

/// Native word mask for a logical bit index within a word.
///
/// Little-endian words map logical bit `i` to native bit `i`. Big-endian words
/// reverse the byte order within each word, so logical bit `i` maps to native
/// bit `8 * (7 - i / 8) + i % 8`. Both targets therefore share the same
/// canonical memory-byte order.
#[inline(always)]
const fn logical_bit_mask(bit: usize) -> u64 {
    #[cfg(target_endian = "little")]
    {
        1u64 << (bit & 63)
    }
    #[cfg(target_endian = "big")]
    {
        let bit = bit & 63;
        1u64 << (8 * (7 - (bit >> 3)) + (bit & 7))
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
            (18_769, 2_141),
            (100_000, 9_592),
            (1_000_000, 78_498),
            (2_000_000, 148_933),
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
    fn test_logical_bit_layout() {
        for bit in 0..64 {
            let word = logical_bit_mask(bit);
            let bytes = word.to_ne_bytes();
            let byte = bit / 8;
            let mask = 1u8 << (bit % 8);
            for (index, &value) in bytes.iter().enumerate() {
                let expected = if index == byte { mask } else { 0 };
                assert_eq!(value, expected, "logical bit {bit}, byte {index}");
            }
        }
    }

    #[test]
    fn test_tail_padding_layout() {
        for rem_bits in 0..64 {
            let padding = tail_padding_mask(rem_bits);
            for bit in 0..64 {
                let is_padding = rem_bits != 0 && bit >= rem_bits;
                let is_set = padding & logical_bit_mask(bit) != 0;
                assert_eq!(is_set, is_padding, "rem_bits {rem_bits}, bit {bit}");
            }
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
