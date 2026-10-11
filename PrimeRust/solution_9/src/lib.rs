//! Dependency-free Eratosthenes sieve with a runtime-generated wheel pattern.
//!
//! Byte `q`, bit `r`, represents `30*q + RESIDUES[r]`. A set bit denotes a
//! composite. Multiples of 2, 3, and 5 have no flags. For each remaining prime
//! `p`, fixing the multiplier modulo 30 fixes the bit mask; adding 30 to that
//! multiplier advances the byte index by exactly `p`.

use core::{fmt, ptr};

/// Increasing residues coprime to 30; position is the bit number in a byte.
const RESIDUES: [usize; 8] = [1, 7, 11, 13, 17, 19, 23, 29];
/// Bit corresponding to a residue; noncandidate residues map to zero.
const MASKS: [u8; 30] = [
    0, 1, 0, 0, 0, 0, 0, 2, 0, 0, 0, 4, 0, 8, 0, 0, 0, 16, 0, 32, 0, 0, 0, 64, 0, 0, 0, 0, 0, 128,
];
const SEED_PRIMES: [usize; 3] = [7, 11, 13];
const SEED_BYTES: usize = 7 * 11 * 13;
// Only seed-prime bits are restored: the excluded bits represent 1, 49 and 77.
const RESTORE_MASKS: [u8; 3] = [0xfe, 0xdf, 0xef];
// Three times each wheel residue modulo 7, in the order of RESIDUES.
const HOLES: [usize; 8] = [3, 0, 5, 4, 2, 1, 6, 3];
// The extended recipe wins beyond the measured 200,000–250,000 crossover.
// 10,001 bytes selects it from limit 300,000, including on 16-bit pointers.
const EXTENDED_BYTES: usize = 10_001;

/// Runtime dimensions for an inclusive upper limit.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SieveSize {
    limit: usize,
    bytes: usize,
    root: usize,
    factor_mask: u8,
    padding_mask: u8,
    extended: bool,
}

/// The requested limit leaves insufficient address space for wheel offsets.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SizeError;

/// Complete prime bitmap computed from freshly allocated storage.
#[derive(Debug)]
pub struct PrimeSieve {
    size: SieveSize,
    composite: Vec<u8>,
}

impl SieveSize {
    /// Validate the limit and derive bitmap dimensions without allocation.
    ///
    /// # Errors
    /// Returns [`SizeError`] when `limit + 30 * floor(sqrt(limit))` overflows.
    /// This guard bounds the eight starting multiples of every sieving prime.
    pub fn new(limit: usize) -> Result<Self, SizeError> {
        let root = limit.isqrt();
        let guard = root.checked_mul(30).ok_or(SizeError)?;
        limit.checked_add(guard).ok_or(SizeError)?;
        let bytes = (limit / 30).checked_add(1).ok_or(SizeError)?;
        let mut factor_mask = 0;
        let mut padding_mask = 0;
        for (bit, &residue) in RESIDUES.iter().enumerate() {
            if residue <= root % 30 {
                factor_mask |= 1 << bit;
            }
            if residue > limit % 30 {
                padding_mask |= 1 << bit;
            }
        }
        Ok(Self {
            limit,
            bytes,
            root,
            factor_mask,
            padding_mask,
            extended: bytes >= EXTENDED_BYTES,
        })
    }

    /// The largest integer represented by the sieve.
    pub const fn limit(self) -> usize {
        self.limit
    }

    /// Bytes allocated for the candidate flags, including the masked final byte.
    pub const fn bytes(self) -> usize {
        self.bytes
    }

    /// Largest possible prime factor needed to eliminate all composites.
    pub const fn root(self) -> usize {
        self.root
    }
}

impl fmt::Display for SizeError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("limit is too large for native wheel offsets")
    }
}

impl std::error::Error for SizeError {}

impl PrimeSieve {
    /// Allocate and compute every prime up to the validated inclusive limit.
    ///
    /// The periodic seed is computed again for every call. Neither prime lists
    /// nor previous sieve results are reused between passes.
    pub fn new(size: SieveSize) -> Self {
        if size.extended {
            Self::compute::<true>(size)
        } else {
            Self::compute::<false>(size)
        }
    }

    fn compute<const EXTENDED: bool>(size: SieveSize) -> Self {
        let mut sieve = Self {
            size,
            composite: initialize::<EXTENDED>(size),
        };
        sieve.mark_composites::<EXTENDED>();
        sieve
    }

    #[allow(
        unsafe_code,
        reason = "validated factor groups and residue masks prove byte bounds and prime offset arithmetic once"
    )]
    fn mark_composites<const EXTENDED: bool>(&mut self) {
        let first_group = if EXTENDED { 97 / 30 } else { 41 / 30 };
        let last_group = self.size.root() / 30;
        for group in first_group..=last_group {
            // SAFETY: group <= root/30 <= limit/30 < size.bytes(). The bitmap
            // is fully initialized by initialize, and no reference is retained
            // across mark_prime's mutable borrow.
            let mut candidates = !unsafe { *self.composite.get_unchecked(group) };
            // p^2 exceeds every value in p's own 30-integer group, so marking
            // cannot change this byte while its candidates are being consumed.
            if group == first_group {
                // The first group's lower factors are covered by the seed.
                candidates &= if EXTENDED { 0xfe } else { 0xfc };
            }
            if group == last_group {
                candidates &= self.size.factor_mask;
            }
            while candidates != 0 {
                // trailing_zeros is at most 7 for a nonzero byte, on every
                // pointer width. Each group is at most root/30.
                let bit = candidates.trailing_zeros() as usize;
                // SAFETY: group*30 <= root; the last group's mask limits its
                // selected residue to root%30. Earlier groups end below root.
                // Thus the addition fits and produces a prime <= root.
                let prime = unsafe { (group * 30).unchecked_add(RESIDUES[bit]) };
                mark_prime::<EXTENDED>(&mut self.composite, self.size.limit(), prime);
                candidates &= candidates - 1;
            }
        }
    }

    /// Return whether `number` is prime; values above the limit return false.
    pub fn is_prime(&self, number: usize) -> bool {
        if number > self.size.limit() || number < 2 {
            return false;
        }
        if number == 2 || number == 3 || number == 5 {
            return true;
        }
        let mask = MASKS[number % 30];
        mask != 0 && self.composite[number / 30] & mask == 0
    }

    /// Count the primes, using word population counts over the packed flags.
    pub fn count_primes(&self) -> usize {
        let (words, tail) = self.composite.as_chunks::<8>();
        let mut count = [2, 3, 5]
            .into_iter()
            .filter(|&prime| prime <= self.size.limit())
            .count();
        for &bytes in words {
            // A word contributes at most 64 bits, so this cast is exact on
            // 16-, 32-, and 64-bit pointers. The sum cannot exceed the limit.
            count += (!u64::from_ne_bytes(bytes)).count_ones() as usize;
        }
        for &byte in tail {
            // A byte contributes at most eight bits on every pointer width.
            count += (!byte).count_ones() as usize;
        }
        count
    }

    /// The largest represented integer and the bitmap's checked dimensions.
    pub const fn size(&self) -> SieveSize {
        self.size
    }
}

#[allow(
    unsafe_code,
    reason = "initialize only the seed and copy that initialized prefix instead of zeroing the entire allocation"
)]
fn initialize<const EXTENDED: bool>(size: SieveSize) -> Vec<u8> {
    let bytes = size.bytes();
    let mut composite = Vec::<u8>::with_capacity(bytes);
    let data = composite.as_mut_ptr();
    let seed_len = bytes.min(SEED_BYTES);
    // SAFETY: capacity is at least bytes >= seed_len >= 1. u8 needs alignment
    // one, and no references to the allocation exist. This initializes the
    // complete seed prefix before a slice or read-modify-write can observe it.
    let seed = unsafe {
        ptr::write_bytes(data, 0, seed_len);
        core::slice::from_raw_parts_mut(data, seed_len)
    };
    for &prime in &SEED_PRIMES {
        for &residue in &RESIDUES {
            // These are bounded by 13*29 = 377, including on 16-bit targets.
            let multiple = prime * residue;
            let start = multiple / 30;
            if start < seed_len {
                // SAFETY: start < seed_len, p is 7, 11, or 13, and the seed
                // was initialized above. seed_len+8*p <= 1105 fits all pointer
                // widths, and seed is the allocation's sole active borrow.
                unsafe { mark_stream::<EXTENDED>(seed, start, prime, MASKS[multiple % 30]) };
            }
        }
    }

    // Divisibility by 7, 11, and 13 repeats every 30*1001 integers. Copying
    // doubles the initialized prefix; all copied lengths are multiples of
    // 1001 until the final, possibly partial copy. No prime results are cached.
    let mut filled = seed_len;
    while filled < bytes {
        let count = filled.min(bytes - filled);
        // SAFETY: [0,count) is initialized because count <= filled. The
        // destination [filled,filled+count) is disjoint and bounded by bytes,
        // which does not exceed capacity. The seed borrow ended above. The
        // updated prefix is <= bytes, so its length addition cannot overflow.
        unsafe {
            ptr::copy_nonoverlapping(data, data.add(filled), count);
            filled = filled.unchecked_add(count);
        }
    }
    // SAFETY: the prefix grows from seed_len to bytes, and every extension
    // copies initialized u8 values. All bytes are initialized, with capacity
    // at least bytes; no reference into the allocation remains active.
    unsafe { composite.set_len(bytes) };

    // Separate short periods avoid the product of all nine seed primes.
    // Each pair replaces individual strided marks with contiguous byte ORs.
    merge_pair::<{ 17 * 19 }, EXTENDED>(&mut composite, [17, 19]);
    merge_pair::<{ 23 * 29 }, EXTENDED>(&mut composite, [23, 29]);
    merge_pair::<{ 31 * 37 }, EXTENDED>(&mut composite, [31, 37]);
    if EXTENDED {
        merge_pair::<{ 41 * 43 }, true>(&mut composite, [41, 43]);
        merge_pair::<{ 47 * 53 }, true>(&mut composite, [47, 53]);
        merge_pair::<{ 59 * 61 }, true>(&mut composite, [59, 61]);
        merge_pair::<{ 67 * 71 }, true>(&mut composite, [67, 71]);
        merge_pair::<{ 73 * 79 }, true>(&mut composite, [73, 79]);
        merge_pair::<{ 83 * 89 }, true>(&mut composite, [83, 89]);
    }

    // Restore only the selected seed-prime flags. Final-byte padding below
    // masks flags beyond the limit, so only the available prefix is touched.
    let masks: &[u8] = if EXTENDED { &RESTORE_MASKS } else { &[0xfe, 3] };
    for (byte, mask) in composite.iter_mut().zip(masks) {
        *byte &= !mask;
    }
    // SAFETY: SieveSize derives bytes=limit/30+1 >= 1. set_len established
    // exactly bytes initialized values; both indexes are in bounds. The two
    // mutable borrows end in separate statements, including when bytes is one.
    unsafe {
        *composite.get_unchecked_mut(0) |= 1;
        *composite.get_unchecked_mut(bytes - 1) |= size.padding_mask;
    }
    composite
}

#[allow(
    unsafe_code,
    reason = "short seed periods give explicit start, initialization, and stride bounds for the marking kernel"
)]
fn merge_pair<const PERIOD: usize, const EXTENDED: bool>(composite: &mut [u8], primes: [usize; 2]) {
    // PERIOD is the product of two distinct primes. Divisibility repeats after
    // 30*PERIOD integers, hence after PERIOD wheel bytes. The scratch buffer
    // holds divisibility flags, including the two primes themselves.
    let mut pattern = [0_u8; PERIOD];
    let length = PERIOD.min(composite.len());
    for prime in primes {
        for &residue in &RESIDUES {
            // The largest seed product is 89*29 = 2581 on all pointer widths.
            let multiple = prime * residue;
            let start = multiple / 30;
            if start < length {
                // SAFETY: start < length, p is a positive seed prime <= 89,
                // and the pattern is initialized. length+8*p <= 8099 fits even
                // 16-bit usize. The mutable pattern borrow is exclusive.
                unsafe {
                    mark_stream::<EXTENDED>(
                        &mut pattern[..length],
                        start,
                        prime,
                        MASKS[multiple % 30],
                    );
                }
            }
        }
    }
    for block in composite.chunks_mut(PERIOD) {
        merge_pattern(block, &pattern);
    }
}

#[allow(
    unsafe_code,
    reason = "the validated wheel guard bounds products and byte progressions before prime marking begins"
)]
fn mark_prime<const EXTENDED: bool>(composite: &mut [u8], limit: usize, prime: usize) {
    let remainder = prime % 30;
    let base = prime - remainder;
    let base_hole = if EXTENDED { (3 * (base % 7)) % 7 } else { 0 };
    for (&residue, residue_hole) in RESIDUES.iter().zip(HOLES) {
        // Each stream starts at its first multiplier >= p, hence at >= p^2.
        // Its multiplier is at most p+29. SieveSize checks limit+30*root,
        // which bounds all these starting products for p <= root.
        let offset = if residue < remainder {
            residue + 30
        } else {
            residue
        };
        // SAFETY: p <= root, multiplier <= p+29. SieveSize checks
        // limit+30*root, bounding the addition and p*multiplier on all widths.
        let multiple = unsafe { prime.unchecked_mul(base.unchecked_add(offset)) };
        if multiple <= limit {
            if !EXTENDED {
                // SAFETY: multiple/30 < len, and the checked limit+30*root
                // guard bounds len+8*p. The initialized borrow is exclusive.
                unsafe {
                    mark_stream::<false>(composite, multiple / 30, prime, MASKS[multiple % 30])
                };
                continue;
            }
            // Adding 30 to the multiplier adds 30*3 = 6 mod 7 to its hole.
            // The sum is <= 18; at most two subtractions reduce it modulo 7.
            let phase = base_hole + residue_hole + if residue < remainder { 6 } else { 0 };
            let phase = if phase >= 14 {
                phase - 14
            } else if phase >= 7 {
                phase - 7
            } else {
                phase
            };
            // SAFETY: multiple/30 <= limit/30 < len; all bytes are initialized.
            // p > 0, and len+8*p <= limit+30*root, whose native
            // representability was checked by SieveSize. The borrow is exclusive.
            // hole < 7 because it is reduced modulo 7. Multipliers advance by
            // 30 = 2 mod 7, so hole = -m/2 = 3*m mod 7 is the first multiple
            // of 7. Every corresponding product is already marked by the seed.
            unsafe {
                mark_non7_stream(composite, multiple / 30, prime, MASKS[multiple % 30], phase);
            }
        }
    }
}

/// OR a periodic pattern prefix into a block no longer than the pattern.
fn merge_pattern(block: &mut [u8], pattern: &[u8]) {
    let source = &pattern[..block.len()];
    let (wide, remainder) = block.as_chunks_mut::<32>();
    let (wide_masks, remaining_masks) = source.as_chunks::<32>();
    // Each iteration merges 32 independent bytes with one loop branch. The
    // fixed two-lane representation expresses the wide work in the source;
    // the integer ORs require no target-specific instructions or alignment.
    for (bytes, masks) in wide.iter_mut().zip(wide_masks) {
        let (pairs, _) = bytes.as_chunks_mut::<16>();
        let (pair_masks, _) = masks.as_chunks::<16>();
        pairs[0] =
            (u128::from_ne_bytes(pairs[0]) | u128::from_ne_bytes(pair_masks[0])).to_ne_bytes();
        pairs[1] =
            (u128::from_ne_bytes(pairs[1]) | u128::from_ne_bytes(pair_masks[1])).to_ne_bytes();
    }
    let (words, tail) = remainder.as_chunks_mut::<8>();
    let (masks, mask_tail) = remaining_masks.as_chunks::<8>();
    for (word, mask) in words.iter_mut().zip(masks) {
        *word = (u64::from_ne_bytes(*word) | u64::from_ne_bytes(*mask)).to_ne_bytes();
    }
    for (byte, &mask) in tail.iter_mut().zip(mask_tail) {
        *byte |= mask;
    }
}

/// Mark a progression while omitting the flags already set by the 7 seed.
///
/// # Safety
/// `start < len`, `stride > 0`, `hole < 7`, and `len + 8*stride` must fit
/// `usize`. All bytes are initialized. The flags at start+(hole+7*k)*stride
/// are already set; the mutable slice is the complete marking destination.
#[allow(
    unsafe_code,
    reason = "the seed invariant removes every seventh redundant store and validates the unrolled stream bounds"
)]
unsafe fn mark_non7_stream(
    composite: &mut [u8],
    start: usize,
    stride: usize,
    mask: u8,
    hole: usize,
) {
    let data = composite.as_mut_ptr();
    let length = composite.len();
    // SAFETY: start < len and hole <= 6 bound the prefix by len+6*stride.
    // Every offset is <= 7*stride; all fit the checked len+8*stride contract.
    let (prefix_end, step2, step3, step4, step5, step7) = unsafe {
        (
            start.unchecked_add(hole.unchecked_mul(stride)),
            stride.unchecked_mul(2),
            stride.unchecked_mul(3),
            stride.unchecked_mul(4),
            stride.unchecked_mul(5),
            stride.unchecked_mul(7),
        )
    };
    let prefix_limit = prefix_end.min(length);
    let mut index = start;
    while index < prefix_limit {
        // SAFETY: index < prefix_limit <= len. All bytes are initialized and
        // exclusively borrowed, and the next index is below len+8*stride.
        unsafe {
            *data.add(index) |= mask;
            index = index.unchecked_add(stride);
        }
    }
    // SAFETY: prefix_end < len+6*stride; skipping its already-set flag leaves
    // an index below len+7*stride. No pointer is formed unless it is in bounds.
    index = unsafe { prefix_end.unchecked_add(stride) };
    let wide_end = length.saturating_sub(step5);
    while index < wide_end {
        // SAFETY: index < len-5*stride proves all six stores are in bounds.
        // Every seventh flag is already set by contract. The exclusive bytes
        // are initialized; the next index is below the checked len+8*stride.
        unsafe {
            *data.add(index) |= mask;
            *data.add(index.unchecked_add(stride)) |= mask;
            *data.add(index.unchecked_add(step2)) |= mask;
            *data.add(index.unchecked_add(step3)) |= mask;
            *data.add(index.unchecked_add(step4)) |= mask;
            *data.add(index.unchecked_add(step5)) |= mask;
            index = index.unchecked_add(step7);
        }
    }
    while index < length {
        // SAFETY: fewer than six flags remain, so the next seventh flag is
        // outside the slice. The loop bounds prove each store is in range;
        // initialized, exclusive bytes and len+8*stride bound the increment.
        unsafe {
            *data.add(index) |= mask;
            index = index.unchecked_add(stride);
        }
    }
}

/// Mark one constant-mask arithmetic progression without division or slicing.
///
/// # Safety
/// `start < composite.len()`, `stride > 0`, and `composite.len() + 8*stride`
/// must fit `usize`. The entire slice must contain initialized bytes.
#[allow(
    unsafe_code,
    reason = "validated wheel dimensions eliminate division and repeated slice checks in the strided kernel"
)]
unsafe fn mark_stream<const EXTENDED: bool>(
    composite: &mut [u8],
    start: usize,
    stride: usize,
    mask: u8,
) {
    let data = composite.as_mut_ptr();
    let mut index = start;
    if EXTENDED {
        // SAFETY: len+8*stride fits by contract, hence every multiple of stride
        // up to eight also fits. These offsets are invariant for the whole stream.
        let (step2, step3, step4, step5, step6, step7, step8) = unsafe {
            (
                stride.unchecked_mul(2),
                stride.unchecked_mul(3),
                stride.unchecked_mul(4),
                stride.unchecked_mul(5),
                stride.unchecked_mul(6),
                stride.unchecked_mul(7),
                stride.unchecked_mul(8),
            )
        };
        let wide_end = composite.len().saturating_sub(step7);
        while index < wide_end {
            // SAFETY: index < len-7*stride proves all eight indexes are below len.
            // The whole allocation is initialized and exclusively borrowed. The
            // next index is below len+8*stride, which fits by the caller's contract.
            unsafe {
                *data.add(index) |= mask;
                *data.add(index.unchecked_add(stride)) |= mask;
                *data.add(index.unchecked_add(step2)) |= mask;
                *data.add(index.unchecked_add(step3)) |= mask;
                *data.add(index.unchecked_add(step4)) |= mask;
                *data.add(index.unchecked_add(step5)) |= mask;
                *data.add(index.unchecked_add(step6)) |= mask;
                *data.add(index.unchecked_add(step7)) |= mask;
                index = index.unchecked_add(step8);
            }
        }
    }
    while index < composite.len() {
        // SAFETY: the loop condition proves index < len. The bytes are
        // initialized and exclusively borrowed; index+stride < len+8*stride
        // fits by contract. Pointers are formed only for in-bounds indexes.
        unsafe {
            *data.add(index) |= mask;
            index = index.unchecked_add(stride);
        }
    }
}
