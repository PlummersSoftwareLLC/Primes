#include <stdint.h>
#include <stdbool.h>
#include <stdalign.h>
#include <stdatomic.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <time.h>
#include <pthread.h>
#include <ctype.h>
#include <unistd.h>

#if defined(_WIN32)
#  include <malloc.h>
#endif

// ---------------------------------------------------------------------------
// SIMD detection
// ---------------------------------------------------------------------------
#if defined(__aarch64__) || defined(_M_ARM64)
#  include <arm_neon.h>
#  define PRIMEC_VECTOR_NEON 1
#elif defined(__AVX512F__)
#  include <immintrin.h>
#  define PRIMEC_VECTOR_AVX2 1
#elif defined(__AVX2__)
#  include <immintrin.h>
#  define PRIMEC_VECTOR_AVX2 1
#endif

// ---------------------------------------------------------------------------
// Tuning / compiler hint macros
// ---------------------------------------------------------------------------
static const uint64_t DEFAULT_UPPER_LIMIT = 1000000ULL;

#ifndef USE_BRANCH_HINTS
#  define USE_BRANCH_HINTS 1
#endif
#if USE_BRANCH_HINTS && (defined(__GNUC__) || defined(__clang__))
#  define LIKELY(x)   (__builtin_expect(!!(x), 1))
#  define UNLIKELY(x) (__builtin_expect(!!(x), 0))
#else
#  define LIKELY(x)   (x)
#  define UNLIKELY(x) (x)
#endif

#if defined(__GNUC__) || defined(__clang__)
#  define ATTR_ALWAYS_INLINE __attribute__((always_inline))
#  define PRIMEC_RESTRICT    __restrict__
#else
#  define ATTR_ALWAYS_INLINE
#  define PRIMEC_RESTRICT
#endif

// Vectorization hint: tell the compiler loop iterations are independent.
#if defined(__clang__)
#  define VECTORIZE_HINT _Pragma("clang loop vectorize(enable)")
#elif defined(__GNUC__)
#  define VECTORIZE_HINT _Pragma("GCC ivdep")
#else
#  define VECTORIZE_HINT
#endif

// ---------------------------------------------------------------------------
// Wheel of 30: store only numbers coprime to 30.
// 8 residues per wheel period: {1, 7, 11, 13, 17, 19, 23, 29}.
// Bit j of byte i represents n = 30*i + WHEEL30_RESIDUE[j].
// Convention: 0 = prime candidate, 1 = composite.
//
// Array size for limit N: N/30 + 2 bytes (~33 KB for N=1,000,000 vs 62.5 KB
// for odd-only storage).  Composites of 2, 3, 5 are never stored.
// ---------------------------------------------------------------------------
static const uint8_t WHEEL30_RESIDUE[8] = {1, 7, 11, 13, 17, 19, 23, 29};
// WHEEL30_INDEX[r] = bit position for residue r; 0xFF if r not coprime to 30.
static const uint8_t WHEEL30_INDEX[30] = {
    0xFF, 0,    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 1,
    0xFF, 0xFF, 0xFF, 2,    0xFF, 3,    0xFF, 0xFF,
    0xFF, 4,    0xFF, 5,    0xFF, 0xFF, 0xFF, 6,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 7
};
// SMASKS_TABLE[WHEEL30_INDEX[p%30]][qi] = bitmask for the qi-th composite class
// of prime p.  Encodes (1u << WHEEL30_INDEX[(p%30 * WHEEL30_RESIDUE[qi]) % 30])
// for all 8 prime residues x 8 qi values (64 bytes = one cache line).
// Rows indexed by WHEEL30_INDEX[p_mod30] (0..7 for {1,7,11,13,17,19,23,29}).
static const uint8_t SMASKS_TABLE[8][8] = {
    /* p%30= 1 */ {0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80},
    /* p%30= 7 */ {0x02,0x20,0x10,0x01,0x80,0x08,0x04,0x40},
    /* p%30=11 */ {0x04,0x10,0x01,0x40,0x02,0x80,0x08,0x20},
    /* p%30=13 */ {0x08,0x01,0x40,0x20,0x04,0x02,0x80,0x10},
    /* p%30=17 */ {0x10,0x80,0x02,0x04,0x20,0x40,0x01,0x08},
    /* p%30=19 */ {0x20,0x08,0x80,0x02,0x40,0x01,0x10,0x04},
    /* p%30=23 */ {0x40,0x04,0x08,0x80,0x01,0x10,0x20,0x02},
    /* p%30=29 */ {0x80,0x40,0x20,0x10,0x08,0x04,0x02,0x01},
};

// ---------------------------------------------------------------------------
// Extended pre-sieve template: composites of 7, 11, 13, and 17 pre-marked.
//
// Mathematical basis — inductive extension:
//   Base period:     lcm(7,11,13)     =  1001 bytes  (marks composites of {7,11,13})
//   Extended period: lcm(7,11,13,17)  = 17017 bytes  (= 17 x 1001)
//
// The 17017-byte template is built by:
//   Step 1: compute the 1001-byte base pattern for {7,11,13}
//   Step 2: tile it 17 times  ->  17017-byte buffer
//   Step 3: OR in prime 17's composites over the full 17017 bytes
//
// This function is called inside prime_sieve_init, which runs inside the
// timed loop, so the presieve is part of each sieve instance's own state.
//
// Implementation: for each presieve prime p, compute the p-byte periodic
// pattern of which wheel bits are divisible by p.  Tile it to 17017 bytes
// (= lcm(7,11,13,17)/30) using O(log(17017/p)) doubling memcpy calls, then
// OR into buf in one sequential vectorizable pass.  Sequential access lets
// clang emit wide SIMD OR instructions instead of scalar scatter stores.
// ---------------------------------------------------------------------------
static void build_presieve(uint8_t* buf)
{
    memset(buf, 0, 17017);

    const uint64_t presieve_primes[4] = {7, 11, 13, 17};
    uint8_t temp[17017];  // tiled pattern for one prime, then OR'd into buf

    for (int pi = 0; pi < 4; pi++) {
        const uint64_t p = presieve_primes[pi];
        const size_t   period = (size_t)p;

        // Compute the p-byte base pattern: one period of composites of p.
        uint8_t base[17] = {0};  // period <= 17
        for (int j = 0; j < 8; j++) {
            const uint8_t mask = (uint8_t)(1u << j);
            uint64_t n = WHEEL30_RESIDUE[j];
            while (n % p != 0) n += 30;
            base[(size_t)(n / 30) % period] |= mask;
        }

        // Tile base to 17017 bytes using O(log N) doubling memcpy calls.
        // Each doubled block is a multiple of period, so tiling is exact.
        memcpy(temp, base, period);
        size_t filled = period;
        while (filled * 2 <= 17017) {
            memcpy(temp + filled, temp, filled);
            filled *= 2;
        }
        if (filled < 17017)
            memcpy(temp + filled, temp, 17017 - filled);

        // OR temp into buf — sequential read+write, auto-vectorized with SIMD.
        VECTORIZE_HINT
        for (size_t j = 0; j < 17017; j++)
            buf[j] |= temp[j];
    }
    // n=1 (bit 0 of byte 0) is handled by dst[0]=0x01 in bitarray_reinit.
}

// ---------------------------------------------------------------------------
// BitArray
// ---------------------------------------------------------------------------
typedef enum {
    ALLOC_NEW_ARRAY,
    ALLOC_POSIX_ALIGNED,
    ALLOC_WIN_ALIGNED
} AllocationKind;

typedef struct {
    uint8_t*       array;
    size_t         logicalSize;
    size_t         byteSize;
    AllocationKind allocationKind;
} BitArray;

// Wheel-30: 1 byte per 30 numbers; +2 for alignment safety.
static inline size_t ba_storage_size(size_t n) {
    return n / 30 + 2;
}

static void bitarray_init(BitArray* ba, size_t size)
{
    ba->logicalSize = size;
    ba->byteSize    = ba_storage_size(size);

#if defined(_WIN32)
    void* ptr = _aligned_malloc(ba->byteSize, 64);
    if (ptr) {
        ba->array           = (uint8_t*)ptr;
        ba->allocationKind  = ALLOC_WIN_ALIGNED;
    } else {
        ba->array           = (uint8_t*)malloc(ba->byteSize);
        ba->allocationKind  = ALLOC_NEW_ARRAY;
    }
#else
    void* ptr = NULL;
    if (posix_memalign(&ptr, 64, ba->byteSize) == 0 && ptr) {
        ba->array           = (uint8_t*)ptr;
        ba->allocationKind  = ALLOC_POSIX_ALIGNED;
    } else {
        ba->array           = (uint8_t*)malloc(ba->byteSize);
        ba->allocationKind  = ALLOC_NEW_ARRAY;
    }
#endif
    // No memset here: bitarray_reinit overwrites every byte with the presieve
    // template, making a prior zero-fill redundant.
}

// Tile the 17017-byte pre-sieve template across the array.
// Restores dst[0]=0x01 so n=7,11,13,17 are prime candidates; n=1 is composite.
static inline void bitarray_reinit(BitArray* ba, const uint8_t* presieve)
{
    uint8_t* PRIMEC_RESTRICT dst = ba->array;
    const size_t total = ba->byteSize;
    size_t pos = 0;
    while (pos + 17017 <= total) {
        memcpy(dst + pos, presieve, 17017);
        pos += 17017;
    }
    if (pos < total)
        memcpy(dst + pos, presieve, total - pos);
    // Byte 0 has bits set for n=7,11,13,17 (marked as composites for tiling
    // correctness) — restore them as prime candidates; keep only n=1 composite.
    dst[0] = 0x01;
}

static void bitarray_destroy(BitArray* ba)
{
    switch (ba->allocationKind) {
        case ALLOC_POSIX_ALIGNED: free(ba->array);           break;
#if defined(_WIN32)
        case ALLOC_WIN_ALIGNED:   _aligned_free(ba->array);  break;
#endif
        default:                  free(ba->array);           break;
    }
    ba->array = NULL;
}

// Returns true if n is prime.
// Handles 2, 3, 5 specially (not stored in array); multiples of 2/3/5 return false.
static inline bool ATTR_ALWAYS_INLINE bitarray_get(const BitArray* ba, size_t n)
{
    if (n == 2 || n == 3 || n == 5) return true;
    if (n < 7) return false;
    uint8_t bidx = WHEEL30_INDEX[n % 30];
    if (bidx == 0xFF) return false;          // divisible by 2, 3, or 5
    size_t byte_i = n / 30;
    if (byte_i >= ba->byteSize) return false;
    return !(ba->array[byte_i] & (uint8_t)(1u << bidx));
}

// ---------------------------------------------------------------------------
// mark_multiples_wheel30
//
// For small primes (stride <= 256): builds a stride-byte tile encoding all
// composite classes, expands it 16x into a wide tile, then ORs across the
// sieve in stride*16-byte blocks.  No hardcoded prime values — stride is a
// runtime quantity, so the vectorized inner loop uses a runtime bound.
//
// For large primes (stride > 256): 8-deep strided scatter.
// ---------------------------------------------------------------------------
static void mark_multiples_wheel30(uint8_t* PRIMEC_RESTRICT arr,
                                    size_t byteCount, uint64_t p)
{
    const uint32_t p32     = (uint32_t)p;
    const uint32_t p_mod30 = p32 % 30u;
    const size_t   stride  = (size_t)p;
    const uint8_t* smask_row = SMASKS_TABLE[WHEEL30_INDEX[p_mod30]];

    size_t  first_bytes[8];
    uint8_t smasks[8];
    bool    any_valid = false;

    for (int qi = 0; qi < 8; qi++) {
        const uint32_t r_q = WHEEL30_RESIDUE[qi];
        smasks[qi]         = smask_row[qi];

        uint64_t q_start;
        if (r_q >= p_mod30)
            q_start = p - p_mod30 + r_q;
        else
            q_start = p - p_mod30 + 30 + r_q;

        first_bytes[qi] = (size_t)((p * q_start) / 30);
        if (first_bytes[qi] >= byteCount) {
            smasks[qi]      = 0;
            first_bytes[qi] = byteCount;
            continue;
        }
        any_valid = true;
    }

    if (!any_valid) return;

    if (stride <= 148) {
        alignas(32) uint8_t tile[256];
        memset(tile, 0, stride);
        size_t tile_start = 0;
        for (int qi = 0; qi < 8; qi++) {
            if (smasks[qi] == 0) continue;
            const size_t tile_off = (size_t)(p32 * (uint32_t)WHEEL30_RESIDUE[qi] / 30u);
            tile[tile_off] |= smasks[qi];
            const size_t ts_qi = first_bytes[qi] - tile_off + (tile_off ? stride : 0);
            if (ts_qi > tile_start) tile_start = ts_qi;
        }
        for (int qi = 0; qi < 8; qi++) {
            if (smasks[qi] == 0) continue;
            const uint8_t mask = smasks[qi];
            for (size_t pos = first_bytes[qi]; pos < tile_start && pos < byteCount; pos += stride)
                arr[pos] |= mask;
        }

        if (tile_start >= byteCount) return;

        // Generic wide-tile: expand tile 16x then OR across the sieve.
        // Uses a runtime stride (no hardcoded prime enumeration).
        // VECTORIZE_HINT tells the compiler loop iterations are independent (enables SIMD).
        const size_t blk = stride * 16;
        alignas(32) uint8_t wide_tile[256 * 16]; // max buffer: 256*16 = 4096 bytes
        for (size_t k = 0; k < 16; k++)
            memcpy(wide_tile + k * stride, tile, stride);

        uint8_t* PRIMEC_RESTRICT _a = arr;
        const uint8_t* PRIMEC_RESTRICT _wt = wide_tile;
        size_t _b = tile_start;
        for (; _b + blk <= byteCount; _b += blk) {
            VECTORIZE_HINT
            for (size_t _j = 0; _j < blk; _j++)
                _a[_b + _j] |= _wt[_j];
        }
        for (; _b + stride <= byteCount; _b += stride)
            for (size_t _j = 0; _j < stride; _j++)
                _a[_b + _j] |= tile[_j];
        for (size_t _j = 0; _b + _j < byteCount; _j++)
            _a[_b + _j] |= tile[_j];
    } else {
        // Large p: 8-deep strided scatter
        for (int qi = 0; qi < 8; qi++) {
            if (smasks[qi] == 0) continue;
            const uint8_t mask = smasks[qi];
            size_t pos = first_bytes[qi];
            if (byteCount >= stride * 7) {
                const size_t end8 = byteCount - stride * 7;
                while (pos < end8) {
                    arr[pos]            |= mask;
                    arr[pos + stride]   |= mask;
                    arr[pos + stride*2] |= mask;
                    arr[pos + stride*3] |= mask;
                    arr[pos + stride*4] |= mask;
                    arr[pos + stride*5] |= mask;
                    arr[pos + stride*6] |= mask;
                    arr[pos + stride*7] |= mask;
                    pos += stride * 8;
                }
            }
            while (pos < byteCount) { arr[pos] |= mask; pos += stride; }
        }
    }
}

// ---------------------------------------------------------------------------
// prime_sieve
// ---------------------------------------------------------------------------
typedef struct {
    BitArray bits;
    uint8_t  presieve[17017];  // built per instance inside the timed loop
} prime_sieve;

static void prime_sieve_init(prime_sieve* ps, uint64_t n) {
    build_presieve(ps->presieve);       // part of this sieve instance's state
    bitarray_init(&ps->bits, (size_t)n);
    bitarray_reinit(&ps->bits, ps->presieve);
}
static void prime_sieve_destroy(prime_sieve* ps) {
    bitarray_destroy(&ps->bits);
}

static void prime_sieve_run(prime_sieve* ps)
{
    const uint64_t limit   = ps->bits.logicalSize;
    const uint64_t q       = (uint64_t)sqrt((double)limit);
    uint8_t* PRIMEC_RESTRICT arr = ps->bits.array;
    const size_t byteCount = ps->bits.byteSize;
    const size_t qByte     = (size_t)(q / 30);

    for (size_t byte_i = 0; byte_i <= qByte; byte_i++) {
        uint8_t primes = (uint8_t)(~arr[byte_i]);   // 1-bits = prime candidates
        while (primes) {
            int bit_j  = __builtin_ctz((unsigned)primes);
            primes    &= (uint8_t)(primes - 1);      // clear lowest set bit

            uint64_t p = (uint64_t)30 * byte_i + WHEEL30_RESIDUE[bit_j];
            if (UNLIKELY(p < 7)) continue;           // safety: n=1 is marked composite
            if (p <= 17) continue;                   // 7,11,13,17 already pre-sieved
            if (p > q) return;

            mark_multiples_wheel30(arr, byteCount, p);
        }
    }
}

static size_t prime_sieve_count(const prime_sieve* ps)
{
    size_t         count     = 3;   // primes 2, 3, 5 not stored in array
    const uint8_t* arr       = ps->bits.array;
    const uint64_t limit     = ps->bits.logicalSize;
    const size_t   last_byte = (size_t)(limit / 30);
    const uint32_t lim_mod   = (uint32_t)(limit % 30);
    const size_t   byte_limit = (last_byte < ps->bits.byteSize) ? last_byte : ps->bits.byteSize;

    size_t i = 0;
#if defined(PRIMEC_VECTOR_AVX2)
    const __m256i lo_mask  = _mm256_set1_epi8(0x0F);
    const __m256i lut      = _mm256_set_epi8(
        4,3,3,2, 3,2,2,1, 3,2,2,1, 2,1,1,0,
        4,3,3,2, 3,2,2,1, 3,2,2,1, 2,1,1,0);
    const __m256i zero     = _mm256_setzero_si256();
    const __m256i all_ones = _mm256_set1_epi8((char)-1);
    __m256i acc0 = zero, acc1 = zero;
    for (; i + 64 <= byte_limit; i += 64) {
        __m256i v0 = _mm256_xor_si256(_mm256_loadu_si256((const __m256i*)(arr + i     )), all_ones);
        __m256i v1 = _mm256_xor_si256(_mm256_loadu_si256((const __m256i*)(arr + i + 32)), all_ones);
        __m256i cnt0 = _mm256_add_epi8(_mm256_shuffle_epi8(lut, _mm256_and_si256(v0, lo_mask)),
                                       _mm256_shuffle_epi8(lut, _mm256_and_si256(_mm256_srli_epi16(v0, 4), lo_mask)));
        __m256i cnt1 = _mm256_add_epi8(_mm256_shuffle_epi8(lut, _mm256_and_si256(v1, lo_mask)),
                                       _mm256_shuffle_epi8(lut, _mm256_and_si256(_mm256_srli_epi16(v1, 4), lo_mask)));
        acc0 = _mm256_add_epi64(acc0, _mm256_sad_epu8(cnt0, zero));
        acc1 = _mm256_add_epi64(acc1, _mm256_sad_epu8(cnt1, zero));
    }
    for (; i + 32 <= byte_limit; i += 32) {
        __m256i v   = _mm256_xor_si256(_mm256_loadu_si256((const __m256i*)(arr + i)), all_ones);
        __m256i cnt = _mm256_add_epi8(_mm256_shuffle_epi8(lut, _mm256_and_si256(v, lo_mask)),
                                      _mm256_shuffle_epi8(lut, _mm256_and_si256(_mm256_srli_epi16(v, 4), lo_mask)));
        acc0 = _mm256_add_epi64(acc0, _mm256_sad_epu8(cnt, zero));
    }
    __m256i acc  = _mm256_add_epi64(acc0, acc1);
    __m128i lo128 = _mm256_castsi256_si128(acc);
    __m128i hi128 = _mm256_extracti128_si256(acc, 1);
    __m128i s128  = _mm_add_epi64(lo128, hi128);
    count += (size_t)_mm_cvtsi128_si64(s128) + (size_t)_mm_extract_epi64(s128, 1);
#endif
    for (; i < byte_limit; i++)
        count += (size_t)__builtin_popcount((uint32_t)(~arr[i] & 0xFFu));

    if (last_byte < ps->bits.byteSize) {
        uint8_t b = (uint8_t)(~arr[last_byte]);
        for (int j = 0; j < 8; j++)
            if (WHEEL30_RESIDUE[j] <= lim_mod && (b & (uint8_t)(1u << j)))
                count++;
    }

    return count;
}

static const struct { uint64_t limit; int count; } results_table[] = {
    {            10ULL,         4 },
    {           100ULL,        25 },
    {          1000ULL,       168 },
    {         10000ULL,      1229 },
    {        100000ULL,      9592 },
    {       1000000ULL,     78498 },
    {      10000000ULL,    664579 },
    {     100000000ULL,   5761455 },
    {    1000000000ULL,  50847534 },
    {   10000000000ULL, 455052511 },
};

static bool prime_sieve_validate(const prime_sieve* ps)
{
    size_t n = sizeof(results_table) / sizeof(results_table[0]);
    for (size_t i = 0; i < n; i++)
        if (results_table[i].limit == ps->bits.logicalSize)
            return results_table[i].count == (int)prime_sieve_count(ps);
    return false;
}

static void prime_sieve_print(const prime_sieve* ps,
                               bool   showResults,
                               double duration,
                               size_t passes,
                               size_t threads)
{
    if (showResults) fprintf(stderr, "2, ");

    size_t count = (ps->bits.logicalSize >= 2) ? 1 : 0;
    for (uint64_t n = 3; n <= ps->bits.logicalSize; n += 2)
        if (bitarray_get(&ps->bits, (size_t)n)) {
            if (showResults) fprintf(stderr, "%llu, ", (unsigned long long)n);
            count++;
        }
    if (showResults) fprintf(stderr, "\n");

    fprintf(stderr,
        "Passes: %zu, Threads: %zu, Time: %g, Average: %g, "
        "Limit: %zu, Counts: %zu/%zu, Valid: %s\n",
        passes, threads, duration, duration / passes,
        ps->bits.logicalSize, count, prime_sieve_count(ps),
        prime_sieve_validate(ps) ? "Pass" : "FAIL!");
    fprintf(stderr, "\n");
}

// ---------------------------------------------------------------------------
// custom_atoll - like atoll but accepts K/M/G/T magnitude suffixes
// Credit: adapted from PrimeCPP/solution_5
// ---------------------------------------------------------------------------
static long long custom_atoll(const char* value_str)
{
    char input[256];
    strncpy(input, value_str, sizeof(input) - 1);
    input[sizeof(input) - 1] = '\0';

    for (size_t i = 0; input[i]; i++)
        input[i] = (char)toupper((unsigned char)input[i]);

    size_t len = strlen(input);
    if (len == 0) return 0LL;

    char      last       = input[len - 1];
    long long multiplier = 1LL;
    if      (last == 'K') multiplier = 1000LL;
    else if (last == 'M') multiplier = 1000000LL;
    else if (last == 'G') multiplier = 1000000000LL;
    else if (last == 'T') multiplier = 1000000000000LL;

    if (multiplier != 1LL) {
        input[len - 1] = '\0';
        return (long long)(strtod(input, NULL) * multiplier);
    }
    return strtoll(input, NULL, 10);
}

// ---------------------------------------------------------------------------
// Threading
// ---------------------------------------------------------------------------
typedef struct {
    unsigned int   threadIndex;
    uint64_t*      passes;
    atomic_int*    keepRunning;
    uint64_t       upperLimit;
} ThreadArgs;

static void* thread_func(void* arg)
{
    ThreadArgs* a      = (ThreadArgs*)arg;
    uint64_t    passes = 0;
    while (atomic_load_explicit(a->keepRunning, memory_order_relaxed)) {
        prime_sieve sieve;
        prime_sieve_init(&sieve, a->upperLimit);
        prime_sieve_run(&sieve);
        prime_sieve_destroy(&sieve);
        passes++;
    }
    *a->passes = passes;
    return NULL;
}

// ---------------------------------------------------------------------------
// Timing helper
// ---------------------------------------------------------------------------
static double timespec_elapsed(const struct timespec* start, const struct timespec* end)
{
    return (double)(end->tv_sec - start->tv_sec)
         + (double)(end->tv_nsec - start->tv_nsec) * 1e-9;
}

// ---------------------------------------------------------------------------
// main
// Command-line interface adapted from PrimeCPP/solution_5.
// ---------------------------------------------------------------------------
int main(int argc, char** argv)
{
    uint64_t ullLimitRequested = 0;
    int      cThreadsRequested = 0;
    int      cSecondsRequested = 0;
    bool     bPrintPrimes      = false;
    bool     bOneshot          = false;
    bool     bQuiet            = false;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
            printf("Syntax: %s [-t,--threads N] [-s,--seconds N] [-l,--limit N] "
                   "[-1,--oneshot] [-p,--print] [-q,--quiet] [-h]\n", argv[0]);
            return 0;
        } else if (strcmp(argv[i], "-t") == 0 || strcmp(argv[i], "--threads") == 0) {
            if (++i < argc) cThreadsRequested = atoi(argv[i]);
            if (cThreadsRequested < 1) cThreadsRequested = 1;
        } else if (strcmp(argv[i], "-s") == 0 || strcmp(argv[i], "--seconds") == 0) {
            if (++i < argc) cSecondsRequested = atoi(argv[i]);
            if (cSecondsRequested < 1) cSecondsRequested = 1;
        } else if (strcmp(argv[i], "-l") == 0 || strcmp(argv[i], "--limit") == 0) {
            if (++i < argc) {
                long long v = custom_atoll(argv[i]);
                ullLimitRequested = (v >= 1) ? (uint64_t)v : 0;
            }
        } else if (strcmp(argv[i], "-1") == 0 || strcmp(argv[i], "--oneshot") == 0) {
            bOneshot          = true;
            cThreadsRequested = 1;
        } else if (strcmp(argv[i], "-p") == 0 || strcmp(argv[i], "--print") == 0) {
            bPrintPrimes = true;
        } else if (strcmp(argv[i], "-q") == 0 || strcmp(argv[i], "--quiet") == 0) {
            bQuiet = true;
        } else {
            fprintf(stderr, "Unknown argument: %s\n", argv[i]);
            return 0;
        }
    }

    if (!bQuiet) {
        fprintf(stderr, "Primes Benchmark (c) 2025 Dave's Garage - https://github.com/davepl/primes\n");
        fprintf(stderr, "--------------------------------------------------------------------------\n");
    }

    if (bOneshot)
        fprintf(stderr, "Oneshot is on. A single pass will be used to simulate a 5 second run.\n");

    if (bOneshot && (cSecondsRequested > 0 || cThreadsRequested > 1)) {
        fprintf(stderr, "Oneshot option cannot be mixed with second count or thread count.\n");
        return 0;
    }

    int      cPasses      = 0;
    int      cSeconds     = cSecondsRequested ? cSecondsRequested : 5;
    int      cThreads     = cThreadsRequested ? cThreadsRequested : (int)sysconf(_SC_NPROCESSORS_ONLN);
    uint64_t llUpperLimit = ullLimitRequested  ? ullLimitRequested  : DEFAULT_UPPER_LIMIT;

    if (!bQuiet) {
        fprintf(stderr, "Computing primes to %llu on %d thread%s for %d second%s.\n",
                (unsigned long long)llUpperLimit,
                cThreads, cThreads == 1 ? "" : "s",
                cSeconds, cSeconds == 1 ? "" : "s");
    }

    double         duration = 0.0;
    struct timespec tStart, tEnd;

    if (bOneshot) {
        clock_gettime(CLOCK_MONOTONIC, &tStart);
        prime_sieve sieve;
        prime_sieve_init(&sieve, llUpperLimit);
        prime_sieve_run(&sieve);
        prime_sieve_destroy(&sieve);
        clock_gettime(CLOCK_MONOTONIC, &tEnd);
        duration = timespec_elapsed(&tStart, &tEnd);
    } else {
        pthread_t*  threads  = (pthread_t*)  malloc((size_t)cThreads * sizeof(pthread_t));
        uint64_t*   l_passes = (uint64_t*)   calloc((size_t)cThreads,  sizeof(uint64_t));
        ThreadArgs* args     = (ThreadArgs*) malloc((size_t)cThreads * sizeof(ThreadArgs));

        atomic_int keepRunning;
        atomic_init(&keepRunning, 1);

        clock_gettime(CLOCK_MONOTONIC, &tStart);

        for (int i = 0; i < cThreads; i++) {
            args[i].threadIndex = (unsigned int)i;
            args[i].passes      = &l_passes[i];
            args[i].keepRunning = &keepRunning;
            args[i].upperLimit  = llUpperLimit;
            pthread_create(&threads[i], NULL, thread_func, &args[i]);
        }

        struct timespec tNow;
        clock_gettime(CLOCK_MONOTONIC, &tNow);
        long long elapsed_ns = (long long)(tNow.tv_sec  - tStart.tv_sec)  * 1000000000LL
                             + (long long)(tNow.tv_nsec - tStart.tv_nsec);
        long long remain_ns  = (long long)cSeconds * 1000000000LL - elapsed_ns;
        if (remain_ns > 0) {
            struct timespec rem = { remain_ns / 1000000000LL, remain_ns % 1000000000LL };
            nanosleep(&rem, NULL);
        }

        atomic_store(&keepRunning, 0);
        for (int i = 0; i < cThreads; i++) {
            pthread_join(threads[i], NULL);
            cPasses += (int)l_passes[i];
        }

        clock_gettime(CLOCK_MONOTONIC, &tEnd);
        duration = timespec_elapsed(&tStart, &tEnd);

        free(threads);
        free(l_passes);
        free(args);
    }

    if (bOneshot) {
        cPasses  = (int)(1.0 / duration * 5);
        duration = 5.0;
    }

    prime_sieve checkSieve;
    prime_sieve_init(&checkSieve, llUpperLimit);
    prime_sieve_run(&checkSieve);
    bool valid = prime_sieve_validate(&checkSieve);

    if (!bQuiet)
        prime_sieve_print(&checkSieve, bPrintPrimes, duration, (size_t)cPasses, (size_t)cThreads);
    else
        fprintf(stderr, "%d, %g\n", cPasses, duration / cPasses);

    prime_sieve_destroy(&checkSieve);

    // Result line is only written when the count is valid
    if (valid)
        printf("msubra_8of30_extend;%zu;%g;%zu;algorithm=wheel,faithful=yes,bits=1\n",
               (size_t)cPasses, duration, (size_t)cThreads);

    return valid ? 0 : 1;
}
