// masks and mask helpers
#define SHIFT_SIZE                  1 // the shift needed to get from SIZE to BIT (1 because even numbers arr not storing in the bitstorage)
#define SHIFT_BYTE                  3 // the shift needed to get from BIT to BYTE

// these are used to describe the max size of the bitstorage (uint64_v8) and use for the benchmarking
#define VECTORWORD_SIZE_BITS        64
#define VECTOR_SIZE_BYTES           64
#define VECTOR_SIZE_BITS            512

// types of vector alignments, used by presets in varianttypes.h
typedef uint64_t uint64v8_t  __attribute__ ((vector_size(64), aligned(cache_line_bytes)));
typedef uint64_t uint64v4_t  __attribute__ ((vector_size(32), aligned(cache_line_bytes)));
typedef uint64_t uint64v2_t  __attribute__ ((vector_size(16), aligned(cache_line_bytes)));
typedef uint32_t uint32v16_t __attribute__ ((vector_size(64), aligned(cache_line_bytes)));
typedef uint32_t uint32v8_t  __attribute__ ((vector_size(32), aligned(cache_line_bytes)));
typedef uint32_t uint32v4_t  __attribute__ ((vector_size(16), aligned(cache_line_bytes)));
typedef uint32_t uint32v2_t  __attribute__ ((vector_size( 8), aligned(cache_line_bytes)));
typedef uint16_t uint16v32_t __attribute__ ((vector_size(64), aligned(cache_line_bytes)));
typedef uint16_t uint16v16_t __attribute__ ((vector_size(32), aligned(cache_line_bytes)));
typedef uint16_t uint16v8_t  __attribute__ ((vector_size(16), aligned(cache_line_bytes)));
typedef uint16_t uint16v4_t  __attribute__ ((vector_size( 8), aligned(cache_line_bytes)));
typedef uint16_t uint16v2_t  __attribute__ ((vector_size( 4), aligned(cache_line_bytes)));

#define builtin_ctz(x)                     __builtin_ctzll((int64_t)(x))
#define shift_calc(bits)                   (builtin_ctz(bits))
#define shift_type(TYPE)                   (shift_calc(sizeof(TYPE)*8))
#define shift_type_from_to(index,from,to)  (sizeof(from) > sizeof(to) ? ((index) << shift_calc(sizeof(from)/sizeof(to))) : ((index) >> shift_calc(sizeof(from)/sizeof(to))))
#define vectorindex_type(index, type)      ((index)>>shift_type(type))
#define index_type(index, type)            ((index)>>shift_type(type)) // type is how the bits are stored, e.g.: uint8_t, uint16_t
#define mask_type(type)                    (sizeof(type)*8-1)
#define bitindex_calc_type(index, type)    ((index) & mask_type(type))
#define bitindex_unsafe_type(index, type)  ((index))
#define bitindex_type(index, type)         ((bitindex_calc_type(index, type)))
#define markmask_calc_type(index, type)    ((type)1ULL << bitindex_calc_type(index, type))
#define markmask_unsafe_type(index, type)  ((type)1ULL << (index))
#define markmask_type(index, type)         (sizeof(type)==8 ? markmask_unsafe_type(index, type) : markmask_calc_type(index, type))
#define bitcount_type(type)                (sizeof(type)*8) 
#define elementcount_type(type, base_type) (sizeof(type)/sizeof(base_type))
#define vectorstart_type(index, type)      ((index) & ~mask_type(type))
#define safe_fill_type(type)               ((type)(~(type)0U))
#define keepmask_type(index, type)         (safe_fill_type(type) << bitindex_calc_type(index, type))
#define chopmask_type(index, type)         (safe_fill_type(type) >> (bitcount_type(type) - bitindex_calc_type(index, type) - 1))
#define index_next_type(index, type)       (vectorstart_type(index, type) + bitcount_type(type))

// globals for tuning
static counter_t global_stripeprime_faster  = 0; // if step > BLOCKSTEP use blocks, else use the whole sieve
static counter_t global_largestep_faster    = 0; // if step < VECTORSTAP_FASTER, use large steps
static counter_t global_blocksize_bits      = 0; // blocksize in bits
static counter_t global_vectorsize          = 0; // vectorsize in bits
static counter_t global_algorithm           = 0; // algorithm to use for the sieve
static counter_t debug_hits                 = 0;
static counter_t debug_final_benchmarking   = 0;
static counter_t debug_final_plan           = 0;
