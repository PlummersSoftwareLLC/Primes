// The sieve manager is responsible for creating, deleting and clearing the sieve

// The sieve is a data structure that is used to store the prime numbers.
// - bitstorage is the aligned bitstorage for the sieve
// - bits is the number of bits in the sieve. It is half the sive of the sieve, because we don't store bits for even numbers
struct sieve_t 
{
    void*     bitstorage __attribute__((aligned(cache_line_bytes)));  // Align to cache line
    counter_t bits       __attribute__((aligned(cache_line_bytes)));     
} __attribute__((aligned(cache_line_bytes)));  // Align the whole structure

// create a sieve with a given size including the bitstorage
static inline struct sieve_t * __attribute__((always_inline, malloc, returns_nonnull, assume_aligned(cache_line_bytes), aligned(cache_line_bytes)))
sieve_create(const counter_t size) 
{
    // allocate memory for the sieve and include all the memory voor the bitstorage, so we have only one malloc
    // make sure there is enought room to align the bitstorage on the cache line
    const size_t bitstorage_bytesize = size >> (SHIFT_SIZE + SHIFT_BYTE); // shift >> 1 for not storing even and shift >>3 for bit to bytesize
    const size_t alloc_size = sizeof(struct sieve_t) + bitstorage_bytesize + 10 * cache_line_bytes; // add 2 * cache_line_bytes to make sure we can align the bitstorage
    struct sieve_t *sieve = malloc(alloc_size);
    if (!sieve) { perror("Allocation of sieve failed"); exit(EXIT_FAILURE);  }

    // align bitstorage
    const uintptr_t raw_address = (uintptr_t)sieve + sizeof(struct sieve_t);
    const uintptr_t aligned_address = (raw_address + (cache_line_bytes - 1)) & ~(cache_line_bytes - 1);
    sieve->bitstorage = __builtin_assume_aligned((void *)aligned_address, cache_line_bytes);
    sieve->bits       = size >> SHIFT_SIZE;
    return sieve;
}

// set the entire bitstorage in the sieve to zero
#undef bitbucket_t
#define bitbucket_t uint64v8_t
#define BITBUCKET_BASE(pattern) ((bitbucket_t){ pattern, pattern, pattern, pattern })
static inline void __attribute__((always_inline, nonnull, aligned(cache_line_bytes))) 
sieve_clear(struct sieve_t *sieve) 
{
    counter_t vector_max = index_type(sieve->bits + 1, bitbucket_t);
    bitbucket_t* bitstorage  = __builtin_assume_aligned(sieve->bitstorage, cache_line_bytes);
    bitbucket_t  vector_zero = BITBUCKET_BASE( (uint64_t) 0ULL );
    #pragma GCC ivdep
    #pragma GCC unroll 8
    for (counter_t i = 0; i <= vector_max; i++) {
        bitstorage[i] = vector_zero;
    }
}
#undef bitbucket_t
#undef BITBUCKET_BASE

// delete the sieve
static inline void __attribute__((always_inline, nonnull, aligned(cache_line_bytes))) 
sieve_delete(struct sieve_t *sieve) 
{
    free(sieve);
}