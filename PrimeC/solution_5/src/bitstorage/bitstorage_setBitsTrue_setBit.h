#include "../generic/setsuffix.h"

// Set one bit to true
static inline void __attribute__((always_inline, hot, nonnull,  aligned(cache_line_bytes))) 
function(setBitTrue,suffix)(void* restrict bitstorage, const register counter_t index) 
{
    register bitbucket_t* restrict bitstorage_sized = __builtin_assume_aligned(bitstorage,cache_line_bytes);
    bitstorage_sized[index_type(index,bitbucket_t)] |= markmask_type(index, bitbucket_t);
}

// Set one bit to false
static void
function(setBitFalse,suffix)(void* restrict bitstorage, const register counter_t index) 
{
    register bitbucket_t* restrict bitstorage_sized = __builtin_assume_aligned(bitstorage,cache_line_bytes);
    bitstorage_sized[index_type(index,bitbucket_t)] &= ~markmask_type(index, bitbucket_t);
}

// Set bits to true with a step in a range. 
static inline void __attribute__((always_inline, hot, nonnull)) 
function(setBitsTrue_range,suffix)(void* restrict bitstorage, const counter_t range_start, const counter_t step, const counter_t range_stop) 
{
    #pragma GCC ivdep
    #pragma GCC unroll 32
    for(register counter_t index = range_start; index < range_stop; index += step) function(setBitTrue,suffix)(bitstorage, index);
}

// Set bits to true with a step in a range. This function returns the last index that was set
static inline counter_t __attribute__((always_inline, hot, nonnull)) 
function(setBitsTrue_range_return,suffix)(void* restrict bitstorage, const counter_t range_start, const counter_t step, const counter_t range_stop) 
{
    register counter_t index = range_start; // outside the loop te be able to return it
    #pragma GCC ivdep
    #pragma GCC unroll 32
    for(; index < range_stop; index += step) function(setBitTrue,suffix)(bitstorage, index);
    return index;
}

#include "../generic/cleansuffix.h"

