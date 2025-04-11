// Finds the index of the next unset (false) bit in a bitstorage, starting from a given index.

#include "../generic/setsuffix.h"

#ifndef variant
#define bitbucket_t uint8_t
#endif

static inline counter_t __attribute__((always_inline, hot, nonnull, aligned(cache_line_bytes))) 
function(checkBitTrue,suffix)(const void* restrict bitstorage, register counter_t index) 
{
    bitbucket_t* restrict bitstorage_sized = __builtin_assume_aligned(bitstorage, cache_line_bytes);
    return (bitstorage_sized[index_type(index, bitbucket_t)] & markmask_type(index, bitbucket_t));
}

static inline counter_t __attribute__((always_inline, hot, nonnull)) 
function(checkBitFalse,suffix)(const void* restrict bitstorage, register counter_t index) 
{
    return !checkBitTrue(bitstorage, index);
}

static inline counter_t __attribute__((always_inline)) 
function(countInvalidInStripe,suffix)(const void* restrict bitstorage, const counter_t range_start, const counter_t step, const counter_t range_stop) 
{
    counter_t count = 0;
    for (counter_t index = range_start; index < range_stop; index += step) {
        if (checkBitFalse(bitstorage, index)) count++;
    }
    return count;
}

static inline counter_t __attribute__((always_inline)) 
function(countBitsTrue,suffix)(const void* bitstorage, const counter_t range_start, const counter_t range_stop) 
{
    counter_t count = 0;
    for (counter_t index = range_start; index < range_stop; index++) {
        if (checkBitTrue(bitstorage, index)) count++;
    }
    return count;
}

static inline counter_t __attribute__((always_inline)) 
function(faultInvalidInStripe,suffix)(const void* restrict bitstorage, const counter_t range_start, const counter_t step, const counter_t range_stop) 
{
    counter_t count = 0;
    for (counter_t index = range_start; index < range_stop; index += step) {
        count += checkBitFalse(bitstorage, index) ? 1 : 0;
        if (count) {
            printf("In range from %ju to %ju, found bit not set at index %ju\n", (uintmax_t)range_start, (uintmax_t)range_stop, (uintmax_t)index);
            exit(0);
        }
    }
    return count;
}

// Finds the index of the next unset (false) bit in a bitmap, starting from a given index
// Optimized function for short ranges which are common
static inline counter_t __attribute__((always_inline, hot, nonnull, const)) 
function(searchBitFalse,suffix)(void* restrict bitstorage, register counter_t index) 
{
    startAnalysis8(time_searchBitFalse, "searchBitFalse from prime %ju (step %ju)", (uintmax_t)index, (uintmax_t)index*2+1);

    #pragma GCC ivdep
    #pragma GCC unroll 4
    for (;checkBitTrue(bitstorage, ++index);)

    endAnalysis8(time_searchBitFalse, " next prime %ju (step %ju)\n", (uintmax_t) index, (uintmax_t)index*2+1);
    return index;
}


// Finds the index of the next unset (false) bit in a bitmap, starting from a given index
// Optimized function for large ranges which are not common
static inline counter_t __attribute__((always_inline, hot, nonnull, const)) 
function(searchBitFalse_largestep,suffix)(const void* restrict bitstorage, register counter_t index) 
{
    startAnalysis8(time_searchBitFalse_largestep, "searchBitFalse_largestep from prime %ju (step %ju)", (uintmax_t)index, (uintmax_t)index*2+1);

    bitbucket_t* restrict bitstorage_sized = __builtin_assume_aligned(bitstorage, cache_line_bytes);

    // Move to the next position after the starting index
    ++index;
    
    // Get the current word and bit position
    register const bitshift_t bit_index_in_word = bitindex_calc_type(index, bitbucket_t);
    register counter_t word_index = index_type(index, bitbucket_t);
    register bitbucket_t current_word = bitstorage_sized[word_index];

    if likely(bit_index_in_word) {
        current_word >>= bit_index_in_word ;
        current_word |= (bitstorage_sized[word_index+1] << (bitcount_type(bitbucket_t) - bit_index_in_word));

        if (current_word == safe_fill_type(bitbucket_t)) {
            current_word = bitstorage_sized[++word_index];
            index += (bitcount_type(bitbucket_t) - bit_index_in_word);
        }
    }

    while (current_word == safe_fill_type(bitbucket_t)) {
        current_word = bitstorage_sized[++word_index];
        index += bitcount_type(bitbucket_t);
    }

    endAnalysis8(time_searchBitFalse_largestep, " next prime %ju (step %ju)\n", (uintmax_t) (index + builtin_ctz(~current_word)), (uintmax_t)(index + builtin_ctz(~current_word))*2+1);

    // Note: ~current_word inverts the bits so we find first 0 instead of 1
    return index + builtin_ctz(~current_word);
}

#include "../generic/cleansuffix.h"

// make explicit versions for all types with different suffixes for different bitbucket_t sizes
#ifndef BITSTORAGE_SEARCH_INCLUDE_GUARD
    #define BITSTORAGE_SEARCH_INCLUDE_GUARD
    #define variant uint8
    #include "bitstorage_search.h"
    #define variant uint16
    #include "bitstorage_search.h"
    #define variant uint32
    #include "bitstorage_search.h"
    #define variant uint64
    #include "bitstorage_search.h"
#endif

