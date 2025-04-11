#include "../generic/setsuffix.h"
static inline void __attribute__((always_inline, nonnull, aligned(cache_line_bytes))) 
function(create_mask_smallstep_rotate_pair,suffix)(void* restrict bitstorage, const counter_t range_start, const counter_t step, const counter_t range_stop)
{
    register bitbucket_t* restrict bitstorage_vector = __builtin_assume_aligned(bitstorage, cache_line_bytes);
    __builtin_prefetch(&bitstorage_vector[index_type(range_start, bitbucket_t)], 1, 3); // prefetch the memory that will be written soon while creating mask

    // build the wordsize pattern, pattern_size en pattern_wordshift efficiently
    register const bitshift_t step_shift = bitindex_calc_type(step, variant_base_type_t); // to enable the compiler to optimize the shift
    register bitshift_t pattern_size = step_shift;
    register variant_base_type_t pattern = (variant_base_type_t) 1U;
    for (;pattern_size < bitcount_type(variant_base_type_t); pattern_size += step_shift) { pattern |= markmask_type(pattern_size, variant_base_type_t);  }
    const bitshift_t pattern_wordshift = pattern_size - bitcount_type(variant_base_type_t);

    // prepare the vectorsized shifts and mask
    register const variant_base_type_t pattern_vectorshift = (variant_base_type_t) (((pattern_size - bitcount_type(variant_base_type_t)) * (bitshift_t)BITBUCKET_ELEMENTS) % step_shift) & mask_type(variant_base_type_t);
    register bitbucket_t pattern_vectorshift_vector = BITBUCKET_BASE(pattern_vectorshift);
    register bitbucket_t step_shift_vector = BITBUCKET_BASE(step_shift);
    const bitshift_t shift = bitindex_calc_type(range_start, variant_base_type_t); 
    bitbucket_t mask_vector = BITBUCKET_BASE(pattern) << (BITBUCKET_BASE(shift) + (BITBUCKET_BASE(pattern_wordshift) * BITBUCKET_BYTEINDEX)) % BITBUCKET_BASE(step);

    // precaulcate the unique range_stop
    const counter_t range_stop_unique_vector = min(range_start + step * bitcount_type(bitbucket_t), range_stop);
    register const counter_t vector_max = index_type(range_stop_unique_vector, bitbucket_t);
    counter_t current_vector = index_type(range_start, bitbucket_t);

    // Apply this vectormask standalone until we align on the cache line
    for (;current_vector&1; current_vector++) {
        function(applyMask,suffix)(bitstorage_vector, step, range_stop, mask_vector, current_vector);
        mask_vector = (mask_vector << pattern_vectorshift_vector) | (mask_vector >> (step_shift_vector - pattern_vectorshift_vector)); 
    }

    // Process vectormasks in pairs from the cacheline
    for (; current_vector < vector_max; current_vector += 2) {
        __builtin_prefetch(&bitstorage_vector[current_vector+step], 1, 3); // prefetch the memory that will be written soon while creating mask
        bitbucket_t mask_vector2 = (mask_vector << pattern_vectorshift_vector) | (mask_vector >> (step_shift_vector - pattern_vectorshift_vector)); 
        function(applyMask_pair,suffix)(bitstorage_vector, step, range_stop, mask_vector, mask_vector2, current_vector);
        mask_vector = (mask_vector2 << pattern_vectorshift_vector) | (mask_vector2 >> (step_shift_vector - pattern_vectorshift_vector)); 
    }

    // Process the last vectormask if needed
    function(applyMask,suffix)(bitstorage_vector, step, range_stop, mask_vector, current_vector);
}

// static inline void __attribute__((always_inline, nonnull)) 
static void __attribute__((nonnull, aligned(cache_line_bytes))) 
function(setBitsTrue_smallstep_rotate_pair,suffix)(void* restrict bitstorage, const counter_t range_start, const counter_t step, const counter_t range_stop) 
{
    startAnalysis6(time_setBitsTrue_smallstep_rotate_pair, "Setting bits step %3ju using smallstep%-10s in %ju bit range (%ju-%ju) with %ju bits to set; using %ju copies of %ju bit mask", (uintmax_t)step, STR(suffix), (uintmax_t)safe_diff(range_stop,range_start),(uintmax_t)range_start,(uintmax_t)range_stop, (uintmax_t)((safe_diff(range_stop,range_start))/(uintmax_t)step), (uintmax_t)(((uintmax_t)safe_diff(range_stop,range_start))/(uintmax_t)(bitcount_type(bitbucket_t)*step)), (uintmax_t)bitcount_type(bitbucket_t));

    const counter_t range_start_nexttvector = vectorstart_type(range_start, bitbucket_t) + bitcount_type(bitbucket_t); // find next vector
    if (range_start_nexttvector + 4 * bitcount_type(bitbucket_t) > range_stop) {
        setBitsTrue_range(bitstorage, range_start, step, range_stop);
        timer_laptime(time_setBitsTrue_smallstep_rotate_pair); verbose6( printf("\n"); )
        return;
    }

    const counter_t range_start_new = setBitsTrue_range_return(bitstorage, range_start, step, range_start_nexttvector);
    function(create_mask_smallstep_rotate_pair,suffix)(bitstorage, range_start_new, step, range_stop);

    endAnalysis6(time_setBitsTrue_smallstep_rotate_pair,"\n");
}

#include "../generic/cleansuffix.h"