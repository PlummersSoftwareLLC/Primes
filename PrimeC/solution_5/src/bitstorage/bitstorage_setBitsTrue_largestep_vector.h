
// Largestep (> WORD_SIZE and < VECTOR_SIZE) means the same vectormask can be reused
#include "../generic/setsuffix.h"
static inline void __attribute__((always_inline, aligned(cache_line_bytes))) 
function(create_mask_vector_largestep,suffix)(void* restrict bitstorage, const counter_t range_start, const counter_t step, const counter_t range_stop)
{
    bitbucket_t* restrict bitstorage_vector = __builtin_assume_aligned(bitstorage, cache_line_bytes);
    const counter_t range_stop_unique_vector = range_start + step * bitcount_type(bitbucket_t) + bitcount_type(bitbucket_t);  // extra size is sometime needed when size < blocklimit

    #pragma GCC ivdep
    for (counter_t index = range_start, current_vector = index_type(range_start, bitbucket_t); index <= range_stop_unique_vector; current_vector++) {
        const counter_t current_vector_start = vectorstart_type(index, bitbucket_t);
        bitbucket_t mask_vector = BITBUCKET_BASE((variant_base_type_t) 0U);

        #pragma GCC ivdep
        for (counter_t element = 0; element < BITBUCKET_ELEMENTS; element++) {
            if (vectorstart_type(index,variant_base_type_t) == (current_vector_start + (bitcount_type(variant_base_type_t) * element))) {
                mask_vector[element] = markmask_calc_type(index, variant_base_type_t); // in clang, markmask_type is enough, not in gcc
                index += step;
            }
        }
        function(applyMask,suffix)(bitstorage_vector, step, range_stop, mask_vector, current_vector);
    }
}

#include "../generic/setsuffix.h"
static inline void __attribute__((always_inline, nonnull,  aligned(cache_line_bytes))) 
function(setBitsTrue_largestep_vector,suffix)(void* restrict bitstorage, const counter_t range_start, const counter_t step, const counter_t range_stop) 
{
    startAnalysis6(time_setBitsTrue_largestep_vector, "Setting bits step %3ju using largestep_vector%s in %ju bit range (%ju-%ju) (%ju occurances; %ju stamps)", (uintmax_t)step, STR(suffix), (uintmax_t)safe_diff(range_stop,range_start),(uintmax_t)range_start,(uintmax_t)range_stop, (uintmax_t)((safe_diff(range_stop,range_start))/(uintmax_t)step), (uintmax_t)(((uintmax_t)safe_diff(range_stop,range_start))/(uintmax_t)(VECTOR_SIZE_BITS*step)));

    const counter_t range_start_nexttvector = index_next_type(range_start, bitbucket_t); // find next vector
    const counter_t range_start_new = setBitsTrue_range_return(bitstorage, range_start, step, range_start_nexttvector);
    if (range_start_new > range_stop) return;
    function(create_mask_vector_largestep,suffix)(bitstorage, range_start_new, step, range_stop);
    
    endAnalysis6(time_setBitsTrue_largestep_vector,"\n");
}

#include "../generic/cleansuffix.h"


