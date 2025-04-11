#include "../generic/setsuffix.h"

static inline void __attribute__((always_inline, hot, nonnull, aligned(cache_line_bytes))) 
function(applyMask,suffix)(void* restrict bitstorage, const counter_t step, const counter_t range_stop, const bitbucket_t mask, const counter_t index_vector) 
{
    startAnalysis8(time_applyMask, "\nApplying %s mask with step %ju in range until %ju", STR(bitbucket_t), (uintmax_t)step, (uintmax_t)range_stop);
  
    register const counter_t step_max                   = step * unrolls;
    register bitbucket_t* restrict bitstorage_sized     = __builtin_assume_aligned(bitstorage, cache_line_bytes);
    register bitbucket_t* restrict index_ptr            = __builtin_assume_aligned(&bitstorage_sized[index_vector],sizeof(bitbucket_t));
    register const bitbucket_t* restrict fast_loop_ptr  = __builtin_assume_aligned(&bitstorage_sized[safe_diff(index_type(range_stop, bitbucket_t),step_max)],sizeof(bitbucket_t));

    #if defined(__GNUC__) && !defined(__clang__) // optimized for GCC
        #if unrolls == 4
            #pragma GCC ivdep
            #pragma GCC unroll 4
            for(;likely(index_ptr < fast_loop_ptr);) {
                *index_ptr |= mask;  index_ptr += step;
                *index_ptr |= mask;  index_ptr += step;
                *index_ptr |= mask;  index_ptr += step;
                *index_ptr |= mask;  index_ptr += step;
            }
        #elif unrolls == 8
            #pragma GCC ivdep
            #pragma GCC unroll 8
            for(;likely(index_ptr < fast_loop_ptr);) {
                *index_ptr |= mask;  index_ptr += step;
                *index_ptr |= mask;  index_ptr += step;
                *index_ptr |= mask;  index_ptr += step;
                *index_ptr |= mask;  index_ptr += step;
                *index_ptr |= mask;  index_ptr += step;
                *index_ptr |= mask;  index_ptr += step;
                *index_ptr |= mask;  index_ptr += step;
                *index_ptr |= mask;  index_ptr += step;
            }
        #endif
    #else // optimized for clang
        for(const counter_t step_2 = step * 2, step_3 = step_2 + step; likely(index_ptr < fast_loop_ptr); index_ptr += step_max) {
            *index_ptr            |= mask; 
            *(index_ptr + step  ) |= mask; 
            *(index_ptr + step_2) |= mask; 
            *(index_ptr + step_3) |= mask;
            #if unrolls > 4
            *(index_ptr + step * 4) |= mask;
            *(index_ptr + step * 5) |= mask;
            *(index_ptr + step * 6) |= mask;
            *(index_ptr + step * 7) |= mask;
            #endif 
        }
    #endif
    
    register const bitbucket_t* restrict range_stop_ptr = __builtin_assume_aligned(&bitstorage_sized[index_type(range_stop, bitbucket_t)],sizeof(bitbucket_t));
    
    for (counter_t i=(unrolls+1); i-- && likely(index_ptr <= range_stop_ptr); index_ptr += step) { // signal compiler that only <4 iterations are left
        *index_ptr |= mask; 
    }

    endAnalysis8(time_applyMask);
}

#include "../generic/cleansuffix.h"
