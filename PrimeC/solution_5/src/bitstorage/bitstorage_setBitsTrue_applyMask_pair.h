#include "../generic/setsuffix.h"

static inline void __attribute__((always_inline, hot, aligned(cache_line_bytes)))
function(applyMask_pair,suffix)(void* restrict bitstorage, const counter_t step, const counter_t range_stop, const bitbucket_t mask1, const bitbucket_t mask2, const counter_t index_vector) 
{
    startAnalysis8(time_applyMask_pair, "\nApplying %s mask in pairs with step %ju in range until %ju", STR(bitbucket_t), (uintmax_t)step, (uintmax_t)range_stop);

    register const counter_t step_max = step * unrolls, step_2 = step * 2, step_3 = step_2 + step;
    register const bitbucket_t* restrict bitstorage_sized = __builtin_assume_aligned(bitstorage, cache_line_bytes);
    register const bitbucket_t* restrict fast_loop_ptr    = __builtin_assume_aligned(&bitstorage_sized[safe_diff(index_type(range_stop, bitbucket_t),step_max)],sizeof(bitbucket_t));
    register const bitbucket_t* restrict range_stop_ptr   = __builtin_assume_aligned(&bitstorage_sized[index_type(range_stop, bitbucket_t)],sizeof(bitbucket_t));
    register bitbucket_t* restrict index_ptr              = __builtin_assume_aligned(&bitstorage_sized[index_vector],sizeof(bitbucket_t));
    
    #if unrolls == 4
        #pragma GCC ivdep
        #pragma GCC unroll 4
        for(;likely(index_ptr < fast_loop_ptr);) {
            *index_ptr                |= mask1; 
            *(index_ptr + step      ) |= mask1; 
            *(index_ptr + step_2)     |= mask1; 
            *(index_ptr + step_3)     |= mask1; 
            *(index_ptr + 1         ) |= mask2; 
            *(index_ptr + step + 1  ) |= mask2; 
            *(index_ptr + step_2 + 1) |= mask2; 
            *(index_ptr + step_3 + 1) |= mask2; 
            index_ptr += step_max;
        }
    #elif unrolls == 8
        #pragma GCC ivdep
        #pragma GCC unroll 8
        while likely(index_ptr < fast_loop_ptr) {
            *index_ptr                  |= mask1;
            *(index_ptr + 1           ) |= mask2;  
            *(index_ptr + step        ) |= mask1; 
            *(index_ptr + step + 1    ) |= mask2;  
            *(index_ptr + step_2      ) |= mask1; 
            *(index_ptr + step_2 + 1  ) |= mask2;  
            *(index_ptr + step_3      ) |= mask1; 
            *(index_ptr + step_3 + 1  ) |= mask2;  
            *(index_ptr + step * 4    ) |= mask1;
            *(index_ptr + step * 4 + 1) |= mask2;
            *(index_ptr + step * 5    ) |= mask1;
            *(index_ptr + step * 5 + 1) |= mask2;
            *(index_ptr + step * 6    ) |= mask1;
            *(index_ptr + step * 6 + 1) |= mask2;
            *(index_ptr + step * 7    ) |= mask1;
            *(index_ptr + step * 7 + 1) |= mask2;
            index_ptr += step_max;
        }
    #endif 
        
    for (counter_t i=(unrolls+1); i-- && likely(index_ptr < range_stop_ptr); index_ptr += step) { // signal compiler that only <unrolls iterations are left
        *index_ptr     |= mask1; 
        *(index_ptr+1) |= mask2; 
    }
    
    if (index_ptr == range_stop_ptr) {
        *index_ptr     |= mask1; 
    }

    endAnalysis8(time_applyMask_pair);
}

#include "../generic/cleansuffix.h"
