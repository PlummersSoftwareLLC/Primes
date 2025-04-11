// assemble the word and vector functions
// these will make differt versions of themselves for different types of bitstorage
#include "bitstorage_setBitsTrue_assemble_word.h" 
#include "bitstorage_setBitsTrue_assemble_vector.h" 

// Function to dispatch the correct setBitsTrue function based on the step size and occurrences
static inline void  
setBitsTrue_v128(void* restrict bitstorage, const counter_t range_start, const counter_t step, const counter_t range_stop) 
{
    if      (step  <  16)  setBitsTrue_smallstep_rotate_pair_uint16v8(bitstorage, range_start, step, range_stop);
    else if (step  <  32)  setBitsTrue_smallstep_rotate_pair_uint32v4(bitstorage, range_start, step, range_stop);
    else if (step  <  64)  setBitsTrue_smallstep_rotate_pair_uint64v2(bitstorage, range_start, step, range_stop); 
    else if (step  < 128 && step < global_largestep_faster)  setBitsTrue_largestep_vector_uint64v2(bitstorage, range_start, step, range_stop); 
    else if (step  < 256 && step < global_largestep_faster)  setBitsTrue_largestep_vector_uint64v4(bitstorage, range_start, step, range_stop); 
    else {
        const counter_t range = range_stop - range_start, ratio = range / step;
        if      (ratio > 512) { setBitsTrue_largestep_repeat_uint8_unroll8 (bitstorage, range_start, step, range_stop); } 
        else if (ratio >  32) { setBitsTrue_largestep_repeat_uint8         (bitstorage, range_start, step, range_stop); } 
        else                    setBitsTrue_largestep_norepeat_uint8       (bitstorage, range_start, step, range_stop);
    }
}

// Function to dispatch the correct setBitsTrue function based on the step size and occurrences
static inline void  
setBitsTrue_v256(void* restrict bitstorage, const counter_t range_start, const counter_t step, const counter_t range_stop) 
{

    if      (step  <  16)  setBitsTrue_smallstep_rotate_pair_uint16v16(bitstorage, range_start, step, range_stop);
    else if (step  <  32)  setBitsTrue_smallstep_rotate_pair_uint32v8 (bitstorage, range_start, step, range_stop);
    else if (step  <  64)  setBitsTrue_smallstep_rotate_pair_uint64v4 (bitstorage, range_start, step, range_stop); 
    else if (step  < 128 && step < global_largestep_faster)  setBitsTrue_largestep_vector_uint64v4(bitstorage, range_start, step, range_stop); 
    else if (step  < 512 && step < global_largestep_faster)  setBitsTrue_largestep_vector_uint64v8(bitstorage, range_start, step, range_stop); 
    else {
        const counter_t range = range_stop - range_start, ratio = range / step;
        if      (ratio > 512) { setBitsTrue_largestep_repeat_uint8_unroll8 (bitstorage, range_start, step, range_stop); } 
        else if (ratio >  32) { setBitsTrue_largestep_repeat_uint8         (bitstorage, range_start, step, range_stop); } 
        else                    setBitsTrue_largestep_norepeat_uint8       (bitstorage, range_start, step, range_stop);
    }
}

// Function to dispatch the correct setBitsTrue function based on the step size and occurrences
static inline void  
setBitsTrue_v512(void* restrict bitstorage, const counter_t range_start, const counter_t step, const counter_t range_stop) 
{

    if      (step  <  16)  setBitsTrue_smallstep_rotate_pair_uint16v32(bitstorage, range_start, step, range_stop);
    else if (step  <  32)  setBitsTrue_smallstep_rotate_pair_uint32v16(bitstorage, range_start, step, range_stop);
    else if (step  <  64)  setBitsTrue_smallstep_rotate_pair_uint64v8 (bitstorage, range_start, step, range_stop); 
    else if (step  < 512 && step < global_largestep_faster)  setBitsTrue_largestep_vector_uint64v8(bitstorage, range_start, step, range_stop); 
    else {
        const counter_t range = range_stop - range_start, ratio = range / step;
        if      (ratio > 512) { setBitsTrue_largestep_repeat_uint8_unroll8 (bitstorage, range_start, step, range_stop); } 
        else if (ratio >  32) { setBitsTrue_largestep_repeat_uint8         (bitstorage, range_start, step, range_stop); } 
        else                    setBitsTrue_largestep_norepeat_uint8       (bitstorage, range_start, step, range_stop);
    }
}

static inline void  __attribute__((always_inline, nonnull, aligned(cache_line_bytes))) 
setBitsTrue(void* restrict bitstorage, const counter_t range_start, const counter_t step, const counter_t range_stop) 
{
    switch(global_vectorsize) {
        case 128: setBitsTrue_v128 (bitstorage, range_start, step, range_stop); break;
        case 256: setBitsTrue_v256 (bitstorage, range_start, step, range_stop); break;
        case 512: setBitsTrue_v512 (bitstorage, range_start, step, range_stop); break;
    }
}
