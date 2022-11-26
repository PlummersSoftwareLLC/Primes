// Sieve algorithm by Rogier van Dam - 2022
// Find all primes up to <max int> using the Sieve of Eratosthenes (https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes)

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <string.h>
#ifdef _OPENMP
#include <omp.h>
#endif

// defaults
#define default_sieve_limit             1000000
#define default_blocksize               (32*1024*8)
#define default_maxTime                 5
#define default_sample_duration         0.0002
#define default_explain_level           0
#define default_verbose_level           0
#define default_tune_level              1
#define default_check_level             0
#define default_show_primes_on_error    100
#define default_showMaxFactor           0
#define anticiped_cache_line_bytesize   128

//set compile_debuggable to 1 to enable explain plan
#define compile_debuggable (0 || default_explain_level)
#if compile_debuggable
#define debug if (compile_debuggable && option.explain)
#else
#define debug if unlikely(0)
#endif

// set this to the level of verbose messages that will be compiled
#define compile_verboselevel 4 
#define verbose(level)    if (level < compile_verboselevel) if (option.verboselevel >= level) 
#define verbose_at(level) if (level < compile_verboselevel) if (option.verboselevel == level)

// helper functions
#define pow(base,pow)       (pow*((base>>pow)&1U))
#define min(a,b)            ((a<b) ? a : b)
#define uintsafeminus(a,b)  ((a>b)?(a-b):0)
#define likely(x)           (__builtin_expect((x),1))
#define unlikely(x)         (__builtin_expect((x),0))
#define xstr(s) str(s)
#define str(s) #s
#define PPCAT_NX(A, B) A ## B
#define PPCAT(A, B) PPCAT_NX(A, B)
#define is_signed(type) (((type)-1)<0)

// quicksets from commandline
#if defined u16_v2
#define bitword_t       uint16_t
#define VECTOR_ELEMENTS 2
#define VECTOR_SETTING  u16_v2
#define SAFE_SHIFT 1 // - set to 1 for 32<= bit (needs mask on shift)
#elif defined u32_v2
#define bitword_t       uint32_t
#define VECTOR_ELEMENTS 2
#define VECTOR_SETTING  u32_v2
#elif defined u64_v2
#define bitword_t       uint64_t
#define VECTOR_ELEMENTS 2
#define VECTOR_SETTING  u64_v2
#define WORD_SIZE_64    64
#elif defined u8_v4
#define bitword_t       uint8_t
#define VECTOR_ELEMENTS 4
#define VECTOR_SETTING  u8_v4
#define SAFE_SHIFT 1 // - set to 1 for 32<= bit (needs mask on shift)
#elif defined u16_v4
#define bitword_t       uint16_t
#define VECTOR_ELEMENTS 4
#define VECTOR_SETTING  u16_v4
#define SAFE_SHIFT 1 // - set to 1 for 32<= bit (needs mask on shift)
#elif defined u32_v4
#define bitword_t       uint32_t
#define VECTOR_ELEMENTS 4
#define VECTOR_SETTING  u32_v4
#elif defined u64_v4
#define bitword_t       uint64_t
#define VECTOR_ELEMENTS 4
#define VECTOR_SETTING  u64_v4
#define WORD_SIZE_64    64
#elif defined u8_v8
#define bitword_t       uint8_t
#define VECTOR_ELEMENTS 8
#define VECTOR_SETTING  u8_v8
#define SAFE_SHIFT 1 // - set to 1 for 32<= bit (needs mask on shift)
#elif defined u16_v8
#define bitword_t       uint16_t
#define VECTOR_ELEMENTS 8
#define VECTOR_SETTING  u16_v8
#define SAFE_SHIFT 1 // - set to 1 for 32<= bit (needs mask on shift)
#elif defined u32_v8
#define bitword_t       uint32_t
#define VECTOR_ELEMENTS 8
#define VECTOR_SETTING  u32_v8
#elif defined u64_v8
#define bitword_t       uint64_t
#define VECTOR_ELEMENTS 8
#define VECTOR_SETTING  u64_v8
#define WORD_SIZE_64    64
#endif
#ifndef VECTOR_SETTING
#define VECTOR_SETTING PPCAT(bitword_t,VECTOR_ELEMENTS)
#endif

// types
#ifndef bitword_t
#define bitword_t  uint64_t // type used to store bits
#endif

// follow main bitword setting in vectors. Change is otherwise needed
#define bitword_vector_t bitword_t
#ifdef WORD_SIZE_64
#define VECTORWORDSIZE_64 64 // enable this is wordsize is 64 - will allow further optimizations
#endif

#ifndef VECTOR_ELEMENTS
#define VECTOR_ELEMENTS 4
#endif

#define bitshift_t uint64_t // type used to shift bits
#define counter_t  uint64_t // type used to count loops, etc


// masks and mask helpers
#define SHIFT_BYTE          3
#define WORD_SIZE           (sizeof(bitword_t)*8)
#define WORD_SIZE_counter   ((counter_t)WORD_SIZE)
#define WORD_SIZE_bitshift  ((bitshift_t)WORD_SIZE)
#define SHIFT_WORD          ((bitshift_t)(pow(WORD_SIZE,1)+pow(WORD_SIZE,2)+pow(WORD_SIZE,3)+pow(WORD_SIZE,4)+pow(WORD_SIZE,5)+pow(WORD_SIZE,6)+pow(WORD_SIZE,7)+pow(WORD_SIZE,8)+pow(WORD_SIZE,9)+pow(WORD_SIZE,10)))

#define VECTORWORD_SIZE           (sizeof(bitword_vector_t)*8)
#define VECTORWORD_SIZE_counter   ((counter_t)VECTORWORD_SIZE)
#define VECTORWORD_SIZE_bitshift  ((bitshift_t)VECTORWORD_SIZE)
#define SHIFT_VECTORWORD          ((bitshift_t)(pow(VECTORWORD_SIZE,1)+pow(VECTORWORD_SIZE,2)+pow(VECTORWORD_SIZE,3)+pow(VECTORWORD_SIZE,4)+pow(VECTORWORD_SIZE,5)+pow(VECTORWORD_SIZE,6)+pow(VECTORWORD_SIZE,7)+pow(VECTORWORD_SIZE,8)+pow(VECTORWORD_SIZE,9)+pow(VECTORWORD_SIZE,10)))

#define VECTOR_SIZE_bytes   (sizeof(bitword_vector_t)*VECTOR_ELEMENTS)
#define VECTOR_SIZE_counter ((counter_t)VECTOR_SIZE_bytes*8)
#define VECTOR_SIZE         (VECTOR_SIZE_bytes*8)
#define SHIFT_VECTOR        ((bitshift_t)(pow(VECTOR_SIZE,1)+pow(VECTOR_SIZE,2)+pow(VECTOR_SIZE,3)+pow(VECTOR_SIZE,4)+pow(VECTOR_SIZE,5)+pow(VECTOR_SIZE,6)+pow(VECTOR_SIZE,7)+pow(VECTOR_SIZE,8)+pow(VECTOR_SIZE,9)+pow(VECTOR_SIZE,10)))

// types (II) - calculated
typedef bitword_vector_t bitvector_t __attribute__ ((vector_size(VECTOR_SIZE_bytes))); 

// globals for tuning
// #define BLOCKSTEP_FASTER ((counter_t)0)
// #define MEDIUMSTEP_FASTER ((counter_t)16)
//  #define VECTORSTEP_FASTER ((counter_t)0)
static counter_t global_BLOCKSTEP_FASTER  =   0ULL; // if step > BLOCKSTEP use blocks, else use the whole sieve
static counter_t global_MEDIUMSTEP_FASTER =  16ULL; // if step < MEDIUMSTEP_FASTER, use medium steps
static counter_t global_VECTORSTEP_FASTER = 128ULL; // if step < VECTORSTAP_FASTER, use large steps
static counter_t global_BLOCKSIZE_BITS = default_blocksize;
#define BLOCKSTEP_FASTER     ((counter_t)global_BLOCKSTEP_FASTER)
#define MEDIUMSTEP_FASTER    ((counter_t)global_MEDIUMSTEP_FASTER)
#define VECTORSTEP_FASTER    ((counter_t)global_VECTORSTEP_FASTER)
#define BLOCKSIZE_BITS       ((counter_t)global_BLOCKSIZE_BITS)

// Patterns based on types
#define SAFE_SHIFTBIT        (bitshift_t)1ULL
#define SAFE_ZERO            (bitword_t)0ULL
#define SAFE_FILL            (bitword_t)~0ULL
#define BITWORD_SHIFTBIT     (bitword_t)1ULL
#define BITVECTORWORD_SHIFTBIT (bitword_vector_t)1ULL
#define WORDMASK             ((((counter_t)1)<<SHIFT_WORD)-(counter_t)1)
#define VECTORWORDMASK       ((((counter_t)1)<<SHIFT_VECTORWORD)-(counter_t)1)
#define VECTORMASK           ((((counter_t)1)<<SHIFT_VECTOR)-(counter_t)1)

// helpder functions for word/vector indexing
#define wordindex(index)     (((counter_t)index) >> SHIFT_WORD)
#define wordend(index)       ((counter_t)(index) |  WORDMASK)
#define wordstart(index)     ((counter_t)(index) &  (counter_t)(~WORDMASK))
#define vectorindex(index)   (((counter_t)index) >> (bitshift_t)SHIFT_VECTOR)
#define vectorstart(index)   (((counter_t)index) &  (counter_t)~VECTORMASK)
#define vectorend(index)     (((counter_t)index) |  (counter_t)VECTORMASK)
#define vector_wordstart(index)     ((counter_t)(index) & (counter_t)(~VECTORWORDMASK))
#define vector_wordindex(index)     (((counter_t)index) >> SHIFT_VECTORWORD)
// #define vectorfromword(word) ((counter_t)(word ) >> (counter_t)SHIFT_VECTOR-SHIFT_WORD))
// #define wordinvector(index)  (((counter_t)index >> SHIFT_WORD) & (VECTORMASK >> SHIFT_WORD))

// modern processors do a & over the shiftssize, so we only have to do that ourselve when using the shiftsize in calculations. 
#define bitindex_calc(index)        ((bitshift_t)(index)&((bitshift_t)(WORDMASK      )))
#define vector_bitindex(index)      ((bitshift_t)(index))
#define vector_bitindex_calc(index) ((bitshift_t)(index)&((bitshift_t)(VECTORWORDMASK)))

#if SAFE_SHIFT == 1
#define bitindex(index)      ((bitshift_t)(index)&((bitshift_t)(WORDMASK)))
#else
#define bitindex(index)      ((bitshift_t)(index))
#endif

#define markmask(index)        (BITWORD_SHIFTBIT << bitindex(index))
#define markmask_calc(index)   (BITWORD_SHIFTBIT << bitindex_calc(index))
#ifdef VECTORWORDSIZE_64 // mask will be applied automatically
#define vector_markmask(index) (BITWORD_SHIFTBIT << vector_bitindex(index))
#else
#define vector_markmask(index) (BITWORD_SHIFTBIT << vector_bitindex_calc(index))
#endif
#define chopmask(index)        (SAFE_FILL >> (WORD_SIZE_bitshift-SAFE_SHIFTBIT-bitindex_calc(index)))
#define keepmask(index)        (SAFE_FILL << (bitindex(index)))

// timing
static struct timespec timer_lap, timer_elapsed;
#define timerStart() clock_gettime(CLOCK_PROCESS_CPUTIME_ID ,&timer_start)
#define timerLapStart() clock_gettime(CLOCK_PROCESS_CPUTIME_ID ,&timer_lap)
void timerLapTime() {
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID ,&timer_elapsed);
    long seconds = timer_elapsed.tv_sec - timer_lap.tv_sec;
    long nanoseconds = timer_elapsed.tv_nsec - timer_lap.tv_nsec;
    double elapsed_time = seconds*1e-9 + nanoseconds;
    // double elapsed_time = timer_elapsed.tv_sec + timer_elapsed.tv_nsec*1e-9 - timer_lap.tv_sec - timer_lap.tv_nsec*1e-9;
    if      (elapsed_time > 2000) printf("...time: \033[0;31m%.0f\033[0m ns\n", elapsed_time);
    else if (elapsed_time > 1000) printf("...time: \033[0;35m%.0f\033[0m ns\n", elapsed_time);
    else if (elapsed_time > 100)  printf("...time: \033[0;36m%.0f\033[0m ns\n", elapsed_time);
    else                          printf("...time: %.0f ns\n", elapsed_time);
}

struct sieve_t {
    bitword_t* bitstorage;
    counter_t  bits;
    counter_t  size;
};

static struct options_t {
    double    maxTime;
    counter_t maxFactor;
    counter_t blocksize_kB;
    counter_t showMaxFactor;
    int       verboselevel;
    int       explain;
    int       check;
    int       tunelevel;
    int       threads;
} option;

static inline void printWord(bitword_t bitword)
{
    char row[WORD_SIZE*2] = {};
    int col=0;
    for (int i=WORD_SIZE-1; i>=0; i--) {
		row[col++] = (bitword & (BITWORD_SHIFTBIT<<i))?'1':'.';
		if (!(i%8)) row[col++] = ' ';
    }

    printf("%s", row);
}
// use cache lines as much as possible - alignment might be key
// moved clearing the sieve with 0 to the sieve_block_extend - it gave weird malloc problems at this point
// switched to one malloc for the sieve, instead of one for the sieve and one for the storage
// bitstorage will be aligned on the anticiped_cache_line_bytesize
static inline struct sieve_t * __attribute__((always_inline)) sieve_create(counter_t size) 
{
    struct sieve_t *sieve = malloc(((sizeof(struct sieve_t) + (size_t)(size>>1))|(anticiped_cache_line_bytesize-1))+1+anticiped_cache_line_bytesize);
    sieve->bitstorage     = __builtin_assume_aligned((void *) (( (uintptr_t) (sieve + sizeof(struct sieve_t))|(anticiped_cache_line_bytesize-1))+1),anticiped_cache_line_bytesize);
    sieve->bits           = size >> 1;
    sieve->size           = size;

    // code below not needed: only clearing the first word of each block will do the trick
    // for (counter_t index_word = 0; index_word <= wordindex(sieve->bits); index_word++) sieve->bitstorage[index_word] = SAFE_ZERO;
    return sieve;
}

static inline void __attribute__((always_inline)) sieve_delete(struct sieve_t *sieve) 
{
    free(sieve);
}

static inline counter_t __attribute__((always_inline)) searchBitFalse(bitword_t* bitstorage, register counter_t index) 
{
    register const bitword_t bitword = bitstorage[wordindex(index)] >> bitindex(index);

    #ifdef WORD_SIZE_64
    register const counter_t length = (counter_t)__builtin_ffsll( (int64_t)~(bitword));
    #else
    register const counter_t length = (counter_t)__builtin_ffsl( (int32_t)~(bitword));
    #endif
    if (length) index += length-1;
    while (bitstorage[wordindex(index)] & markmask(index)) index++;
    return index;
}

// apply the same word mask at large ranges
// manually unlooped - this here is where the main speed increase comes from
// idea from PrimeRust/solution_1 by Michael Barber 
static inline void __attribute__((always_inline)) applyMask_word(bitword_t* __restrict bitstorage, const counter_t step, const counter_t range_stop, const bitword_t mask, const counter_t index_word) 
{
    register const counter_t step_2 = step << 1;
    register const counter_t step_3 = step_2 + step;
    register const counter_t step_4 = step << 2;

    register bitword_t* __restrict index_ptr = __builtin_assume_aligned(&bitstorage[index_word], sizeof(bitword_t));

    const counter_t range_stop_word = wordindex(range_stop);
    register const bitword_t* __restrict fast_loop_ptr  =  &bitstorage[((range_stop_word>step_4) ? (range_stop_word - step_4):0)];

    #pragma GCC ivdep
    while (index_ptr < fast_loop_ptr) {
        *index_ptr            |= mask; 
        *(index_ptr + step  ) |= mask; 
        *(index_ptr + step_2) |= mask; 
        *(index_ptr + step_3) |= mask; 
        index_ptr += step_4;
    }
    register const bitword_t* __restrict range_stop_ptr = &bitstorage[(range_stop_word)];

    for (counter_t i=4; i-- && likely(index_ptr < range_stop_ptr);  index_ptr += step) { // signal compiler that only <4 iterations are left
        *index_ptr |= mask; 
    }

    // doing this instead of index_ptr <= above is faster. unexplained. 
    if (index_ptr == range_stop_ptr) { // index_ptr could also end above range_stop_ptr, depending on steps. 
        *index_ptr |= mask; // chop not needed is block-size aligned with word size
    }
}

// same as word mask, but at a vector level - uses the sse/avx extensions, hopefully
static inline void __attribute__((always_inline)) applyMask_vector(bitvector_t* __restrict bitstorage, const counter_t step, const counter_t range_stop, const bitvector_t mask, counter_t index_vector) 
{
    const counter_t range_stop_vector = vectorindex(range_stop);
    register const counter_t step_4 = step << 2;
    register bitvector_t* __restrict index_ptr      =  __builtin_assume_aligned(&bitstorage[index_vector],sizeof(bitvector_t));
    #if is_signed(counter_t)
    register bitvector_t* __restrict fast_loop_ptr  =  __builtin_assume_aligned(&bitstorage[range_stop_vector] - step_4,sizeof(bitvector_t));
    #else
    register bitvector_t* __restrict fast_loop_ptr  =  __builtin_assume_aligned(&bitstorage[((range_stop_vector > step_4) ? (range_stop_vector - step_4):0)],sizeof(bitvector_t));
    #endif

    #pragma GCC ivdep
    while likely(index_ptr < fast_loop_ptr) {
        *index_ptr |= mask; index_ptr += step;
        *index_ptr |= mask; index_ptr += step;
        *index_ptr |= mask; index_ptr += step;
        *index_ptr |= mask; index_ptr += step;
    }
    
    register const bitvector_t* __restrict range_stop_ptr = &bitstorage[(range_stop_vector)];
    
    for (counter_t i=4; i-- && likely(index_ptr < range_stop_ptr);  index_ptr += step) { // signal compiler that only <4 iterations are left
        *index_ptr |= mask; 
    }

    // doing this instead of index_ptr <= above is faster. unexplained. 
    if (index_ptr == range_stop_ptr) {
        *index_ptr |= mask; 
    }
}

// Medium steps could be within the same word (e.g. less than 64 bits apart).
// By joining the masks and then writing to memory, we might save some time.
// This is especially true for small steps over long ranges
// but it needs tuning, because there is some overhead of checking if the next step is in the same word
static inline void  __attribute__((always_inline)) setBitsTrue_mediumStep(bitword_t* __restrict bitstorage, const counter_t range_start, const counter_t step, const counter_t range_stop) 
{
    debug timerLapStart();

    // fast exit for small ranges / large steps
    if (range_start + step > range_stop) {
        debug printf("Setting bits step %ju in %ju bit range (%ju-%ju) using mediumstep-nostep (%ju occurances)", (uintmax_t)step, (uintmax_t)range_stop-(uintmax_t)range_start,(uintmax_t)range_start,(uintmax_t)range_stop, (uintmax_t)(((uintmax_t)range_stop-(uintmax_t)range_start)/(uintmax_t)step));
        bitstorage[wordindex(range_start)] |= markmask(range_start);
        debug timerLapTime();
        return;
    }

    const counter_t range_stop_unique =  range_start + WORD_SIZE_counter * step;

    if unlikely(range_stop_unique > range_stop) { // the range will not repeat itself; no need to try to resuse the mask
        debug printf("Setting bits step %ju in %ju bit range (%ju-%ju) using mediumstep-unique (%ju occurances)", (uintmax_t)step, (uintmax_t)range_stop-(uintmax_t)range_start,(uintmax_t)range_start,(uintmax_t)range_stop, (uintmax_t)(((uintmax_t)range_stop-(uintmax_t)range_start)/(uintmax_t)step));
        for (register counter_t index = range_start; index <= range_stop;) {
            const counter_t index_word = wordindex(index);
            register bitword_t mask = SAFE_ZERO;
            for(counter_t index_word_start = wordstart(index); index_word_start == wordstart(index); index += step) mask |= markmask(index);
            bitstorage[index_word] |= mask;
        }
    }
    else { // this mask will reoccur at a interval of step words -> fill mask and reapply as a interval of step
        debug printf("Setting bits step %ju in %ju bit range (%ju-%ju) using mediumstep-repeat (%ju occurances)", (uintmax_t)step, (uintmax_t)range_stop-(uintmax_t)range_start,(uintmax_t)range_start,(uintmax_t)range_stop, (uintmax_t)(((uintmax_t)range_stop-(uintmax_t)range_start)/(uintmax_t)step));
        
        for (register counter_t index = range_start; index <= range_stop_unique;) {
            const counter_t index_word = wordindex(index);
            register bitword_t mask = SAFE_ZERO;
            for(counter_t index_word_start = wordstart(index); index_word_start == wordstart(index); index += step) mask |= markmask(index);
            applyMask_word(bitstorage, step, range_stop, mask, index_word);
        }
    }
    debug timerLapTime();
}

// Large ranges (> WORD_SIZE * step) mean the same mask can be reused
static inline void  __attribute__((always_inline)) setBitsTrue_largeRange(bitword_t* __restrict bitstorage, const counter_t range_start, const counter_t step, const counter_t range_stop) 
{
    debug timerLapStart();

    const counter_t range_stop_unique =  range_start + WORD_SIZE_counter * step;

    if likely(range_stop_unique <= range_stop) { // the range will not repeat itself; no need to try to resuse the mask
        debug printf("Setting bits step %ju in %ju bit range (%ju-%ju) using largerange-repeat (%ju occurances)", (uintmax_t)step, (uintmax_t)range_stop-(uintmax_t)range_start,(uintmax_t)range_start,(uintmax_t)range_stop, (uintmax_t)(((uintmax_t)range_stop-(uintmax_t)range_start)/(uintmax_t)step));
        debug timerLapStart();
        for (register counter_t index = range_start; index < range_stop_unique; index += step) {
            applyMask_word(bitstorage, step, range_stop, markmask(index), wordindex(index));
        }
    }
    else {
        debug printf("Setting bits step %ju in %ju bit range (%ju-%ju) using largerange-unique (%ju occurances)", (uintmax_t)step, (uintmax_t)range_stop-(uintmax_t)range_start,(uintmax_t)range_start,(uintmax_t)range_stop, (uintmax_t)(((uintmax_t)range_stop-(uintmax_t)range_start)/(uintmax_t)step));
        debug timerLapStart();

        const counter_t step_2 = step * 2;
        #if is_signed(counter_t)
        const counter_t loop_stop = range_stop - step_2;
        #else
        const counter_t loop_stop = (range_stop > step_2) ? range_stop - step_2 : 0;
        #endif
        register counter_t index = range_start;

        #pragma GCC ivdep
        for (; index < loop_stop; index += step_2) {
            bitstorage[wordindex(index         )] |= markmask(index);
            bitstorage[wordindex(index + step  )] |= markmask(index + step  );
        }

        for (counter_t i=4; i-- && index < range_stop; index += step) 
            bitstorage[wordindex(index)] |= markmask(index);

        if unlikely(index==range_stop)
            bitstorage[wordindex(index)] |= markmask(index);
    }
    debug timerLapTime();
}

static inline void  __attribute__((always_inline)) setBitsTrue_largeRange_vector(bitword_t* __restrict bitstorage, counter_t range_start, const counter_t step, const counter_t range_stop) 
{
    debug printf("Setting bits step %ju in %ju bit range (%ju-%ju) using largerange vector (%ju occurances; %ju stamps) ", (uintmax_t)step, (uintmax_t)range_stop-(uintmax_t)range_start,(uintmax_t)range_start,(uintmax_t)range_stop, (uintmax_t)(((uintmax_t)range_stop-(uintmax_t)range_start)/(uintmax_t)step), (uintmax_t)(((uintmax_t)range_stop-(uintmax_t)range_start)/(uintmax_t)(VECTOR_SIZE_counter*step)));
    debug timerLapStart();

    counter_t range_start_atvector = vectorstart(range_start);
    if likely(( range_start_atvector + step) < range_start) { // not the first step possible in this vector - would give incomplete copies
        debug printf("\n..Range start %ju not at start of vector %ju\n",(uintmax_t)range_start, (uintmax_t)range_start_atvector); 

        range_start_atvector += VECTOR_SIZE; // find next vector
        if (unlikely(range_start_atvector > range_stop)) { // we should not be here; just handle without vector
            // #pragma GCC ivdep
            for (counter_t index = range_start; index <= range_stop; index += step) 
                bitstorage[wordindex(index)] |= markmask(index);
            debug timerLapTime();
            return;
        }

        // #pragma GCC ivdep
        for (; range_start < range_start_atvector; range_start += step) 
            bitstorage[wordindex(range_start)] |= markmask(range_start);

        if unlikely(range_start==range_start_atvector)
            bitstorage[wordindex(range_start)] |= markmask(range_start);
    }
    
    const counter_t range_stop_unique =  range_start + VECTOR_SIZE_counter * step; 
    if (range_stop_unique > range_stop || step > VECTOR_SIZE_counter) { // fallback to other methods if vector is too large to repeat -> TODO: remove and fix in VECTORSIZE check
        if (step < WORD_SIZE_counter) setBitsTrue_mediumStep(bitstorage, range_start, step, range_stop);
        else setBitsTrue_largeRange(bitstorage, range_start, step, range_stop);
        debug timerLapTime();
        return;
    }

    debug printf("..building masks in range %ju-%ju with WORD_SIZE %ju", (uintmax_t)range_start, (uintmax_t)range_stop_unique, (uintmax_t)WORD_SIZE_counter);

    bitvector_t* __restrict bitstorage_vector = (bitvector_t*) __builtin_assume_aligned(bitstorage, anticiped_cache_line_bytesize);
    counter_t current_vector =  vectorindex(range_start);

    if (step < VECTORWORD_SIZE_counter) {
        const bitword_t pattern_base = BITVECTORWORD_SHIFTBIT;
        register bitword_t pattern   = BITVECTORWORD_SHIFTBIT;
        bitshift_t pattern_size = step;

        if (pattern_size < (VECTORWORD_SIZE_bitshift >> 2)) {
            pattern |= (pattern_base << step) | (pattern_base << step*2) | (pattern_base << step*3);
            pattern_size = step << 2;
        }
        for (; pattern_size <= VECTORWORD_SIZE_bitshift; pattern_size += step) pattern |= (pattern_base << pattern_size);

        register bitshift_t       shift         = vector_bitindex_calc(range_start); 
        register const bitshift_t pattern_shift = VECTORWORD_SIZE_bitshift + step - pattern_size; 

        #if VECTOR_ELEMENTS == 8
            register bitvector_t quadmask_base = { pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern };
        #elif VECTOR_ELEMENTS == 4
            register bitvector_t quadmask_base = { pattern, pattern, pattern, pattern };
        #else 
            register bitvector_t quadmask_base = { pattern, pattern };
        #endif

        for (counter_t current_word = vector_wordindex(range_start); current_word < vector_wordindex(range_stop_unique); current_word += VECTOR_ELEMENTS) {
            register bitshift_t shift1 = shift;
            if (pattern_shift > shift) shift += step;
            shift -= pattern_shift;
            register bitshift_t shift2 = shift;
            if (pattern_shift > shift) shift += step;
            shift -= pattern_shift;
            #if VECTOR_ELEMENTS <= 2
                register bitvector_t shiftmask = { shift1, shift2 };
            #else
                register bitshift_t shift3 = shift;
                if (pattern_shift > shift) shift += step;
                shift -= pattern_shift;
                register bitshift_t shift4 = shift;
                if (pattern_shift > shift) shift += step;
                shift -= pattern_shift;
                #if VECTOR_ELEMENTS <= 4
                    register bitvector_t shiftmask = { shift1, shift2, shift3, shift4 };
                #else
                    register bitshift_t shift5 = shift;
                    if (pattern_shift > shift) shift += step;
                    shift -= pattern_shift;
                    register bitshift_t shift6 = shift;
                    if (pattern_shift > shift) shift += step;
                    shift -= pattern_shift;
                    register bitshift_t shift7 = shift;
                    if (pattern_shift > shift) shift += step;
                    shift -= pattern_shift;
                    register bitshift_t shift8 = shift;
                    if (pattern_shift > shift) shift += step;
                    shift -= pattern_shift;
                    register bitvector_t shiftmask = { shift1, shift2, shift3, shift4, shift5, shift6, shift7, shift8 };
                #endif
            #endif

            register bitvector_t quadmask = quadmask_base << shiftmask;
            applyMask_vector(bitstorage_vector, step, range_stop, quadmask, current_vector);
            current_vector++;
        }
    }
    else {
        for (counter_t index = range_start; index < range_stop_unique;) {
            register const counter_t current_vector_start = vectorstart(index);

            // bitvector_t quadmask;
            #if VECTOR_ELEMENTS == 8
            register bitvector_t quadmask = { SAFE_ZERO, SAFE_ZERO, SAFE_ZERO, SAFE_ZERO, SAFE_ZERO, SAFE_ZERO, SAFE_ZERO, SAFE_ZERO };
            #elif VECTOR_ELEMENTS == 4
            register bitvector_t quadmask = { SAFE_ZERO, SAFE_ZERO, SAFE_ZERO, SAFE_ZERO };
            #else 
            register bitvector_t quadmask = { SAFE_ZERO, SAFE_ZERO };
            #endif

            if     (vector_wordstart(index) == (current_vector_start                              )) { quadmask[0] = vector_markmask(index); index += step; }
            if     (vector_wordstart(index) == (current_vector_start | (VECTORWORD_SIZE_counter  ))) { quadmask[1] = vector_markmask(index); index += step; }
            #if VECTOR_ELEMENTS > 2
                if (vector_wordstart(index) == (current_vector_start | (VECTORWORD_SIZE_counter*2))) { quadmask[2] = vector_markmask(index); index += step; }
                if (vector_wordstart(index) == (current_vector_start | (VECTORWORD_SIZE_counter*3))) { quadmask[3] = vector_markmask(index); index += step; }
            #endif
            #if VECTOR_ELEMENTS > 4
                if (vector_wordstart(index) == (current_vector_start | (VECTORWORD_SIZE_counter*4))) { quadmask[4] = vector_markmask(index); index += step; }
                if (vector_wordstart(index) == (current_vector_start | (VECTORWORD_SIZE_counter*5))) { quadmask[5] = vector_markmask(index); index += step; }
                if (vector_wordstart(index) == (current_vector_start | (VECTORWORD_SIZE_counter*6))) { quadmask[6] = vector_markmask(index); index += step; }
                if (vector_wordstart(index) == (current_vector_start | (VECTORWORD_SIZE_counter*7))) { quadmask[7] = vector_markmask(index); index += step; }
            #endif

            // use mask on all n*step multiples
            applyMask_vector(bitstorage_vector, step, range_stop, quadmask, current_vector);
            current_vector++;
        }
    }

    debug timerLapTime();
}

static inline void __attribute__((always_inline)) continuePattern_smallSize(bitword_t* __restrict bitstorage, const counter_t source_start, const counter_t size, const counter_t destination_stop)
{
    debug printf("Extending sieve size %ju in %ju bit range (%ju-%ju) using smallsize (%ju copies)", (uintmax_t)size, (uintmax_t)destination_stop-(uintmax_t)source_start,(uintmax_t)source_start,(uintmax_t)destination_stop, (uintmax_t)(((uintmax_t)destination_stop-(uintmax_t)source_start)/(uintmax_t)size));
    debug timerLapStart();

    const counter_t source_word = wordindex(source_start);
    register const bitword_t base_pattern = ((bitstorage[source_word] >> bitindex(source_start)) | (bitstorage[source_word+1] << (WORD_SIZE_counter-bitindex_calc(source_start)))) & chopmask(size);
    register bitword_t pattern = base_pattern;

    register counter_t pattern_size = size;
    if (pattern_size < (WORD_SIZE_counter >> 2)) {
        pattern |= (base_pattern << size) | (base_pattern << size*2) | (base_pattern << size*3);
        pattern_size = size << 2;
    }

    const counter_t destination_start = source_start + size;
    if ((destination_stop - destination_start) > pattern_size) {
        for (; pattern_size <= WORD_SIZE_counter; pattern_size += size) pattern |= (base_pattern << pattern_size);
        pattern_size -= size;
    }

    counter_t destination_start_word = wordindex(destination_start);
    const counter_t destination_stop_word = wordindex(destination_stop);
    if (destination_start_word >= destination_stop_word) {
        bitstorage[destination_start_word] |= (pattern << bitindex(destination_start)) & chopmask(destination_stop);
        debug timerLapTime();
        return;
    }

    bitstorage[destination_start_word] |= (pattern << bitindex(destination_start));

    register const bitshift_t pattern_shift = WORD_SIZE_bitshift - pattern_size;
    register bitshift_t shift = (WORD_SIZE_bitshift - bitindex_calc(destination_start)) & WORDMASK; // be sure this stays > 0
    register counter_t loop_range = destination_stop_word - destination_start_word;
    destination_start_word++;
    
    #pragma GCC ivdep
    for (counter_t i=0; i<=loop_range; ++i ) {
        bitstorage[destination_start_word+i] = (pattern << (pattern_size - ((shift+i*pattern_shift) & WORDMASK)  ) ) | (pattern >> ((shift+i*pattern_shift) & WORDMASK));
    }
    // bitstorage[destination_stop_word] &= chopmask(destination_stop); // not needed with appropriate block_size

    debug timerLapTime();
}

static inline void  __attribute__((always_inline)) continuePattern_aligned(bitword_t* bitstorage, const counter_t source_start, const counter_t size, const counter_t destination_stop)
{
    debug printf("Extending sieve size %ju in %ju bit range (%ju-%ju) using aligned (%ju copies)\n", (uintmax_t)size, (uintmax_t)destination_stop-(uintmax_t)source_start,(uintmax_t)source_start,(uintmax_t)destination_stop, (uintmax_t)(((uintmax_t)destination_stop-(uintmax_t)source_start)/(uintmax_t)size));
    debug timerLapStart();

    const counter_t destination_stop_word = wordindex(destination_stop);
    const counter_t copy_start = source_start + size;
    counter_t source_word = wordindex(source_start);
    counter_t copy_word = wordindex(copy_start);
    
    bitstorage[copy_word] = bitstorage[source_word] & ~chopmask(copy_start);

    while (copy_word + size <= destination_stop_word) {
        memcpy(&bitstorage[copy_word], &bitstorage[source_word], (uintmax_t)size*sizeof(bitword_t) );
        copy_word += size;
    }

   while (copy_word < destination_stop_word) {
        bitstorage[copy_word] = bitstorage[source_word];
        source_word++;
        copy_word++;
    }
    debug timerLapTime();
}

static inline void  __attribute__((always_inline)) continuePattern_shiftright(bitword_t* __restrict bitstorage, const counter_t source_start, const counter_t size, const counter_t destination_stop)
{
    debug printf("Extending sieve size %ju in %ju bit range (%ju-%ju) using shiftright (%ju copies)", (uintmax_t)size, (uintmax_t)destination_stop-(uintmax_t)source_start,(uintmax_t)source_start,(uintmax_t)destination_stop, (uintmax_t)(((uintmax_t)destination_stop-(uintmax_t)source_start)/(uintmax_t)size));
    debug timerLapStart();

    const counter_t destination_stop_word = wordindex(destination_stop);
    const counter_t copy_start = source_start + size;
    register const bitshift_t shift = bitindex_calc(copy_start) - bitindex_calc(source_start);
    register const bitshift_t shift_flipped = WORD_SIZE_bitshift-shift;
    register counter_t source_word = wordindex(source_start);
    register counter_t copy_word = wordindex(copy_start);

    if unlikely(copy_word >= destination_stop_word) { 
        bitstorage[copy_word] |= ((bitstorage[source_word] << shift)  // or the start in to not lose data
                                | (bitstorage[copy_word] >> shift_flipped))
                                & keepmask(copy_start) & chopmask(destination_stop);
        return; // rapid exit for one word variant
    }

    bitstorage[copy_word] |= ((bitstorage[source_word] << shift)  // or the start in to not lose data
                                | (bitstorage[copy_word] >> shift_flipped))
                                & keepmask(copy_start);
    
    copy_word++;

    debug { printf("...start - %ju - %ju - end..",(uintmax_t)wordindex(copy_start), (uintmax_t)destination_stop_word) ; }

    if (copy_word < source_word + VECTOR_ELEMENTS) {
        debug printf("...continue word by word (because source and copy are close together)..");
        for (;copy_word <= destination_stop_word; copy_word++, source_word++ ) 
            bitstorage[copy_word] = (bitstorage[source_word] >> shift_flipped) | (bitstorage[source_word+1] << shift);
        return; 
    }

    counter_t copy_size_byte  = size;
    counter_t copy_start_word = wordindex(vectorend(copy_start + (copy_size_byte << SHIFT_BYTE))+1); 
    if (copy_start_word > destination_stop_word) copy_start_word = destination_stop_word;

    // copy with shift - needed the not aligned at bytelevel
    // speed up when source and copy are further apart - may vectorize the loop

    debug printf("...speed copy until word %ju..", (uintmax_t)copy_start_word);

    #ifdef WORD_SIZE_64
        #pragma GCC ivdep // only for 64bit
        for (; copy_word <= copy_start_word; copy_word++, source_word++ ) 
            bitstorage[copy_word] = (bitstorage[source_word] >> shift_flipped) | (bitstorage[source_word+1] << shift);
    #else
        for (; copy_word <= copy_start_word; copy_word++, source_word++ ) 
            bitstorage[copy_word] = (bitstorage[source_word] >> shift_flipped) | (bitstorage[source_word+1] << shift);
    #endif
    // end if we reached the destination already
    if (copy_word >= destination_stop_word) return;

    register uint8_t* __restrict source_byte            = (u_int8_t*)((uintptr_t) bitstorage + (copy_start_word << (SHIFT_WORD-SHIFT_BYTE) ) - copy_size_byte);
    register uint8_t* __restrict copy_byte              = (u_int8_t*)((uintptr_t) bitstorage + (copy_start_word << (SHIFT_WORD-SHIFT_BYTE) ));
    const uint8_t* __restrict destination_stop_byte     = (u_int8_t*)((uintptr_t) bitstorage + ((destination_stop_word + 1) << SHIFT_BYTE) );

    do {
        memcpy(copy_byte, source_byte, copy_size_byte);
        copy_byte += copy_size_byte;
        copy_size_byte += copy_size_byte;
    } while (copy_byte + copy_size_byte < destination_stop_byte);

    memcpy(copy_byte, source_byte, destination_stop_byte - copy_byte);

    debug timerLapTime();
}

static inline counter_t  __attribute__((always_inline)) continuePattern_shiftleft_unrolled(bitword_t* __restrict bitstorage, const counter_t aligned_copy_word, const bitshift_t shift, counter_t copy_word, counter_t source_word) 
{
    #if is_signed(bitword_t)
    const counter_t fast_loop_stop_word = aligned_copy_word;
    #else
    const counter_t fast_loop_stop_word = (aligned_copy_word>2) ? (aligned_copy_word - 2) : 0; // safe for unsigned ints
    #endif

    register const bitshift_t shift_flipped = WORD_SIZE_bitshift-shift;
    counter_t distance = 0;

    while (copy_word < fast_loop_stop_word) {
        register const bitword_t source0 = bitstorage[source_word  ];
        register const bitword_t source1 = bitstorage[source_word+1];
        bitstorage[copy_word  ] = (source0 >> shift) | (source1 << shift_flipped);
        register const bitword_t source2 = bitstorage[source_word+2];
        bitstorage[copy_word+1] = (source1 >> shift) | (source2 << shift_flipped);
        copy_word += 2;
        source_word += 2;
        distance += 2;
    }
    return distance;
}

static inline void __attribute__((always_inline)) continuePattern_shiftleft(bitword_t* bitstorage, const counter_t source_start, const counter_t size, const counter_t destination_stop)
{
    debug printf("Extending sieve size %ju in %ju bit range (%ju-%ju) using shiftleft (%ju copies)", (uintmax_t)size, (uintmax_t)destination_stop-(uintmax_t)source_start,(uintmax_t)source_start,(uintmax_t)destination_stop, (uintmax_t)(((uintmax_t)destination_stop-(uintmax_t)source_start)/(uintmax_t)size));
    debug timerLapStart();
    
    const counter_t destination_stop_word = wordindex(destination_stop);
    const counter_t copy_start = source_start + size;
    register const bitshift_t shift = bitindex_calc(source_start) - bitindex_calc(copy_start);
    register const bitshift_t shift_flipped = WORD_SIZE_bitshift-shift;
    register counter_t source_word = wordindex(source_start);
    register counter_t copy_word = wordindex(copy_start);
    bitstorage[copy_word] |= ((bitstorage[source_word] >> shift)
                             | (bitstorage[source_word+1] << shift_flipped))
                             & ~chopmask(copy_start); // because this is the first word, dont copy the extra bits in front of the source

    copy_word++;
    source_word++;

    const counter_t aligned_copy_word = min(source_word + size, destination_stop_word); // after <<size>> words, just copy at word level
    const counter_t distance  = continuePattern_shiftleft_unrolled(bitstorage, aligned_copy_word, shift, copy_word, source_word);
    source_word += distance;
    copy_word += distance;

    debug { counter_t fast_loop_stop_word = uintsafeminus(aligned_copy_word,2); printf("...start - %ju - end fastloop - %ju - start alignment - %ju - end", (uintmax_t)fast_loop_stop_word - (uintmax_t)wordindex(copy_start), (uintmax_t)aligned_copy_word - (uintmax_t)fast_loop_stop_word, (uintmax_t)destination_stop_word - (uintmax_t)aligned_copy_word); }

    for (;copy_word <= aligned_copy_word; copy_word++,source_word++) {
        bitstorage[copy_word] = (bitstorage[source_word  ] >> shift) | (bitstorage[source_word+1 ] << shift_flipped);
    }

    if (copy_word >= destination_stop_word) return;

    source_word = copy_word - size; // recalibrate
    const size_t memsize = (size_t)size*sizeof(bitword_t);

    for (;copy_word + size <= destination_stop_word; copy_word += size) 
        memcpy(&bitstorage[copy_word], &bitstorage[source_word],memsize );

    for (;copy_word <= destination_stop_word;  copy_word++,source_word++)
        bitstorage[copy_word] = bitstorage[source_word];

    debug timerLapTime();
}

// continue a pattern that start at <source_start> with a size of <size>.
// repeat this pattern up to <destination_stop>.
// for small sizes, this is done on a word level
// for larger sizes, we look at the offset / start bit and apply the appropriate algorithm.
// note that these algorithms are general for bitstorage and have no specialized assumptions for the sieve application
static inline void __attribute__((always_inline)) continuePattern(bitword_t* bitstorage, const counter_t source_start, const counter_t size, const counter_t destination_stop)
{
    if (size < WORD_SIZE_counter) {
        continuePattern_smallSize (bitstorage, source_start, size, destination_stop);
        return;
    }

    const bitshift_t copy_bit   = bitindex_calc(source_start + size);
    const bitshift_t source_bit = bitindex_calc(source_start);

    if      (source_bit > copy_bit) continuePattern_shiftleft (bitstorage, source_start, size, destination_stop);
    else if (source_bit < copy_bit) continuePattern_shiftright(bitstorage, source_start, size, destination_stop);
    else                            continuePattern_aligned   (bitstorage, source_start, size, destination_stop);
}

static counter_t sieve_block_stripe(bitword_t* bitstorage, const counter_t block_start, const counter_t block_stop, const counter_t prime_start, const counter_t maxprime)
{
    counter_t prime = prime_start;
    counter_t start = 0;
    counter_t step  = prime * 2 + 1;

    debug printf("Block stripe for block %ju - %ju\n",(uintmax_t)block_start,(uintmax_t)block_stop);
    
    while ((prime < maxprime) && (prime * step <= block_stop)) {
        if likely(block_start > prime)
            start = (block_start + prime) + prime - ((block_start + prime) % step);
        else
            start = prime * step + prime; 

        if unlikely(step < VECTORSTEP_FASTER)
            setBitsTrue_largeRange_vector(bitstorage, start, step, block_stop);
        else 
            setBitsTrue_largeRange(bitstorage, start, step, block_stop);

        prime = searchBitFalse(bitstorage, prime + 1 );
        step  = prime * 2 + 1;
    }
    return prime;
}

// structure to help sieve_block_extend report back to the main module
struct block {
    counter_t pattern_size; // size of pattern applied 
    counter_t pattern_start; // start of pattern
    counter_t prime; // next prime to be striped
};

// returns prime that could not be handled:
// start is too large
// range is too big
static struct block sieve_block_extend(struct sieve_t *sieve, const counter_t block_start, const counter_t block_stop) 
{
    bitword_t* bitstorage      = sieve->bitstorage;
    register counter_t prime   = 0;
    counter_t patternsize_bits = 1;
    counter_t pattern_start    = 0;
    counter_t range_stop       = block_start;
    struct block block = { .prime = 0, .pattern_start = 0, .pattern_size = 0 };

    sieve->bitstorage[wordindex(block_start)] = SAFE_ZERO; // only the first word has to be cleared; the rest is populated by the extension procedure
    
    for (;range_stop < block_stop;) {
        prime = searchBitFalse(bitstorage, prime+1);
        counter_t start = (prime * prime * 2) + (prime * 2);
        if unlikely(start > block_stop) break;

        const counter_t step = prime * 2 + 1;
        if (block_start > prime) start = (block_start + prime) + prime - ((block_start + prime) % step);

        range_stop = block_start + patternsize_bits * step * 2;  // range is x2 so the second block cointains all multiples of primes
        block.pattern_size = patternsize_bits;
        block.pattern_start = pattern_start;
        if unlikely(range_stop > block_stop) break;//return block; //range_stop = block_stop;

        if likely(patternsize_bits>1) {
            pattern_start = block_start | patternsize_bits;
            continuePattern(bitstorage, pattern_start, patternsize_bits, range_stop);
        }
        patternsize_bits *= step;

        if (step < MEDIUMSTEP_FASTER)      setBitsTrue_mediumStep(bitstorage, start, step, range_stop);
        else if (step < VECTORSTEP_FASTER) setBitsTrue_largeRange_vector(bitstorage, start, step, range_stop);
        else                               setBitsTrue_largeRange(bitstorage, start, step, range_stop);
        block.prime = prime;
    } 

    return block;
}

/* This is the main module that directs all the work*/
static struct sieve_t* sieve_shake_blockbyblock(const counter_t maxFactor, const counter_t blocksize) 
{
    struct sieve_t *sieve = sieve_create(maxFactor);
    bitword_t* bitstorage = sieve->bitstorage;
    counter_t startprime  = 1;
    for(counter_t index=0; index<wordindex(maxFactor/2); index++) {
        bitstorage[index]=SAFE_ZERO;
    }

    debug printf("\nShaking sieve to find all primes up to %ju with blocksize %ju\n",(uintmax_t)maxFactor,(uintmax_t)blocksize);

    // in the sieve all bits for the multiples of primes up to startprime have been set
    // process the sieve and stripe all the multiples of primes > start_prime
    // do this block by block to minimize cache misses
    for (counter_t block_start = 0,  block_stop = blocksize-1;block_start <= sieve->bits; block_start += blocksize, block_stop += blocksize) {
        if unlikely(block_stop > sieve->bits) block_stop = sieve->bits;
        counter_t prime = searchBitFalse(bitstorage, startprime);
        sieve_block_stripe(bitstorage, block_start, block_stop, prime, maxFactor);
    } 

    // return the completed sieve
    return sieve;
}

/* This is the main module that directs all the work*/
static struct sieve_t* sieve_shake(const counter_t maxFactor, const counter_t blocksize) 
{
    struct sieve_t *sieve = sieve_create(maxFactor);
    bitword_t* bitstorage = sieve->bitstorage;

    debug printf("\nShaking sieve to find all primes up to %ju with blocksize %ju\n",(uintmax_t)maxFactor,(uintmax_t)blocksize);

    // fill the whole sieve bij adding en copying incrementally
    struct block block = sieve_block_extend(sieve, 0, sieve->bits);

    // continue the found pattern to the entire sieve
    continuePattern(bitstorage, block.pattern_start, block.pattern_size, sieve->bits);

    // continue from the max prime that was processed in the pattern until the tuned value
    counter_t startprime = block.prime;
    if (startprime < BLOCKSTEP_FASTER) startprime = sieve_block_stripe(bitstorage, 0, sieve->bits, startprime, BLOCKSTEP_FASTER);

    // in the sieve all bits for the multiples of primes up to startprime have been set
    // process the sieve and stripe all the multiples of primes > start_prime
    // do this block by block to minimize cache misses
    for (counter_t block_start = 0, block_stop = blocksize-1; block_start <= sieve->bits; block_start += blocksize, block_stop += blocksize) {
        if unlikely(block_stop > sieve->bits) block_stop = sieve->bits;
        counter_t prime = searchBitFalse(bitstorage, startprime);
        sieve_block_stripe(bitstorage, block_start, block_stop, prime, maxFactor);
    } 

    // return the completed sieve
    return sieve;
}

static void show_primes(struct sieve_t *sieve, counter_t maxFactor) 
{
    counter_t primeCount = 1;    // We already have 2
    for (counter_t factor=1; factor < sieve->bits; factor = searchBitFalse(sieve->bitstorage, factor+1)) {
        primeCount++;
        if (factor < maxFactor/2) {
            printf("%3ju ",(uintmax_t)factor*2+1);
            if (primeCount % 10 == 0) printf("\n");
        }
    }
    printf("\nFound %ju primes until %ju\n",(uintmax_t)primeCount, (uintmax_t)sieve->bits*2+1);
}

static counter_t count_primes(struct sieve_t *sieve) {
    counter_t primeCount = 1;
    for (counter_t factor=1; factor < sieve->bits; factor = searchBitFalse(sieve->bitstorage, factor+1)) primeCount++;
    return primeCount;
}

static void deepAnalyzePrimes(struct sieve_t *sieve) 
{
    printf("DeepAnalyzing\n");
    counter_t warn_prime = 0;
    counter_t warn_nonprime = 0;
    for (counter_t prime = 1; prime < sieve->bits; prime++ ) {
        if ((sieve->bitstorage[wordindex(prime)] & markmask_calc(prime))==0) { // is this a prime?
            for(counter_t c=1; c<=sieve->bits && c*c <= prime*2+1; c++) {
                if ((prime*2+1) % (c*2+1) == 0 && (c*2+1) != (prime*2+1)) {
                    if (warn_prime++ < 30) printf("Number %ju (%ju) was marked prime, but %ju * %ju = %ju\n", (uintmax_t)prime*2+1, (uintmax_t)prime, (uintmax_t)c*2+1, (uintmax_t)((prime*2+1)/(c*2+1)), (uintmax_t)prime*2+1 );
                }
            }
        }
        else {
            counter_t c_prime = 0;
            for(counter_t c=1; c<=sieve->bits && c*c <= prime*2+1; c++) {
                if ((prime*2+1) % (c*2+1) == 0 && (c*2+1) != (prime*2+1)) c_prime++;
            }
            if (c_prime==0 && warn_nonprime++ < 30) printf("Number %ju (%ju) was marked non-prime, but no factors found. So it is prime\n", (uintmax_t)prime*2+1,(uintmax_t) prime);
        }
    }
}

static int validatePrimeCount(struct sieve_t *sieve) 
{
    counter_t primecount = count_primes(sieve);
    counter_t valid_primes = 0;
    switch(sieve->size) {
        case 10:            valid_primes = 4;         break;
        case 100:           valid_primes = 25;        break;
        case 1000:          valid_primes = 168;       break;
        case 10000:         valid_primes = 1229;      break;
        case 100000:        valid_primes = 9592;      break;
        case 1000000:       valid_primes = 78498;     break;
        case 10000000:      valid_primes = 664579;    break;
        case 100000000:     valid_primes = 5761455;   break;
        case 1000000000:    valid_primes = 50847534;  break;
        // case 10000000000:   valid_primes = 455052511; break;
        default:            valid_primes= 0;
    }

    int valid = (valid_primes == primecount);
    verbose(4) if (valid) printf("Result: Sievesize %ju is expected to have %ju primes. algorithm produced %ju primes\n",(uintmax_t)sieve->size,(uintmax_t)valid_primes,(uintmax_t)primecount );
    verbose(1) if (!valid) {
        printf("No valid result. Sievesize %ju was expected to have %ju primes, but algorithm produced %ju primes\n",(uintmax_t)sieve->size,(uintmax_t)valid_primes,(uintmax_t)primecount );
        verbose(2) show_primes(sieve, default_show_primes_on_error);
        verbose(2) deepAnalyzePrimes(sieve);
    }
    return (valid);
}

typedef struct  {
    counter_t maxFactor;
    counter_t blocksize_bits;
    counter_t blocksize_kB;
    counter_t free_bits;
    counter_t blockstep_faster;
    counter_t mediumstep_faster;
    counter_t vectorstep_faster;
    counter_t threads;
    double    sample_duration;
    counter_t passes;
    double    elapsed_time;
    double    avg;
} benchmark_result_t;

static int compare_tuning_result(const void *a, const void *b) 
{
    benchmark_result_t *resultA = (benchmark_result_t *)a;
    benchmark_result_t *resultB = (benchmark_result_t *)b;
    return (resultB->avg > resultA->avg ? 1 : -1);
}

static void benchmark(benchmark_result_t* tuning_result) 
{
    // don't use VECTORSTEP for steps larger than VECTOR_SIZE
    if (tuning_result->vectorstep_faster > VECTOR_SIZE_counter ) tuning_result->vectorstep_faster = VECTOR_SIZE_counter;

    global_BLOCKSTEP_FASTER = tuning_result->blockstep_faster;
    global_MEDIUMSTEP_FASTER = tuning_result->mediumstep_faster;
    global_VECTORSTEP_FASTER = tuning_result->vectorstep_faster;
    double sample_duration = tuning_result->sample_duration * CLOCKS_PER_SEC;

    counter_t passes = 0;
    double elapsed_time = 0;
    clock_t startTime = clock();
    #ifdef _OPENMP
    omp_set_num_threads(tuning_result->threads);
    sample_duration *= tuning_result->threads;
    #pragma omp parallel reduction(+:passes)
    #endif
    while (elapsed_time <= sample_duration) {
        struct sieve_t *sieve = sieve_shake(tuning_result->maxFactor, tuning_result->blocksize_bits);
        sieve_delete(sieve);
        elapsed_time = (double)(clock() - startTime);         
        passes++;
    }
    tuning_result->passes = passes;
    tuning_result->elapsed_time = elapsed_time / CLOCKS_PER_SEC / tuning_result->threads;
    tuning_result->avg = passes/elapsed_time;
}

static inline void tuning_result_print(benchmark_result_t tuning_result) 
{
    printf("blocksize_bits %10ju; blocksize %4jukB; free_bits %5ju; small %2ju; medium %2ju; vector %3ju; passes %3ju; time %f/%f;average %f\n", 
                            (uintmax_t)tuning_result.blocksize_bits, (uintmax_t)tuning_result.blocksize_kB,(uintmax_t)tuning_result.free_bits,
                            (uintmax_t)tuning_result.blockstep_faster,(uintmax_t)tuning_result.mediumstep_faster,(uintmax_t)tuning_result.vectorstep_faster,
                            (uintmax_t)tuning_result.passes, tuning_result.elapsed_time, tuning_result.sample_duration, tuning_result.avg);
}

static benchmark_result_t tune(int tune_level, counter_t maxFactor, counter_t threads, counter_t option_blocksize_kB) 
{
    counter_t best_blocksize_bits = default_blocksize;

    double best_avg = 0;
    best_blocksize_bits = 0;
    counter_t best_blockstep_faster = 0;
    counter_t best_mediumstep_faster = 0;
    counter_t best_vectorstep_faster = 0;
    counter_t blockstep_faster_steps = 4;
    counter_t mediumstep_faster_steps = 4;
    counter_t vectorstep_faster_steps = 32;
    counter_t freebits_steps = anticiped_cache_line_bytesize;
    double sample_duration = default_sample_duration;

    // determines the size of the resultset
    switch (tune_level) {
        case 1:
            blockstep_faster_steps  = WORD_SIZE/2;
            mediumstep_faster_steps = WORD_SIZE/4;
            vectorstep_faster_steps = VECTOR_SIZE_counter/4;
            freebits_steps = anticiped_cache_line_bytesize*8*2;
            sample_duration = default_sample_duration;
            break;
        case 2:
            blockstep_faster_steps  = WORD_SIZE/4;
            mediumstep_faster_steps = WORD_SIZE/16;
            vectorstep_faster_steps = VECTOR_SIZE_counter/8;
            freebits_steps = anticiped_cache_line_bytesize*8;
            sample_duration = default_sample_duration*2;
            break;
        case 3:
            blockstep_faster_steps  = WORD_SIZE/16;
            mediumstep_faster_steps = WORD_SIZE/16;
            vectorstep_faster_steps = VECTOR_SIZE_counter/16;
            freebits_steps = anticiped_cache_line_bytesize/2;
            sample_duration = default_sample_duration*4;
            break;
    }
    
    verbose(1) { 
        verbose(2) printf("\n");
        printf("Tuning..."); 
        verbose(2) printf(".. best options (shown when found):\n");
        fflush(stdout);
    }
    const size_t max_results = ((size_t)(VECTOR_SIZE_counter/blockstep_faster_steps)+1) * ((size_t)(VECTOR_SIZE_counter/mediumstep_faster_steps)+1) * ((size_t)(VECTOR_SIZE_counter/vectorstep_faster_steps)+1) * 32 * (size_t)(anticiped_cache_line_bytesize*8*4/freebits_steps);
    benchmark_result_t* tuning_result = malloc(max_results * sizeof(tuning_result));
    counter_t tuning_results=0;
    counter_t tuning_result_index=0;
    
    for (counter_t blockstep_faster = 0; blockstep_faster <= VECTOR_SIZE_counter; blockstep_faster += blockstep_faster_steps) {
        for (counter_t mediumstep_faster = 0; mediumstep_faster <= WORD_SIZE_counter; mediumstep_faster += mediumstep_faster_steps) {
            for (counter_t vectorstep_faster = mediumstep_faster; vectorstep_faster <= VECTOR_SIZE_counter; vectorstep_faster += vectorstep_faster_steps) {
                for (counter_t blocksize_kB=64; blocksize_kB>=8; blocksize_kB /= 2) {
                    for (counter_t free_bits=0; (free_bits < (anticiped_cache_line_bytesize*8*4) && (free_bits < blocksize_kB * 1024 * 8)); free_bits += freebits_steps) {

                        // hack to ovrrule tuning of user setting
                        if (option_blocksize_kB) { blocksize_kB=option_blocksize_kB; free_bits=0; }

                        counter_t blocksize_bits = (blocksize_kB * 1024 * 8) - free_bits;

                        // set variables
                        tuning_results++;
                        tuning_result[tuning_result_index].maxFactor = maxFactor;
                        tuning_result[tuning_result_index].sample_duration = sample_duration;
                        tuning_result[tuning_result_index].blocksize_kB = blocksize_kB;
                        tuning_result[tuning_result_index].free_bits = free_bits;
                        tuning_result[tuning_result_index].blocksize_bits = blocksize_bits;
                        tuning_result[tuning_result_index].blockstep_faster = blockstep_faster;
                        tuning_result[tuning_result_index].mediumstep_faster = mediumstep_faster;
                        tuning_result[tuning_result_index].vectorstep_faster = vectorstep_faster;
                        tuning_result[tuning_result_index].threads = threads;
                        benchmark(&tuning_result[tuning_result_index]);

                        if ( tuning_result[tuning_result_index].avg > best_avg) {
                            best_avg = tuning_result[tuning_result_index].avg;
                            best_blocksize_bits = blocksize_bits;
                            best_blockstep_faster = blockstep_faster;
                            best_mediumstep_faster = mediumstep_faster;
                            best_vectorstep_faster = vectorstep_faster;
                            verbose(2) { printf(".(<)"); tuning_result_print(tuning_result[tuning_result_index]); fflush(stdout); }
                        }
                        verbose(3) { printf("...."); tuning_result_print(tuning_result[tuning_result_index]); }
                        tuning_result_index++;
                        verbose_at(1) { printf("\rTuning...tuning %ju options..in %lf seconds  ",(uintmax_t)tuning_results, (double)tuning_results*sample_duration ); fflush(stdout); }
                        if (option_blocksize_kB) break;
                    }
                    if (option_blocksize_kB) break;
                }
            }
        }
    }
    verbose_at(1) { printf("\rTuning...tuned %ju options..",(uintmax_t)tuning_results); }
    verbose(2) {
        printf("Finished scan of \033[1;33m%ju\033[0m options. Inital best blocksize: %ju; best blockstep %ju; best mediumstep %ju; best vectorstep %ju\n",(uintmax_t)tuning_results,(uintmax_t)best_blocksize_bits, (uintmax_t)best_blockstep_faster,(uintmax_t)best_mediumstep_faster, (uintmax_t)best_vectorstep_faster);
        printf("Finding the best option by reevaluating the top options with a longer sample duration.\n");
    }

    counter_t tuning_results_max = tuning_results; // keep this value for verbose messages
    for (counter_t step=0; tuning_results>4; step++) {
        qsort(tuning_result, (size_t)tuning_results, sizeof(benchmark_result_t), compare_tuning_result);
        verbose(2) {
            printf("\r(iteration %1ju) ",(uintmax_t)step); tuning_result_print(tuning_result[0]); fflush(stdout);
            verbose(3) {
                for (tuning_result_index=0; tuning_result_index<min(40,tuning_results); tuning_result_index++) {
                    printf("..."); tuning_result_print(tuning_result[tuning_result_index]);
                }
            }
        }

        tuning_results = tuning_results / 4;
        // sample_duration *= 2;

        for (counter_t i=0; i<tuning_results; i++) {
            tuning_result[i].sample_duration = sample_duration;
            verbose(1) { printf("\rTuning...found %ju options..benchmarking step %ju - tuning %3ju options in %lf seconds",(uintmax_t)tuning_results_max,(uintmax_t)step, (uintmax_t)i, (double)tuning_results*sample_duration); fflush(stdout); }
            benchmark(&tuning_result[i]);
        }
    }

    // take best result
    benchmark_result_t best_result = tuning_result[0];
    free(tuning_result);
    verbose(1) {
        printf("\33[2K\r");
        verbose(2) { printf("Best result:  "); tuning_result_print(best_result); printf("\n"); }
    }
    return best_result;
}

static void usage(char *name) 
{
    fprintf(stderr, "Usage: %s [options] [maximum]\n", name);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  --block <kilobyte> Set the block size to a specific <size> in kilobytes\n");
    fprintf(stderr, "  --check            Check the correctness of the algorithm\n");
    #if compile_debuggable
    fprintf(stderr, "  --explain          Explain the steps of the algorithm - only when compiled for debug\n");
    #endif
    fprintf(stderr, "  --help             This help function\n");
    fprintf(stderr, "  --show  <maximum>  Show the primes found up to the maximum\n");
    #ifdef _OPENMP
    fprintf(stderr, "  --threads <count>  Set the maximum number of threads to be used (only when compiled for openmp)\n");
    fprintf(stderr, "                     Use 'all' to use all available threads or 'half' for /2 (e.g. for no hyperthreading)\n");
    #endif
    fprintf(stderr, "  --time  <seconds>  The maximum time (in seconds) to run passes of the sieve algorithm\n");
    fprintf(stderr, "  --tune  <level>    find the best settings for the current os and hardware\n");
    fprintf(stderr, "                     1 - fast tuning\n");
    fprintf(stderr, "                     2 - refined tuning\n");
    fprintf(stderr, "                     3 - maximum tuning (takes long)\n");
    fprintf(stderr, "  --verbose <level>  Show more output to a certain level:\n");
    fprintf(stderr, "                     1 - show phase progress\n");
    fprintf(stderr, "                     2 - show general progress within the phase\n");
    fprintf(stderr, "                     3 - show actual work\n");
    exit(1);
}

static struct options_t parseCommandLine(int argc, char *argv[], struct options_t option)
{
    // processing command line changes to options
    for (int arg=1; arg < argc; arg++) {
        if (strcmp(argv[arg], "--help")==0) { usage(argv[0]); }
        else if (strcmp(argv[arg], "--verbose")==0) {
            if (++arg >= argc) { fprintf(stderr, "No verbose level specified\n"); usage(argv[0]); }
            if (sscanf(argv[arg], "%d", &option.verboselevel) != 1 || option.verboselevel > 4) {
                fprintf(stderr, "Error: Invalid measurement time: %s\n", argv[arg]); usage(argv[0]);
            }
            verbose(1) printf("Verbose level set to %d\n",option.verboselevel);
        } 
        else if (strcmp(argv[arg], "--block")==0) {
            if (++arg >= argc) { fprintf(stderr, "No block size specified\n"); usage(argv[0]); }
            if (sscanf(argv[arg], "%ju", (uintmax_t*)&option.blocksize_kB) != 1) {
                fprintf(stderr, "Error: Invalid size in kilobyte: %s\n", argv[arg]); usage(argv[0]);
            }
            counter_t sieve_bits = option.maxFactor >> 1;
            if ((option.blocksize_kB*1024*8) > (sieve_bits)) option.blocksize_kB = (sieve_bits / (1024*8))+1;
            verbose(1) printf("Blocksize set to %ju kB\n",(uintmax_t)option.blocksize_kB);
        } 
        #if compile_debuggable
        else if (strcmp(argv[arg], "--explain")==0) { option.explain=1; }
        #endif
        else if (strcmp(argv[arg], "--check")==0) { option.check=1; }
        else if (strcmp(argv[arg], "--nocheck")==0) { option.check=0; }
        else if (strcmp(argv[arg], "--tune")==0) { option.tunelevel=0;
            if (++arg >= argc) { fprintf(stderr, "No tune level specified\n"); usage(argv[0]); }
            if (sscanf(argv[arg], "%d", &option.tunelevel) != 1 || option.tunelevel > 4) {
                fprintf(stderr, "Error: Invalid tune level: %s\n", argv[arg]); usage(argv[0]);
            }
            verbose(1) printf("Tune level set to %d\n",option.tunelevel);
        }
        else if (strcmp(argv[arg], "--time")==0) { option.maxTime=0;
            if (++arg >= argc) { fprintf(stderr, "No time specified\n"); usage(argv[0]); }
            if (sscanf(argv[arg], "%lf", &option.maxTime) != 1 ) {
                fprintf(stderr, "Error: Invalid max time: %s\n", argv[arg]); usage(argv[0]);
            }
            verbose(1) printf("Max time is set to %d seconds\n",option.tunelevel);
        }
        else if (strcmp(argv[arg], "--show")==0) { option.showMaxFactor=0;
            if (++arg >= argc) { fprintf(stderr, "No show maximum specified\n"); usage(argv[0]); }
            if (sscanf(argv[arg], "%ju", (uintmax_t*)&option.showMaxFactor) != 1 || option.showMaxFactor > option.maxFactor) {
                fprintf(stderr, "Error: Invalid show maximum: %s\n", argv[arg]); usage(argv[0]);
            }
            verbose(1) printf("Show maximum set to %ju\n",(uintmax_t)option.showMaxFactor);
        }
        else if (strcmp(argv[arg], "--threads")==0) { 
            if (++arg >= argc) { fprintf(stderr, "No thread maximum specified\n"); usage(argv[0]); }
        #ifdef _OPENMP
            int max_threads = omp_get_max_threads();
            if (strcmp(argv[arg], "all")==0) option.threads = max_threads;
            else if (strcmp(argv[arg], "half")==0) option.threads = max_threads>>1;
            else if (sscanf(argv[arg], "%d", &option.threads) != 1 ) { fprintf(stderr, "Error: Invalid max threads: %s\n", argv[arg]); usage(argv[0]); }
            if (option.threads <1)  option.threads = 1;
            if (option.threads > max_threads)  option.threads = max_threads;
            verbose(1) printf("Thread maximum set to %ju\n",(uintmax_t)option.threads);
        #else
            verbose(1) printf("This is the version without multithreading - ignoring threads\n");
        #endif
        }
        else if (sscanf(argv[arg], "%ju", (uintmax_t*)&option.maxFactor) != 1) {
            fprintf(stderr, "Invalid size %s\n",argv[arg]); usage(argv[0]); 
            printf("Maximum set to %ju\n",(uintmax_t)option.maxFactor);
        }

        counter_t sieve_bits = option.maxFactor >> 1;
        if ((option.blocksize_kB*1024*8) > (sieve_bits)) {
            option.blocksize_kB = (sieve_bits / (1024*8))+1;
            verbose(1) printf("Blocksize corrected to %ju kB\n",(uintmax_t)option.blocksize_kB);
        }

    }
    return option;
}

static struct options_t setDefaultOptions() {
    option.maxTime       = default_maxTime;
    option.maxFactor     = default_sieve_limit;
    option.showMaxFactor = default_showMaxFactor;
    option.explain       = default_explain_level;
    option.verboselevel  = default_verbose_level;
    option.check         = default_check_level;
    option.tunelevel     = default_tune_level;
    option.threads       = 1;
    option.blocksize_kB  = 0; // this is what the user entered
    #ifdef _OPENMP
    option.threads = omp_get_max_threads();
    #endif

    return option;
}

#if compile_debuggable
static void explainSieveShake() 
{
    // warm up
    int org_option_explain = option.explain;
    option.explain = 0;
    for (int i=0; i<10; i++) {
        struct sieve_t* sieve = sieve_shake(option.maxFactor, default_blocksize);
        sieve_delete(sieve);
    }    
    option.explain = org_option_explain;

    struct sieve_t* sieve = sieve_shake(option.maxFactor, default_blocksize);
    printf("\nResult set:\n");
    show_primes(sieve, min(option.showMaxFactor,100));
    int valid = validatePrimeCount(sieve);
    if (!valid) printf("The sieve is \033[0;31m\033[5mNOT\033[0;0m valid...\n");
    else printf("The sieve is \033[0;mVALID\033[0;0m\n");
    sieve_delete(sieve);
    exit(0);
}
#endif

static void checkSieveAlgorithm()
{
    verbose(1) { 
        printf("Validating..."); 
        verbose(2) printf("\n");
        fflush(stdout); 
    }

    // validate algorithm - run one time for all sizes
    for (counter_t sieveSize_check = 100; sieveSize_check <= 100000000; sieveSize_check *=10) {
        verbose(2) {
            printf("..Checking size %ju ...",(uintmax_t)sieveSize_check); 
            verbose(3) printf("\n");
            fflush(stdout); 
        }
        struct sieve_t *sieve_check;
        for (counter_t blocksize_bits=1024; blocksize_bits<=256*1024*8; blocksize_bits *= 2) {
            verbose(3) printf("....Blocksize %ju:",(uintmax_t)blocksize_bits);
            sieve_check = sieve_shake(sieveSize_check, blocksize_bits);
            int valid = validatePrimeCount(sieve_check);
            sieve_delete(sieve_check);
            if (!valid) {
                fprintf(stderr,"Invalid count for %ju Settings used: blocksize %ju, %ju/%ju/%ju/%ju/%ju\n",(uintmax_t)sieveSize_check,(uintmax_t)blocksize_bits,(uintmax_t)global_BLOCKSTEP_FASTER,(uintmax_t)global_MEDIUMSTEP_FASTER,(uintmax_t)global_VECTORSTEP_FASTER,(uintmax_t)WORD_SIZE_counter,(uintmax_t)VECTOR_ELEMENTS);
                exit(1); 
            }
            else verbose(3) printf("\033[0;32mvalid\033[0;0m\n");
        }
        verbose(2) printf("\033[0;32mvalid\033[0;0m\n");
    }
    verbose(1) printf("\033[0;32mvalid\033[0;0m algorithm\n");
}

int main(int argc, char *argv[]) 
{
    option = setDefaultOptions();
    option = parseCommandLine(argc, argv, option);
    
    verbose(2) {
        printf("\nRunning sieve algorithm by Rogier van Dam with the following target:\n");
        printf("Count all primes up to \033[1;33m%ju\033[0m using the sieve of Eratosthenes\n", (uintmax_t)option.maxFactor);
        printf("\n");
    }

    #if compile_debuggable
    if (option.explain==1) explainSieveShake();
    #endif
    
    // command line --check can be used to check the algorithm for all sieve/blocksize combinations
    if (option.check) checkSieveAlgorithm(); 

    benchmark_result_t benchmark_result;
    benchmark_result.sample_duration   = option.maxTime;
    benchmark_result.blocksize_bits    = global_BLOCKSIZE_BITS;
    benchmark_result.blockstep_faster  = global_BLOCKSTEP_FASTER;
    benchmark_result.mediumstep_faster = global_MEDIUMSTEP_FASTER;
    benchmark_result.vectorstep_faster = global_VECTORSTEP_FASTER; 
    benchmark_result.maxFactor = option.maxFactor;

    // tuning - try combinations of different settings and apply these
    if (option.tunelevel) { 
        benchmark_result_t tuning_result = tune(option.tunelevel, option.maxFactor, option.threads, option.blocksize_kB);
        benchmark_result.blockstep_faster  = tuning_result.blockstep_faster;
        benchmark_result.mediumstep_faster = tuning_result.mediumstep_faster;
        benchmark_result.vectorstep_faster = tuning_result.vectorstep_faster;
        benchmark_result.blocksize_bits    = tuning_result.blocksize_bits;
    }

    if (option.blocksize_kB) benchmark_result.blocksize_bits = option.blocksize_kB*1024*8; // overrule all settings with user specified blocksize
    option.explain = 0; // always turn of explain before benchmarking.

    counter_t runs = 0;
    for(counter_t threads=option.threads; threads >= 1 && runs < 2; threads = (threads>>1), runs++ ) {
        verbose(1) {
            printf("Benchmarking... with blocksize %ju settings: %ju/%ju/%ju/%ju/%ju and %ju threads for %.1f seconds - Results: (wait %.1lf seconds)...\n", 
                  (uintmax_t)benchmark_result.blocksize_bits,(uintmax_t)benchmark_result.blockstep_faster, (uintmax_t)benchmark_result.mediumstep_faster, (uintmax_t)benchmark_result.vectorstep_faster, 
                  (uintmax_t)WORD_SIZE_counter, (uintmax_t)VECTOR_ELEMENTS,
                  (uintmax_t)threads, benchmark_result.sample_duration, benchmark_result.sample_duration );
            fflush(stdout);
        }

        // one last check to make sure this is a valid algorithm for these settings
        struct sieve_t* sieve_check = sieve_shake(benchmark_result.maxFactor, global_BLOCKSIZE_BITS);
        int valid = validatePrimeCount(sieve_check);
        sieve_delete(sieve_check);
        if (!valid) { fprintf(stderr, "The sieve is \033[0;31mNOT\033[0m valid for these settings\n"); exit(1); }
        else verbose(3) printf("valid;\n");

        // prepare for a clean result
        benchmark_result.threads = threads;
        benchmark_result.elapsed_time = 0;
        benchmark_result.passes = 0;
        benchmark_result.avg = 0;

        // perform benchmark
        benchmark(&benchmark_result);

        // report results
        char extension[50];
        if (threads > 1)
            sprintf(extension,"-u%juv%jub%jut%ju", (uintmax_t)WORD_SIZE_counter, (uintmax_t)VECTOR_ELEMENTS, (uintmax_t)benchmark_result.blocksize_bits/1024/8,(uintmax_t)threads);
        else
            sprintf(extension,"-u%juv%jub%ju", (uintmax_t)WORD_SIZE_counter, (uintmax_t)VECTOR_ELEMENTS, (uintmax_t)benchmark_result.blocksize_bits/1024/8);
        #ifdef _OPENMP
        printf("rogiervandam_extend_epar%s;%ju;%f;%ju;algorithm=other,faithful=yes,bits=1\n",extension,(uintmax_t)benchmark_result.passes,benchmark_result.elapsed_time,(uintmax_t)threads);
        #else
        printf("rogiervandam_extend%s;%ju;%f;%ju;algorithm=other,faithful=yes,bits=1\n",extension,(uintmax_t)benchmark_result.passes,benchmark_result.elapsed_time,(uintmax_t)threads);
        #endif
        verbose(1) {
            printf("\033[0;32m(Passes - per %.1f seconds: \033[1;33m%f\033[0m - per second \033[1;33m%.1f\033[0;32m)\033[0m\n", option.maxTime, option.maxTime*benchmark_result.passes/benchmark_result.elapsed_time, benchmark_result.passes/benchmark_result.elapsed_time);
            if (option.maxTime!=5.0) printf("\033[0;32m(Passes - per %.1f seconds: \033[1;33m%f\033[0m - per second \033[1;33m%.1f\033[0;32m)\033[0m\n", 5.0, 5.0*benchmark_result.passes/benchmark_result.elapsed_time, benchmark_result.passes/benchmark_result.elapsed_time);
            if (threads>1) printf("\033[0;32m(Passes per thread (total %ju) - per %.1f seconds: %.1f - per second \033[1;33m%.1f\033[0;32m)\033[0m\n", 
                                 (uintmax_t)threads,  option.maxTime, option.maxTime*benchmark_result.passes/benchmark_result.elapsed_time/threads, benchmark_result.passes/benchmark_result.elapsed_time/threads);
        }
        fflush(stdout);
    }

    // show results for --show command line option
    if (option.showMaxFactor > 0) {
        printf("Show result set:\n");
        struct sieve_t* sieve = sieve_shake(option.maxFactor, option.maxFactor);
        show_primes(sieve, option.showMaxFactor);
        sieve_delete(sieve);
    }
}
