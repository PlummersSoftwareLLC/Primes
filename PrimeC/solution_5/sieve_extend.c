// Sieve algorithm by Rogier van Dam - 2022
// Find all primes up to <max int> using the Sieve of Eratosthenes (https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes)

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <string.h>
#if defined(__x86_64__)
// #include <immintrin.h>
#endif
#ifdef _OPENMP
#include <omp.h>
#endif

//set compile_debuggable to 1 to enable explain plan
#define compile_debuggable 0
#if compile_debuggable
#define debug if (compile_debuggable && option.explain)
#else
#define debug if unlikely(0)
#endif
#define verbose(level) if (option.verboselevel >= level)
#define verbose_at(level) if (option.verboselevel == level)

// defaults
#define default_sieve_limit 1000000
#define default_blocksize (32*1024*8)
#define default_maxTime 5
#define default_sample_duration 0.001
#define default_explain_level 0
#define default_verbose_level 0
#define default_tune_level 1
#define default_check_level 1
#define default_show_primes_on_error 100
#define default_showMaxFactor (0 || compile_debuggable?100:0)
#define anticiped_cache_line_bytesize 128

// helper functions
#define pow(base,pow) (pow*((base>>pow)&1U))
#define min(a,b) ((a<b) ? a : b)
#define uintsafeminus(a,b) ((a>b)?(a-b):0)
#define likely(x)       (__builtin_expect((x),1))
#define unlikely(x)     (__builtin_expect((x),0))

// types
#define bitword_t uint64_t
#define TYPE uint64_t
#define counter_t uint64_t
#define bitshift_t TYPE

// masks and mask helpers
#define WORD_SIZE (sizeof(bitword_t)*8)
#define WORD_SIZE_counter ((counter_t)WORD_SIZE)
#define WORD_SIZE_bitshift ((bitshift_t)WORD_SIZE)
#define SHIFT_WORD ((bitshift_t)(pow(WORD_SIZE,1)+pow(WORD_SIZE,2)+pow(WORD_SIZE,3)+pow(WORD_SIZE,4)+pow(WORD_SIZE,5)+pow(WORD_SIZE,6)+pow(WORD_SIZE,7)+pow(WORD_SIZE,8)+pow(WORD_SIZE,9)+pow(WORD_SIZE,10)))
#define VECTOR_ELEMENTS 4
#define VECTOR_SIZE_bytes (sizeof(bitword_t)*VECTOR_ELEMENTS)
#define VECTOR_SIZE_counter ((counter_t)VECTOR_SIZE_bytes*8)
#define VECTOR_SIZE (VECTOR_SIZE_bytes*8)
#define SHIFT_VECTOR ((bitshift_t)(pow(VECTOR_SIZE,1)+pow(VECTOR_SIZE,2)+pow(VECTOR_SIZE,3)+pow(VECTOR_SIZE,4)+pow(VECTOR_SIZE,5)+pow(VECTOR_SIZE,6)+pow(VECTOR_SIZE,7)+pow(VECTOR_SIZE,8)+pow(VECTOR_SIZE,9)+pow(VECTOR_SIZE,10)))

// types (II) - calculated
typedef bitword_t bitvector_t __attribute__ ((vector_size(VECTOR_SIZE_bytes)));

// globals for tuning
// #define SMALLSTEP_FASTER ((counter_t)0)
// #define MEDIUMSTEP_FASTER ((counter_t)16)
// #define VECTORSTEP_FASTER ((counter_t)128)
static counter_t global_SMALLSTEP_FASTER = 0ULL;
static counter_t global_MEDIUMSTEP_FASTER = 48ULL;
static counter_t global_VECTORSTEP_FASTER = 144ULL;
static counter_t global_BLOCKSIZE_BITS = default_blocksize;
#define SMALLSTEP_FASTER ((counter_t)global_SMALLSTEP_FASTER)
#define MEDIUMSTEP_FASTER ((counter_t)global_MEDIUMSTEP_FASTER)
#define VECTORSTEP_FASTER ((counter_t)global_VECTORSTEP_FASTER)
#define BLOCKSIZE_BITS ((counter_t)global_BLOCKSIZE_BITS)

// Patterns based on types
#define SAFE_SHIFTBIT (bitshift_t)1ULL
#define SAFE_ZERO  (bitword_t)0ULL
#define BITWORD_SHIFTBIT (bitword_t)1ULL
#define WORDMASK ((~SAFE_ZERO)>>(WORD_SIZE_bitshift-SHIFT_WORD))
#define VECTORMASK ((~SAFE_ZERO)>>(WORD_SIZE_bitshift-SHIFT_VECTOR))

// helpder functions for word/vector indexing
#define wordindex(index) (((counter_t)index) >> (bitshift_t)SHIFT_WORD)
#define wordend(index) ((counter_t)index|WORDMASK)
#define wordstart(index) ((counter_t)(index)&(counter_t)(~WORDMASK))
#define vectorindex(index) (((counter_t)index) >> (bitshift_t)SHIFT_VECTOR)
#define vectorstart(index) (((counter_t)index) & (counter_t)~VECTORMASK)
#define vectorfromword(word) ((counter_t)(word)>>(SHIFT_VECTOR-SHIFT_WORD))
#define wordinvector(index) ((counter_t)(index) & (counter_t)VECTORMASK)

// modern processors do a & over the shiftssize, so we only have to do that ourselve when using the shiftsize in calculations. 
#define bitindex(index) ((bitshift_t)(index))
#define bitindex_calc(index) ((bitshift_t)(index)&((bitshift_t)(WORDMASK)))
#define markmask(index) (BITWORD_SHIFTBIT << bitindex(index))
#define markmask_calc(index) (BITWORD_SHIFTBIT << bitindex_calc(index))
// #define chopmask(index) ((SAFE_SHIFTBIT << bitindex(index))-SAFE_SHIFTBIT)
#define chopmask(index) (~SAFE_ZERO >> (WORD_SIZE-1-bitindex_calc(index)))
#define keepmask(index) (~SAFE_ZERO << (bitindex_calc(index)))

struct sieve_t {
    bitword_t* bitstorage;
    counter_t  bits;
    counter_t  size;
};

struct options_t {
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

//#include "debugtools.h"

// use cache lines as much as possible - alignment might be key
// moved clearing the sieve with 0 to the sieve_block_extend - it gave weird malloc problems at this point
#define ceiling(x,y) (((x) + (y) - 1) / (y)) // Return the smallest multiple N of y such that:  x <= y * N
static struct sieve_t *sieve_create(counter_t size) 
{
    struct sieve_t *sieve = aligned_alloc(8, sizeof(struct sieve_t));
    sieve->bitstorage = aligned_alloc((size_t)anticiped_cache_line_bytesize, (size_t)ceiling(1+((size_t)size>>1), anticiped_cache_line_bytesize<<3) * anticiped_cache_line_bytesize );
    sieve->bits     = size >> 1;
    sieve->size     = size;

    // code below not needed: only clearing the first word of each block will do the trick
    // for (counter_t index_word = 0; index_word <= wordindex(sieve->bits); index_word++) sieve->bitstorage[index_word] = SAFE_ZERO;
    return sieve;
}

static void sieve_delete(struct sieve_t *sieve) 
{
    free(sieve->bitstorage);
    free(sieve);
}

// search the next bit not set - for small expected distances
static inline counter_t searchBitFalse(bitword_t* bitstorage, register counter_t index) 
{
    while (bitstorage[wordindex(index)] & markmask(index)) index++;
    return index;
}

// not much performance gain at smaller sieves, but its's nice to have an implementation
static inline counter_t searchBitFalse_longRange(bitword_t* bitstorage, register counter_t index)
{
    const bitshift_t index_bit  = bitindex_calc(index);
    register counter_t index_word = wordindex(index);
    register bitshift_t distance = (bitshift_t) __builtin_ctzll( ~(bitstorage[index_word] >> index_bit));  // take inverse to be able to use ctz
    index += distance;
    distance += index_bit;

    while unlikely(distance == WORD_SIZE_bitshift) { // to prevent a bug from optimzer
        index_word++;
        distance = (bitshift_t) __builtin_ctzll(~(bitstorage[index_word]));
        index += distance;
    }

    return index;
}

// apply the same word mask at large ranges
// manually unlooped - this here is where the main speed increase comes from
// idea from PrimeRust/solution_1 by Michael Barber 
static inline void applyMask_word(bitword_t* __restrict bitstorage, const counter_t step, const counter_t range_stop, const bitword_t mask, const counter_t index_word) 
{
#if defined(__x86_64__) // unexplained (yet) 4k/s performance increase for this version on x86_64
    const counter_t range_stop_word = wordindex(range_stop);
    register bitword_t* __restrict index_ptr      =  __builtin_assume_aligned(&bitstorage[index_word],8);

    register bitword_t* __restrict fast_loop_ptr  =  __builtin_assume_aligned(&bitstorage[((range_stop_word>step*5) ? (range_stop_word - step*5):0)],8);
    #pragma GCC ivdep
    while (index_ptr < fast_loop_ptr) {
        *index_ptr |= mask;  index_ptr+=step;
        *index_ptr |= mask;  index_ptr+=step;
        *index_ptr |= mask;  index_ptr+=step;
        *index_ptr |= mask;  index_ptr+=step;
        *index_ptr |= mask;  index_ptr+=step;
    }

   register const bitword_t* __restrict range_stop_ptr = __builtin_assume_aligned(&bitstorage[(range_stop_word)],8);
   #pragma GCC ivdep
   while likely(index_ptr < range_stop_ptr) {
       *index_ptr |= mask;  index_ptr+=step;
   }

   if (index_ptr == range_stop_ptr) { // index_ptr could also end above range_stop_ptr, depending on steps. Then a chop is not needed
      *index_ptr |= (mask & chopmask(range_stop));
   }
#else
    const counter_t range_stop_word = wordindex(range_stop);
    register bitword_t* __restrict index_ptr      =  __builtin_assume_aligned(&bitstorage[index_word],8);
    const counter_t step_4 = step<<2;
    const counter_t length_word = range_stop_word - index_word;
    const counter_t fast_iterations = length_word/step_4;
    #pragma GCC ivdep
    for (counter_t i=fast_iterations; i--; ) {
        *index_ptr |= mask;  index_ptr+=step;
        *index_ptr |= mask;  index_ptr+=step;
        *index_ptr |= mask;  index_ptr+=step;
        *index_ptr |= mask;  index_ptr+=step;
    }

    register const bitword_t* __restrict range_stop_ptr = __builtin_assume_aligned(&bitstorage[range_stop_word],8);
    #pragma GCC ivdep
    for (counter_t i=4; i-- && likely(index_ptr < range_stop_ptr);) { // signal compiler that only <4 iterations are left
        *index_ptr |= mask;  index_ptr+=step; 
    }

    // doing this instead of index_ptr <= above is faster. unexplained. 
    if (index_ptr == range_stop_ptr) { // index_ptr could also end above range_stop_ptr, depending on steps. 
        *index_ptr |= mask; // chop not needed is block-size aligned with word size
    }
#endif
}

// same as word mask, but at a vector level - uses the sse/avx extensions, hopefully
static inline void applyMask_vector(bitvector_t* __restrict bitstorage, const counter_t step, const counter_t range_stop, const bitvector_t mask, counter_t index_vector) 
{
    const counter_t range_stop_vector = vectorindex(range_stop);
    register bitvector_t* __restrict index_ptr      =  __builtin_assume_aligned(&bitstorage[index_vector],anticiped_cache_line_bytesize);
    register bitvector_t* __restrict fast_loop_ptr  =  __builtin_assume_aligned(&bitstorage[((range_stop_vector>step*4) ? (range_stop_vector - step*4):0)],anticiped_cache_line_bytesize);
    
    #pragma GCC ivdep
    while likely(index_ptr < fast_loop_ptr) {
        *index_ptr |= mask; index_ptr+=step;
        *index_ptr |= mask; index_ptr+=step;
        *index_ptr |= mask; index_ptr+=step;
        *index_ptr |= mask; index_ptr+=step;
    }
    
    register const bitvector_t* __restrict range_stop_ptr = __builtin_assume_aligned(&bitstorage[(range_stop_vector)],anticiped_cache_line_bytesize);
    #pragma GCC ivdep
    while likely(index_ptr < range_stop_ptr) {
        *index_ptr |= mask; index_ptr+=step;
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
static void  setBitsTrue_mediumStep(bitword_t* __restrict bitstorage, const counter_t range_start, const counter_t step, const counter_t range_stop) 
{
    const counter_t range_stop_unique =  range_start + WORD_SIZE_counter * step;

    if unlikely(range_stop_unique > range_stop) { // the range will not repeat itself; no need to try to resuse the mask
        debug printf("Setting bits step %ju in %ju bit range (%ju-%ju) using mediumstep-unique (%ju occurances)\n", (uintmax_t)step, (uintmax_t)range_stop-(uintmax_t)range_start,(uintmax_t)range_start,(uintmax_t)range_stop, (uintmax_t)(((uintmax_t)range_stop-(uintmax_t)range_start)/(uintmax_t)step));

        for (register counter_t index = range_start; index <= range_stop;) {
            counter_t index_word = wordindex(index);
            register counter_t index_word_start = wordstart(index);
            register bitword_t mask = SAFE_ZERO;
            for(; index_word_start == wordstart(index); index += step) mask |= markmask(index);
            bitstorage[index_word] |= mask;
        }
    }
    else { // this mask will reoccur at a interval of step words -> fill mask and reapply as a interval of step
        debug printf("Setting bits step %ju in %ju bit range (%ju-%ju) using mediumstep-repeat (%ju occurances)\n", (uintmax_t)step, (uintmax_t)range_stop-(uintmax_t)range_start,(uintmax_t)range_start,(uintmax_t)range_stop, (uintmax_t)(((uintmax_t)range_stop-(uintmax_t)range_start)/(uintmax_t)step));
        
        for (register counter_t index = range_start; index <= range_stop_unique;) {
            counter_t index_word = wordindex(index);
            register counter_t index_word_start = wordstart(index);
            register bitword_t mask = SAFE_ZERO;
            for(; index_word_start == wordstart(index); index += step) mask |= markmask(index);
            applyMask_word(bitstorage, step, range_stop, mask, index_word);
        }
    }
}

// Large ranges (> WORD_SIZE * step) mean the same mask can be reused
static inline void setBitsTrue_largeRange(bitword_t* __restrict bitstorage, const counter_t range_start, const counter_t step, const counter_t range_stop) 
{
    debug printf("Setting bits step %ju in %ju bit range (%ju-%ju) using largerange (%ju occurances)\n", (uintmax_t)step, (uintmax_t)range_stop-(uintmax_t)range_start,(uintmax_t)range_start,(uintmax_t)range_stop, (uintmax_t)(((uintmax_t)range_stop-(uintmax_t)range_start)/(uintmax_t)step));
    const counter_t range_stop_unique =  range_start + WORD_SIZE_counter * step;

    if unlikely(range_stop_unique > range_stop) { // the range will not repeat itself; no need to try to resuse the mask
        for (register counter_t index = range_start; index <= range_stop; index+= step) {
            bitstorage[wordindex(index)] |= markmask(index);
        }
    }
    else {
        for (register counter_t index = range_start; index < range_stop_unique; index += step) {
            applyMask_word(bitstorage, step, range_stop, markmask(index), wordindex(index));
        }
    }
}

static inline void setBitsTrue_largeRange_vector(bitword_t* __restrict bitstorage_word, counter_t range_start, const counter_t step, const counter_t range_stop) 
{
    debug printf("Setting bits step %ju in %ju bit range (%ju-%ju) using largerange vector (%ju occurances)\n", (uintmax_t)step, (uintmax_t)range_stop-(uintmax_t)range_start,(uintmax_t)range_start,(uintmax_t)range_stop, (uintmax_t)(((uintmax_t)range_stop-(uintmax_t)range_start)/(uintmax_t)step));

    counter_t range_start_atvector = vectorstart(range_start);
    if likely(( range_start_atvector + step) < range_start) { // not the first step possible in this vector
        debug printf("..Range start+step %ju not at start of vector %ju\n",(uintmax_t)range_start+(uintmax_t)step, (uintmax_t)range_start_atvector); 

        range_start_atvector += VECTOR_SIZE; // find next vector
        if (unlikely(range_start_atvector > range_stop)) { // we should not be here; just handle without vector
            for (counter_t index = range_start; index <= range_stop; index += step) 
                bitstorage_word[wordindex(index)] |= markmask(index);
            return;
        }

        for (; range_start <= range_start_atvector; range_start += step) 
            bitstorage_word[wordindex(range_start)] |= markmask(range_start);
    }
    
    const counter_t range_stop_unique =  range_start + VECTOR_SIZE_counter * step; 
    if (range_stop_unique > range_stop) { // fallback to other methods
        if (step < WORD_SIZE_counter) setBitsTrue_mediumStep(bitstorage_word, range_start, step, range_stop);
        else setBitsTrue_largeRange(bitstorage_word, range_start, step, range_stop);
        return;
    }

    debug printf("..building masks in range %ju-%ju\n", (uintmax_t)range_start, (uintmax_t)range_stop_unique);
    for (counter_t index = range_start; index < range_stop_unique;) {
        const counter_t current_vector =  vectorindex(index);

        register bitvector_t quadmask = { };
        register bitword_t mask = SAFE_ZERO;
        register const counter_t curent_vector_start = vectorstart(index);

        #pragma GCC ivdep
        for(register counter_t vector_wordstart = (curent_vector_start                        ); wordstart(index) == vector_wordstart; index += step) mask |= markmask(index);
        quadmask[0] = mask; mask = SAFE_ZERO;
        #pragma GCC ivdep
        for(register counter_t vector_wordstart = (curent_vector_start | (WORD_SIZE_counter  )); wordstart(index) == vector_wordstart; index += step) mask |= markmask(index);
        quadmask[1] = mask; mask = SAFE_ZERO;
        #pragma GCC ivdep
        for(register counter_t vector_wordstart = (curent_vector_start | (WORD_SIZE_counter*2)); wordstart(index) == vector_wordstart; index += step) mask |= markmask(index);
        quadmask[2] = mask; mask = SAFE_ZERO;
        #pragma GCC ivdep
        for(register counter_t vector_wordstart = (curent_vector_start | (WORD_SIZE_counter*3)); wordstart(index) == vector_wordstart; index += step) mask |= markmask(index);
        quadmask[3] = mask;

        // use mask on all n*step multiples
        bitvector_t* __restrict bitstorage_vector = __builtin_assume_aligned( (bitvector_t*) bitstorage_word, anticiped_cache_line_bytesize);
        applyMask_vector(bitstorage_vector, step, range_stop, quadmask, current_vector);
    }
}

static void continuePattern_smallSize(bitword_t* __restrict bitstorage, const counter_t source_start, const counter_t size, const counter_t destination_stop)
{
    debug printf("Extending sieve size %ju in %ju bit range (%ju-%ju) using smallsize (%ju copies)\n", (uintmax_t)size, (uintmax_t)destination_stop-(uintmax_t)source_start,(uintmax_t)source_start,(uintmax_t)destination_stop, (uintmax_t)(((uintmax_t)destination_stop-(uintmax_t)source_start)/(uintmax_t)size));

    const counter_t source_word = wordindex(source_start);
    register bitword_t pattern = ((bitstorage[source_word] >> bitindex(source_start)) | (bitstorage[source_word+1] << (WORD_SIZE_counter-bitindex_calc(source_start)))) & chopmask(size);
    for (bitshift_t pattern_size = (bitshift_t)size; pattern_size <= WORD_SIZE_bitshift; pattern_size += pattern_size)
        pattern |= (pattern << pattern_size);

    const counter_t destination_start = source_start + size;
    counter_t destination_start_word = wordindex(destination_start);
    counter_t destination_stop_word = wordindex(destination_stop);
    if (destination_start_word >= destination_stop_word) {
        bitstorage[destination_start_word] |= (pattern << bitindex(destination_start)) & chopmask(destination_stop);
        return;
    }

    bitstorage[destination_start_word] |= (pattern << bitindex(destination_start));

    register const bitshift_t pattern_shift = WORD_SIZE_counter % size;
    register const bitshift_t pattern_size = WORD_SIZE_bitshift - pattern_shift;
    register bitshift_t shift = (WORD_SIZE_bitshift - bitindex_calc(destination_start)) & WORDMASK; // be sure this stays > 0
    register counter_t loop_range = destination_stop_word - destination_start_word;
    destination_start_word++;
    
    #pragma GCC ivdep
    for (counter_t i=0; i<=loop_range; ++i ) {
        bitstorage[destination_start_word+i] = (pattern << (pattern_size - ((shift+i*pattern_shift) & WORDMASK)  ) ) | (pattern >> ((shift+i*pattern_shift) & WORDMASK));
    }
    bitstorage[destination_stop_word] &= chopmask(destination_stop);
}

static void continuePattern_aligned(bitword_t* bitstorage, const counter_t source_start, const counter_t size, const counter_t destination_stop)
{
    debug printf("Extending sieve size %ju in %ju bit range (%ju-%ju) using aligned (%ju copies)\n", (uintmax_t)size, (uintmax_t)destination_stop-(uintmax_t)source_start,(uintmax_t)source_start,(uintmax_t)destination_stop, (uintmax_t)(((uintmax_t)destination_stop-(uintmax_t)source_start)/(uintmax_t)size));

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

}

static void continuePattern_shiftright(bitword_t* bitstorage, const counter_t source_start, const counter_t size, const counter_t destination_stop)
{
    debug printf("Extending sieve size %ju in %ju bit range (%ju-%ju) using shiftright (%ju copies)\n", (uintmax_t)size, (uintmax_t)destination_stop-(uintmax_t)source_start,(uintmax_t)source_start,(uintmax_t)destination_stop, (uintmax_t)(((uintmax_t)destination_stop-(uintmax_t)source_start)/(uintmax_t)size));
   
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

    debug { printf("...start - %ju - %ju - end\n",(uintmax_t)wordindex(copy_start), (uintmax_t)destination_stop_word) ; }

    if (copy_word > source_word + 4) {
        #pragma GCC ivdep
        for (; copy_word <= destination_stop_word; copy_word++, source_word++ ) 
            bitstorage[copy_word] = (bitstorage[source_word] >> shift_flipped) | (bitstorage[source_word+1] << shift);
    }
    else {
        for (; copy_word <= destination_stop_word; copy_word++, source_word++ ) 
            bitstorage[copy_word] = (bitstorage[source_word] >> shift_flipped) | (bitstorage[source_word+1] << shift);
    }
}



static inline counter_t continuePattern_shiftleft_unrolled(bitword_t* __restrict bitstorage, const counter_t aligned_copy_word, const bitshift_t shift, counter_t copy_word, counter_t source_word) 
{
    const counter_t fast_loop_stop_word = (aligned_copy_word>2) ? (aligned_copy_word - 2) : 0; // safe for unsigned ints
    register const bitshift_t shift_flipped = WORD_SIZE_bitshift-shift;
    counter_t distance = 0;

    while (copy_word < fast_loop_stop_word) {
        bitword_t source0 = bitstorage[source_word  ];
        bitword_t source1 = bitstorage[source_word+1];
        bitstorage[copy_word  ] = (source0 >> shift) | (source1 << shift_flipped);
        bitword_t source2 = bitstorage[source_word+2];
        bitstorage[copy_word+1] = (source1 >> shift) | (source2 << shift_flipped);
        copy_word += 2;
        source_word += 2;
        distance += 2;
    }
    return distance;
}

static void continuePattern_shiftleft(bitword_t* bitstorage, const counter_t source_start, const counter_t size, const counter_t destination_stop)
{
    debug printf("Extending sieve size %ju in %ju bit range (%ju-%ju) using shiftleft (%ju copies)\n", (uintmax_t)size, (uintmax_t)destination_stop-(uintmax_t)source_start,(uintmax_t)source_start,(uintmax_t)destination_stop, (uintmax_t)(((uintmax_t)destination_stop-(uintmax_t)source_start)/(uintmax_t)size));

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

    debug { counter_t fast_loop_stop_word = uintsafeminus(aligned_copy_word,2); printf("...start - %ju - end fastloop - %ju - start alignment - %ju - end\n", (uintmax_t)fast_loop_stop_word - (uintmax_t)wordindex(copy_start), (uintmax_t)aligned_copy_word - (uintmax_t)fast_loop_stop_word, (uintmax_t)destination_stop_word - (uintmax_t)aligned_copy_word); }

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


}

// continue a pattern that start at <source_start> with a size of <size>.
// repeat this pattern up to <destination_stop>.
// for small sizes, this is done on a word level
// for larger sizes, we look at the offset / start bit and apply the appropriate algorithm.
// note that these algorithms are general for bitstorage and have no specialized assumptions for the sieve application
static inline void continuePattern(bitword_t* bitstorage, const counter_t source_start, const counter_t size, const counter_t destination_stop)
{
    if (size < WORD_SIZE_counter) return continuePattern_smallSize (bitstorage, source_start, size, destination_stop);

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

    debug printf("Block strip for block %ju - %ju\n",(uintmax_t)block_start,(uintmax_t)block_stop);
    
    while ((prime < maxprime) && (prime*step <= block_stop)) {
        if likely(block_start >= (prime + 1)) 
            start = block_start + prime + prime - ((block_start + prime) % step);
        else 
            start = prime * prime * 2 + prime * 2;

        if unlikely(step < VECTORSTEP_FASTER) {
            setBitsTrue_largeRange_vector(bitstorage, start, step, block_stop);
            prime = searchBitFalse(bitstorage, prime+1 );
        }
        else {
            setBitsTrue_largeRange(bitstorage, start, step, block_stop);
            prime = searchBitFalse_longRange(bitstorage, prime+1 );
        }

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
        counter_t start = prime * prime * 2 + prime * 2;
        if unlikely(start > block_stop) break;

        const counter_t step  = prime * 2 + 1;
        if (block_start >= (prime + 1)) start = block_start + prime + prime - ((block_start + prime) % step);

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

    // retunr the completed sieve
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
    counter_t startprime = sieve_block_stripe(bitstorage, 0, sieve->bits, block.prime, global_SMALLSTEP_FASTER);

    // in the sieve all bits for the multiples of primes up to startprime have been set
    // process the sieve and stripe all the multiples of primes > start_prime
    // do this block by block to minimize cache misses
    for (counter_t block_start = 0,  block_stop = blocksize-1;block_start <= sieve->bits; block_start += blocksize, block_stop += blocksize) {
        if unlikely(block_stop > sieve->bits) block_stop = sieve->bits;
        counter_t prime = searchBitFalse(bitstorage, startprime);
        sieve_block_stripe(bitstorage, block_start, block_stop, prime,maxFactor);
    } 

    // retunr the completed sieve
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
        case 10:            valid_primes = 4;        break;
        case 100:           valid_primes = 25;       break;
        case 1000:          valid_primes = 168;      break;
        case 10000:         valid_primes = 1229;     break;
        case 100000:        valid_primes = 9592;     break;
        case 1000000:       valid_primes = 78498;    break;
        case 10000000:      valid_primes = 664579;   break;
        case 100000000:     valid_primes = 5761455;  break;
        case 1000000000:    valid_primes = 50847534; break;
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
    counter_t smallstep_faster;
    counter_t mediumstep_faster;
    counter_t vectorstep_faster;
    counter_t threads;
    double    sample_duration;
    counter_t passes;
    double    elapsed_time;
    double    avg;
} tuning_result_t;

int compare_tuning_result(const void *a, const void *b) 
{
    tuning_result_t *resultA = (tuning_result_t *)a;
    tuning_result_t *resultB = (tuning_result_t *)b;
    return (resultB->avg > resultA->avg ? 1 : -1);
}

static void benchmark(tuning_result_t* tuning_result) 
{
    counter_t passes = 0;
    double elapsed_time = 0;
    struct timespec start_time,end_time;
    struct sieve_t *sieve;

    global_SMALLSTEP_FASTER = tuning_result->smallstep_faster;
    global_MEDIUMSTEP_FASTER = tuning_result->mediumstep_faster;
    global_VECTORSTEP_FASTER = tuning_result->vectorstep_faster;
    double sample_duration = tuning_result->sample_duration;

    clock_gettime(CLOCK_MONOTONIC,&start_time);
    #ifdef _OPENMP
    omp_set_num_threads(tuning_result->threads);
    #pragma omp parallel reduction(+:passes)
    #endif
    while (elapsed_time <= sample_duration) {
        sieve = sieve_shake(tuning_result->maxFactor, tuning_result->blocksize_bits);
        sieve_delete(sieve);
        clock_gettime(CLOCK_MONOTONIC,&end_time);
        elapsed_time = end_time.tv_sec + end_time.tv_nsec*1e-9 - start_time.tv_sec - start_time.tv_nsec*1e-9;
        passes++;
    }
    tuning_result->passes = passes;
    tuning_result->elapsed_time = elapsed_time;
    tuning_result->avg = passes/elapsed_time;
}

static inline void tuning_result_print(tuning_result_t tuning_result) 
{
    printf("blocksize_bits %10ju; blocksize %4jukB; free_bits %5ju; small %2ju; medium %2ju; vector %3ju; passes %3ju; time %f/%f;average %f\n", 
                            (uintmax_t)tuning_result.blocksize_bits, (uintmax_t)tuning_result.blocksize_kB,(uintmax_t)tuning_result.free_bits,
                            (uintmax_t)tuning_result.smallstep_faster,(uintmax_t)tuning_result.mediumstep_faster,(uintmax_t)tuning_result.vectorstep_faster,
                            (uintmax_t)tuning_result.passes, tuning_result.elapsed_time, tuning_result.sample_duration, tuning_result.avg);
}

static tuning_result_t tune(int tune_level, counter_t maxFactor, counter_t threads, counter_t option_blocksize_kB) 
{
    counter_t best_blocksize_bits = default_blocksize;

    double best_avg = 0;
    best_blocksize_bits = 0;
    counter_t best_smallstep_faster = 0;
    counter_t best_mediumstep_faster = 0;
    counter_t best_vectorstep_faster = 0;
    counter_t smallstep_faster_steps = 4;
    counter_t mediumstep_faster_steps = 4;
    counter_t vectorstep_faster_steps = 32;
    counter_t freebits_steps = anticiped_cache_line_bytesize;
    double sample_duration = default_sample_duration;

    // determines the size of the resultset
    switch (tune_level) {
        case 1:
            smallstep_faster_steps  = WORD_SIZE/2;
            mediumstep_faster_steps = WORD_SIZE/4;
            vectorstep_faster_steps = WORD_SIZE/2;
            freebits_steps = anticiped_cache_line_bytesize*8*2;
            sample_duration = 0.001;
            break;
        case 2:
            smallstep_faster_steps  = WORD_SIZE/4;
            mediumstep_faster_steps = WORD_SIZE/16;
            vectorstep_faster_steps = WORD_SIZE/4;
            freebits_steps = anticiped_cache_line_bytesize*8;
            sample_duration = 0.002;
            break;
        case 3:
            smallstep_faster_steps  = WORD_SIZE/16;
            mediumstep_faster_steps = WORD_SIZE/16;
            vectorstep_faster_steps = WORD_SIZE/16;
            freebits_steps = anticiped_cache_line_bytesize/2;
            sample_duration = 0.004;
            break;
    }
    
    verbose(1) { 
        verbose(2) printf("\n");
        printf("Tuning..."); 
        verbose(2) printf(".. best options (shown when found):\n");
        fflush(stdout);
    }
    const size_t max_results = ((size_t)(VECTOR_SIZE_counter/smallstep_faster_steps)+1) * ((size_t)(VECTOR_SIZE_counter/mediumstep_faster_steps)+1) * ((size_t)(VECTOR_SIZE_counter/vectorstep_faster_steps)+1) * 32 * (size_t)(anticiped_cache_line_bytesize*8*4/freebits_steps);
    tuning_result_t* tuning_result = malloc(max_results * sizeof(tuning_result));
    counter_t tuning_results=0;
    counter_t tuning_result_index=0;
    
    for (counter_t smallstep_faster = 0; smallstep_faster <= VECTOR_SIZE_counter; smallstep_faster += smallstep_faster_steps) {
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
                        tuning_result[tuning_result_index].smallstep_faster = smallstep_faster;
                        tuning_result[tuning_result_index].mediumstep_faster = mediumstep_faster;
                        tuning_result[tuning_result_index].vectorstep_faster = vectorstep_faster;
                        tuning_result[tuning_result_index].threads = threads;
                        benchmark(&tuning_result[tuning_result_index]);

                        if ( tuning_result[tuning_result_index].avg > best_avg) {
                            best_avg = tuning_result[tuning_result_index].avg;
                            best_blocksize_bits = blocksize_bits;
                            best_smallstep_faster = smallstep_faster;
                            best_mediumstep_faster = mediumstep_faster;
                            best_vectorstep_faster = vectorstep_faster;
                            verbose(2) { printf(".(<)"); tuning_result_print(tuning_result[tuning_result_index]); fflush(stdout); }
                        }
                        if (option.verboselevel >= 3) { printf("...."); tuning_result_print(tuning_result[tuning_result_index]); }
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
        printf("Finished scan of \033[1;33m%ju\033[0m options. Inital best blocksize: %ju; best smallstep %ju; best mediumstep %ju; best vectorstep %ju\n",(uintmax_t)tuning_results,(uintmax_t)best_blocksize_bits, (uintmax_t)best_smallstep_faster,(uintmax_t)best_mediumstep_faster, (uintmax_t)best_vectorstep_faster);
        printf("Finding the best option by reevaluating the top options with a longer sample duration.\n");
    }

    counter_t tuning_results_max = tuning_results; // keep this value for verbose messages
    for (counter_t step=0; tuning_results>4; step++) {
        qsort(tuning_result, (size_t)tuning_results, sizeof(tuning_result_t), compare_tuning_result);
        verbose(2) {
            tuning_result_index = 0;
            printf("\r(iteration %1ju) ",(uintmax_t)step); tuning_result_print(tuning_result[tuning_result_index]); fflush(stdout);
            verbose(3) {
                for (counter_t tuning_result_index=0; tuning_result_index<min(40,tuning_results); tuning_result_index++) {
                    printf("..."); tuning_result_print(tuning_result[tuning_result_index]);
                }
            }
        }

        tuning_results = tuning_results / 4;
        sample_duration *= 2;

        for (counter_t i=0; i<tuning_results; i++) {
            tuning_result[i].sample_duration = sample_duration;
            verbose(1) { printf("\rTuning...found %ju options..benchmarking step %ju - tuning %3ju options in %lf seconds",(uintmax_t)tuning_results_max,(uintmax_t)step,i, (double)tuning_results*sample_duration); fflush(stdout); }
            benchmark(&tuning_result[i]);
        }
    }

    // take best result
    tuning_result_t best_result = tuning_result[0];
    free(tuning_result);
    verbose(1) {
        printf("\33[2K\r");
        verbose(2) { printf("Best result:  "); tuning_result_print(best_result); printf("\n"); }
    }
    return best_result;
}

void usage(char *name) 
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

struct options_t parseCommandLine(int argc, char *argv[], struct options_t option)
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
            if (sscanf(argv[arg], "%ju", &option.blocksize_kB) != 1) {
                fprintf(stderr, "Error: Invalid size in kilobyte: %s\n", argv[arg]); usage(argv[0]);
            }
            counter_t sieve_bits = option.maxFactor >> 1;
            if ((option.blocksize_kB*1024*8) > (sieve_bits)) option.blocksize_kB = (sieve_bits / (1024*8))+1;
            verbose(1) printf("Blocksize set to %ju kB\n",option.blocksize_kB);
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
            verbose(1) printf("Blocksize corrected to %ju kB\n",option.blocksize_kB);
        }

    }
    return option;
}

struct options_t setDefaultOptions() {
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
void explainSieveShake() 
{
    struct sieve_t* sieve = sieve_shake(option.maxFactor, default_blocksize);
    printf("\nResult set:\n");
    show_primes(sieve, option.showMaxFactor);
    int valid = validatePrimeCount(sieve);
    if (!valid) printf("The sieve is NOT valid...\n");
    else printf("The sieve is VALID\n");
    sieve_delete(sieve);
    exit(0);
}
#endif

void checkSieveAlgorithm()
{
    verbose(1) { 
        printf("Validating..."); 
        verbose(2) printf("\n");
        fflush(stdout); 
    }

    // validate algorithm - run one time for all sizes
    for (counter_t sieveSize_check = 100; sieveSize_check <= 10000000; sieveSize_check *=10) {
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
                printf("Settings used: blocksize %ju, %ju/%ju/%ju\n",blocksize_bits,global_SMALLSTEP_FASTER,global_MEDIUMSTEP_FASTER,global_VECTORSTEP_FASTER);
                exit(1); 
            }
            else verbose(3) printf("valid\n");
        }
        verbose(2) printf("valid\n");
    }
    verbose(1) printf("valid algorithm\n");
}

int main(int argc, char *argv[]) 
{
    option = setDefaultOptions();
    option = parseCommandLine(argc, argv, option);
    
    verbose(1) {
        printf("\nRunning sieve algorithm by Rogier van Dam with the following target:\n");
        printf("Count all primes up to \033[1;33m%ju\033[0m using the sieve of Eratosthenes\n", option.maxFactor);
        printf("\n");
    }

    #if compile_debuggable
    if (option.explain==1) explainSieveShake();
    #endif
    
    // command line --check can be used to check the algorithm for all sieve/blocksize combinations
    if (option.check) checkSieveAlgorithm(); 

    tuning_result_t benchmark_result;
    benchmark_result.sample_duration   = option.maxTime;
    benchmark_result.blocksize_bits    = global_BLOCKSIZE_BITS;
    benchmark_result.smallstep_faster  = global_SMALLSTEP_FASTER;
    benchmark_result.mediumstep_faster = global_MEDIUMSTEP_FASTER;
    benchmark_result.vectorstep_faster = global_VECTORSTEP_FASTER; 
    benchmark_result.maxFactor = option.maxFactor;

    // tuning - try combinations of different settings and apply these
    if (option.tunelevel) { 
        tuning_result_t tuning_result = tune(option.tunelevel, option.maxFactor, option.threads, option.blocksize_kB);
        benchmark_result.smallstep_faster  = tuning_result.smallstep_faster;
        benchmark_result.mediumstep_faster = tuning_result.mediumstep_faster;
        benchmark_result.vectorstep_faster = tuning_result.vectorstep_faster;
        benchmark_result.blocksize_bits    = tuning_result.blocksize_bits;
    }

    if (option.blocksize_kB) benchmark_result.blocksize_bits = option.blocksize_kB*1024*8; // overrule all settings with user specified blocksize
    option.explain = 0; // always turn of explain before benchmarking.

    for(counter_t threads=option.threads; threads >= 1; threads = (threads>>1) ) {
        verbose(1) {
            printf("Benchmarking... with blocksize %ju steps: %ju/%ju/%ju and %ju threads for %.1f seconds - Results: (wait %.1lf seconds)...\n", 
                  (uintmax_t)benchmark_result.blocksize_bits,(uintmax_t)benchmark_result.smallstep_faster, (uintmax_t)benchmark_result.mediumstep_faster, (uintmax_t)benchmark_result.vectorstep_faster, (uintmax_t)threads, benchmark_result.sample_duration, benchmark_result.sample_duration );
            fflush(stdout);
        }

        // one last check to make sure this is a valid algorithm for these settings
        struct sieve_t* sieve_check = sieve_shake(benchmark_result.maxFactor, global_BLOCKSIZE_BITS);
        int valid = validatePrimeCount(sieve_check);
        sieve_delete(sieve_check);
        if (!valid) { fprintf(stderr, "The sieve is not valid for these settings\n"); exit(1); }
        else verbose(3) printf("valid;\n");

        // prepare for a clean result
        benchmark_result.threads = threads;
        benchmark_result.elapsed_time = 0;
        benchmark_result.passes = 0;
        benchmark_result.avg = 0;

        // perform benchmark
        benchmark(&benchmark_result);

        // report results
        #ifdef _OPENMP
        printf("rogiervandam_extend_epar;%ju;%f;%ju;algorithm=other,faithful=yes,bits=1\n", (uintmax_t)benchmark_result.passes,benchmark_result.elapsed_time,(uintmax_t)threads);
        #else
        printf("rogiervandam_extend;%ju;%f;%ju;algorithm=other,faithful=yes,bits=1\n", (uintmax_t)benchmark_result.passes,benchmark_result.elapsed_time,(uintmax_t)threads);
        #endif
        verbose(1) {
            printf("\033[0;32m(Passes - per %.1f seconds: \033[1;33m%f\033[0m - per second \033[1;33m%.1f\033[0;32m)\033[0m\n", option.maxTime, option.maxTime*benchmark_result.passes/benchmark_result.elapsed_time, benchmark_result.passes/benchmark_result.elapsed_time);
            if (threads>1) printf("\033[0;32m(Passes per thread (total %ju) - per %.1f seconds: %.1f - per second \033[1;33m%.1f\033[0;32m)\033[0m\n", 
                                 (uintmax_t)threads,  option.maxTime, option.maxTime*benchmark_result.passes/benchmark_result.elapsed_time/threads, benchmark_result.passes/benchmark_result.elapsed_time/threads);
            fflush(stdout);
        }
    }

    // show results for --show command line option
    if (option.showMaxFactor > 0) {
        printf("Show result set:\n");
        struct sieve_t* sieve = sieve_shake(option.maxFactor, option.maxFactor);
        show_primes(sieve, option.showMaxFactor);
        sieve_delete(sieve);
    }
}
