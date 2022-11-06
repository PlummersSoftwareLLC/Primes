// Sieve algorithm by Rogier van Dam - 2022
// Find all primes up to <max int> using the Sieve of Eratosthenes (https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes)

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <math.h>
#include <string.h>
#ifdef _OPENMP
#include <omp.h>
#endif

//add debug in front of a line to only compile it if the value below is set to 1 (or !=0)
#define option_runonce 0
#define debug if (option_runonce)
#define verbose(level) if (option_verboselevel >= level)
#define verbose_at(level) if (option_verboselevel == level)

// defaults
#define default_sieve_limit 1000000
#define default_blocksize (32*1024*8)
#define default_max_time 5
#define default_sample_duration 0.1
#define default_sample_max 5
#define default_verbose_level 0
#define default_tune_level 1
#define default_check_level 1
#define default_show_primes_on_error 100
#define default_showMaxFactor (0 || option_runonce?100:0)
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
#define counter_t TYPE
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
counter_t global_SMALLSTEP_FASTER = 0;
counter_t global_MEDIUMSTEP_FASTER = 32;
counter_t global_VECTORSTEP_FASTER = 96;
#define SMALLSTEP_FASTER ((counter_t)global_SMALLSTEP_FASTER)
#define MEDIUMSTEP_FASTER ((counter_t)global_MEDIUMSTEP_FASTER)
#define VECTORSTEP_FASTER ((counter_t)global_VECTORSTEP_FASTER)

// Patterns based on types
#define SAFE_SHIFTBIT (bitshift_t)1U
#define SAFE_ZERO  (bitword_t)0U
#define BITWORD_SHIFTBIT (bitword_t)1U
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

//#include "debugtools.h"

// use cache lines as much as possible - alignment might be key
// moved clearing the sieve with 0 to the sieve_block_extend - it gave weird malloc problems at this point
#define ceiling(x,y) (((x) + (y) - 1) / (y)) // Return the smallest multiple N of y such that:  x <= y * N
static struct sieve_t *sieve_create(counter_t size) {
    struct sieve_t *sieve = aligned_alloc(8, sizeof(struct sieve_t));
    sieve->bitstorage = aligned_alloc((size_t)anticiped_cache_line_bytesize, (size_t)ceiling(1+((size_t)size>>1), anticiped_cache_line_bytesize<<3) * anticiped_cache_line_bytesize );
    sieve->bits     = size >> 1;
    sieve->size     = size;
    return sieve;
}

static void sieve_delete(struct sieve_t *sieve) {
    free(sieve->bitstorage);
    free(sieve);
}

// search the next bit not set - for small expected distances
static inline counter_t searchBitFalse(bitword_t* bitstorage, register counter_t index) {
    while (bitstorage[wordindex(index)] & markmask(index)) index++;
    return index;
}

// not much performance gain at smaller sieves, but its's nice to have an implementation
static inline counter_t searchBitFalse_longRange(bitword_t* bitstorage, register counter_t index) {
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
static inline void applyMask_word(bitword_t* __restrict bitstorage, const counter_t step, const counter_t range_stop, const bitword_t mask, counter_t index_word) {
   const counter_t range_stop_word = wordindex(range_stop);
   register bitword_t* __restrict index_ptr      =  __builtin_assume_aligned(&bitstorage[index_word],8);
   register bitword_t* __restrict fast_loop_ptr  =  __builtin_assume_aligned(&bitstorage[((range_stop_word>step*5) ? (range_stop_word - step*5):0)],8);

   //#pragma GCC unroll 10
   #pragma GCC ivdep
   while likely(index_ptr < fast_loop_ptr) {
       *index_ptr |= mask;  index_ptr+=step;
       *index_ptr |= mask;  index_ptr+=step;
       *index_ptr |= mask;  index_ptr+=step;
       *index_ptr |= mask;  index_ptr+=step;
       *index_ptr |= mask;  index_ptr+=step;
   }

   register const bitword_t* __restrict range_stop_ptr = __builtin_assume_aligned(&bitstorage[(range_stop_word)],8);
   while likely(index_ptr < range_stop_ptr) {
       *index_ptr |= mask;  index_ptr+=step;
   }

   if (index_ptr == range_stop_ptr) { // index_ptr could also end above range_stop_ptr, depending on steps. Then a chop is not needed
      *index_ptr |= (mask & chopmask(range_stop));
   }
}

// same as word mask, but at a vector level - uses the sse/avx extensions, hopefully
static inline void applyMask_vector(bitvector_t* __restrict bitstorage, const counter_t step, const counter_t range_stop, const bitvector_t mask, counter_t index_vector) {
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
    while likely(index_ptr <= range_stop_ptr) {
        *index_ptr |= mask; index_ptr+=step;
    }
}

// Medium steps could be within the same word (e.g. less than 64 bits apart).
// By joining the masks and then writing to memory, we might save some time.
// This is especially true for small steps over long ranges
// but it needs tuning, because there is some overhead of checking if the next step is in the same word
static void  setBitsTrue_mediumStep(bitword_t* __restrict bitstorage, const counter_t range_start, const counter_t step, const counter_t range_stop) {
    const counter_t range_stop_unique =  range_start + WORD_SIZE_counter * step;

    if (range_stop_unique > range_stop) { // the range will not repeat itself; no need to try to resuse the mask
        debug printf("Setting bits step %ju in %ju bit range (%ju-%ju) using mediumstep-unique (%ju occurances)\n", (uintmax_t)step, (uintmax_t)range_stop-(uintmax_t)range_start,(uintmax_t)range_start,(uintmax_t)range_stop, (uintmax_t)(((uintmax_t)range_stop-(uintmax_t)range_start)/(uintmax_t)step));

        for (register counter_t index = range_start; index <= range_stop;) {
            register counter_t index_word = wordindex(index);
            register bitword_t mask = SAFE_ZERO;
            for(; index_word == wordindex(index); index += step) mask |= markmask(index);
            bitstorage[index_word] |= mask;
        }
    }
    else { // this mask will reoccur at a interval of step words -> fill mask and reapply as a interval of step
        debug printf("Setting bits step %ju in %ju bit range (%ju-%ju) using mediumstep-repeat (%ju occurances)\n", (uintmax_t)step, (uintmax_t)range_stop-(uintmax_t)range_start,(uintmax_t)range_start,(uintmax_t)range_stop, (uintmax_t)(((uintmax_t)range_stop-(uintmax_t)range_start)/(uintmax_t)step));
        
        for (register counter_t index = range_start; index <= range_stop_unique;) {
            register counter_t index_word = wordindex(index);
            register bitword_t mask = SAFE_ZERO;
            for(; index_word == wordindex(index); index += step) mask |= markmask(index);
            applyMask_word(bitstorage, step, range_stop, mask, index_word);
        }
    }
}

// Large ranges (> WORD_SIZE * step) mean the same mask can be reused
static inline void setBitsTrue_largeRange(bitword_t* __restrict bitstorage, const counter_t range_start, const counter_t step, const counter_t range_stop) {
    debug printf("Setting bits step %ju in %ju bit range (%ju-%ju) using largerange (%ju occurances)\n", (uintmax_t)step, (uintmax_t)range_stop-(uintmax_t)range_start,(uintmax_t)range_start,(uintmax_t)range_stop, (uintmax_t)(((uintmax_t)range_stop-(uintmax_t)range_start)/(uintmax_t)step));
    const counter_t range_stop_unique =  range_start + WORD_SIZE_counter * step;

    #pragma GCC ivdep
    for (register counter_t index = range_start; index < range_stop_unique; index += step) {
        applyMask_word(bitstorage, step, range_stop, markmask(index), wordindex(index));
    }
}

static inline void setBitsTrue_largeRange_vector(bitword_t* __restrict bitstorage_word, counter_t range_start, const counter_t step, const counter_t range_stop) {
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
    if (range_stop_unique >= range_stop) {
        setBitsTrue_largeRange(bitstorage_word, range_start, step, range_stop);
        return;
    }

    debug printf("..building masks in range %ju-%ju\n", (uintmax_t)range_start, (uintmax_t)range_stop_unique);
    for (counter_t index = range_start; index < range_stop_unique;) {
        const counter_t current_vector =  vectorindex(index);
        const counter_t current_vector_start_word =  current_vector << (SHIFT_VECTOR - SHIFT_WORD);
        register counter_t word = wordindex(index) - current_vector_start_word;
        register bitvector_t quadmask = { };

        // build a quadmask
        #pragma GCC ivdep 
        do {
            quadmask[word] |= markmask(index);
            index += step;
            word = wordindex(index) - current_vector_start_word;
        } while (word < (VECTOR_ELEMENTS));// && index <= range_stop_unique);

        // use mask on all n*step multiples
        // register counter_t current_vector = current_vector_start_word >> (SHIFT_VECTOR - SHIFT_WORD);
        bitvector_t* __restrict bitstorage_vector = __builtin_assume_aligned( (bitvector_t*) bitstorage_word, anticiped_cache_line_bytesize);
        applyMask_vector(bitstorage_vector, step, range_stop, quadmask, current_vector);
    }
}

static void continuePattern_smallSize(bitword_t* __restrict bitstorage, const counter_t source_start, const counter_t size, const counter_t destination_stop) {
    debug printf("Extending sieve size %ju in %ju bit range (%ju-%ju) using smallsize (%ju copies)\n", (uintmax_t)size, (uintmax_t)destination_stop-(uintmax_t)source_start,(uintmax_t)source_start,(uintmax_t)destination_stop, (uintmax_t)(((uintmax_t)destination_stop-(uintmax_t)source_start)/(uintmax_t)size));
    // debug { printf("...At start. "); dump_bitstorage(bitstorage, 4); }

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

static void continuePattern_aligned(bitword_t* bitstorage, const counter_t source_start, const counter_t size, const counter_t destination_stop) {
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

static void continuePattern_shiftright(bitword_t* bitstorage, const counter_t source_start, const counter_t size, const counter_t destination_stop) {
    debug printf("Extending sieve size %ju in %ju bit range (%ju-%ju) using shiftright (%ju copies)\n", (uintmax_t)size, (uintmax_t)destination_stop-(uintmax_t)source_start,(uintmax_t)source_start,(uintmax_t)destination_stop, (uintmax_t)(((uintmax_t)destination_stop-(uintmax_t)source_start)/(uintmax_t)size));
   
    const counter_t destination_stop_word = wordindex(destination_stop);
    const counter_t copy_start = source_start + size;
    register const bitshift_t shift = bitindex_calc(copy_start) - bitindex_calc(source_start);
    register const bitshift_t shift_flipped = WORD_SIZE_bitshift-shift;
    register counter_t source_word = wordindex(source_start);
    register counter_t copy_word = wordindex(copy_start);

    bitstorage[copy_word] |= ((bitstorage[source_word] << shift) |  (bitstorage[copy_word] >> shift_flipped)) & keepmask(copy_start);
    if unlikely(++copy_word > destination_stop_word) return; // rapid exit for one word variant

    debug { printf("...start - %ju - %ju - end\n",(uintmax_t)wordindex(copy_start), (uintmax_t)destination_stop_word) ; }

    for (; copy_word <= destination_stop_word; ++copy_word, ++source_word ) {
        bitstorage[copy_word] = (bitstorage[source_word] >> shift_flipped) | (bitstorage[source_word+1] << shift);
    }
}


static inline counter_t continuePattern_shiftleft_unrolled(bitword_t* __restrict bitstorage, const counter_t aligned_copy_word, const bitshift_t shift, counter_t copy_word, counter_t source_word) {
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

static void continuePattern_shiftleft(bitword_t* bitstorage, const counter_t source_start, const counter_t size, const counter_t destination_stop) {
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

    #pragma GCC ivdep
    for (;copy_word <= aligned_copy_word; copy_word++,source_word++) {
        bitstorage[copy_word] = (bitstorage[source_word  ] >> shift) | (bitstorage[source_word+1 ] << shift_flipped);
    }

    if (copy_word >= destination_stop_word) return;

    source_word = copy_word - size; // recalibrate
    const size_t memsize = (size_t)size*sizeof(bitword_t);

    #pragma GCC ivdep
    for (;copy_word + size <= destination_stop_word; copy_word += size) 
        memcpy(&bitstorage[copy_word], &bitstorage[source_word],memsize );

    #pragma GCC ivdep
    for (;copy_word <= destination_stop_word;  copy_word++,source_word++)
        bitstorage[copy_word] = bitstorage[source_word];


 }

// continue a pattern that start at <source_start> with a size of <size>.
// repeat this pattern up to <destination_stop>.
// for small sizes, this is done on a word level
// for larger sizes, we look at the offset / start bit and apply the appropriate algorithm.
// note that these algorithms are general for bitstorage and have no specialized assumptions for the sieve application
static inline void continuePattern(bitword_t* bitstorage, const counter_t source_start, const counter_t size, const counter_t destination_stop)	{
    if (size < WORD_SIZE_counter) return continuePattern_smallSize (bitstorage, source_start, size, destination_stop);

    const bitshift_t copy_bit   = bitindex_calc(source_start + size);
    const bitshift_t source_bit = bitindex_calc(source_start);

    if      (source_bit > copy_bit) continuePattern_shiftleft (bitstorage, source_start, size, destination_stop);
    else if (source_bit < copy_bit) continuePattern_shiftright(bitstorage, source_start, size, destination_stop);
    else                            continuePattern_aligned   (bitstorage, source_start, size, destination_stop);
}

static void sieve_block_stripe(bitword_t* bitstorage, const counter_t block_start, const counter_t block_stop, const counter_t prime_start) {
    counter_t prime = prime_start;
    counter_t start = 0;
    counter_t step  = prime * 2 + 1;

    debug printf("Block strip for block %ju - %ju\n",(uintmax_t)block_start,(uintmax_t)block_stop);
    
    while (prime*step <= block_stop) {
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
static struct block sieve_block_extend(struct sieve_t *sieve, const counter_t block_start, const counter_t block_stop) {
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


/* This is the main module that does all the work*/
static struct sieve_t* sieve_shake(const counter_t maxints, const counter_t blocksize) {
    struct sieve_t *sieve = sieve_create(maxints);
    bitword_t* bitstorage = sieve->bitstorage;

    debug printf("Running sieve to find all primes up to %ju with blocksize %ju\n",(uintmax_t)maxints,(uintmax_t)blocksize);

    // fill the whole sieve bij adding en copying incrementally
    struct block block = sieve_block_extend(sieve, 0, sieve->bits);

    // returns the max prime that was processed in the pattern
    counter_t startprime = block.prime;

    // continue the found pattern to the entire sieve
    continuePattern(bitstorage, block.pattern_start, block.pattern_size, sieve->bits);

    // in the sieve all bits for the multiples of primes up to startprime have been set
    // process the sieve and stripe all the multiples of primes > start_prime
    // do this block by block to minimize cache misses
    for (counter_t block_start = 0,  block_stop = blocksize-1;block_start <= sieve->bits; block_start += blocksize, block_stop += blocksize) {
        if unlikely(block_stop > sieve->bits) block_stop = sieve->bits;
        counter_t prime = searchBitFalse(bitstorage, startprime);
        sieve_block_stripe(bitstorage, block_start, block_stop, prime);
    } 

    // retunr the completed sieve
    return sieve;
}

static void show_primes(struct sieve_t *sieve, counter_t maxFactor) {
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

static void deepAnalyzePrimes(struct sieve_t *sieve) {
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


int validatePrimeCount(struct sieve_t *sieve, int option_verboselevel) {

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
    if (valid  && option_verboselevel >= 4) printf("Result: Sievesize %ju is expected to have %ju primes. algorithm produced %ju primes\n",(uintmax_t)sieve->size,(uintmax_t)valid_primes,(uintmax_t)primecount );
    if (!valid && option_verboselevel >= 1) {
        printf("No valid result. Sievesize %ju was expected to have %ju primes, but algorithm produced %ju primes\n",(uintmax_t)sieve->size,(uintmax_t)valid_primes,(uintmax_t)primecount );
        if (!valid && option_verboselevel >= 2) show_primes(sieve, default_show_primes_on_error);
    }
    if (!valid && option_verboselevel >= 2) deepAnalyzePrimes(sieve);
    return (valid);
}

void usage(char *name) {
    fprintf(stderr, "Usage: %s [options] [maximum]\n", name);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  --check            check the correctness of the algorithm\n");
    fprintf(stderr, "  --help             This help function\n");
    fprintf(stderr, "  --show <maximum>   Show the primes found up to the maximum\n");
    #ifdef _OPENMP
    fprintf(stderr, "  --threads <count>  Set the maximum number of threads to be used\n");
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

typedef struct  {
    counter_t maxints;
    counter_t blocksize_bits;
    counter_t blocksize_kb;
    counter_t free_bits;
    counter_t smallstep_faster;
    counter_t mediumstep_faster;
    counter_t vectorstep_faster;
    counter_t threads;
    counter_t sample_max;
    double    sample_duration;
    counter_t passes;
    double    elapsed_time;
    double    avg;
} tuning_result_type;

int compare_tuning_result(const void *a, const void *b) {
    tuning_result_type *resultA = (tuning_result_type *)a;
    tuning_result_type *resultB = (tuning_result_type *)b;
    return (resultB->avg > resultA->avg ? 1 : -1);
}

static void tune_benchmark(tuning_result_type* tuning_result, counter_t tuning_result_index) {
    counter_t passes = 0;
    double elapsed_time = 0;
    struct timespec start_time,end_time;
    struct sieve_t *sieve;

    global_SMALLSTEP_FASTER = tuning_result[tuning_result_index].smallstep_faster;
    global_MEDIUMSTEP_FASTER = tuning_result[tuning_result_index].mediumstep_faster;
    global_VECTORSTEP_FASTER = tuning_result[tuning_result_index].vectorstep_faster;

    clock_gettime(CLOCK_MONOTONIC,&start_time);
    #ifdef _OPENMP
    omp_set_num_threads(tuning_result[tuning_result_index].threads);
    #pragma omp parallel reduction(+:passes)
    #endif
    while (elapsed_time <= tuning_result[tuning_result_index].sample_duration && passes < tuning_result[tuning_result_index].sample_max) {
        sieve = sieve_shake(tuning_result[tuning_result_index].maxints, tuning_result[tuning_result_index].blocksize_bits);//blocksize_bits);
        sieve_delete(sieve);
        passes++;
        clock_gettime(CLOCK_MONOTONIC,&end_time);
        elapsed_time = end_time.tv_sec + end_time.tv_nsec*1e-9 - start_time.tv_sec - start_time.tv_nsec*1e-9;
    }
    tuning_result[tuning_result_index].passes = passes;
    tuning_result[tuning_result_index].elapsed_time = elapsed_time;
    tuning_result[tuning_result_index].avg = passes/elapsed_time;
}

static inline void tuning_result_print(tuning_result_type tuning_result) {
    printf("blocksize_bits %10ju; blocksize %4jukb; free_bits %5ju; small %2ju; medium %2ju; vector %3ju; passes %3ju/%3ju; time %f/%f;average %f\n", 
                            (uintmax_t)tuning_result.blocksize_bits, (uintmax_t)tuning_result.blocksize_kb,(uintmax_t)tuning_result.free_bits,
                            (uintmax_t)tuning_result.smallstep_faster,(uintmax_t)tuning_result.mediumstep_faster,(uintmax_t)tuning_result.vectorstep_faster,
                            (uintmax_t)tuning_result.passes, (uintmax_t) tuning_result.sample_max,
                            tuning_result.elapsed_time, tuning_result.sample_duration, tuning_result.avg);
}

static tuning_result_type tune(int tune_level, counter_t maxints, counter_t threads, int option_verboselevel) {
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
    counter_t sample_max = default_sample_max;
    double sample_duration = default_sample_duration;

    // determines the size of the resultset
    switch (tune_level) {
        case 1:
            smallstep_faster_steps  = WORD_SIZE/4;
            mediumstep_faster_steps = WORD_SIZE/4;
            vectorstep_faster_steps = WORD_SIZE/4;
            freebits_steps = anticiped_cache_line_bytesize*8*2;
            sample_max = 16;
            sample_duration = 0.1;
            break;
        case 2:
            smallstep_faster_steps  = WORD_SIZE/8;
            mediumstep_faster_steps = WORD_SIZE/16;
            vectorstep_faster_steps = WORD_SIZE/4;
            freebits_steps = anticiped_cache_line_bytesize*8;
            sample_max = 16;
            sample_duration = 0.2;
            break;
        case 3:
            smallstep_faster_steps  = WORD_SIZE/16;
            mediumstep_faster_steps = WORD_SIZE/16;
            vectorstep_faster_steps = WORD_SIZE/16;
            freebits_steps = anticiped_cache_line_bytesize/2;
            sample_max = 16;
            sample_duration = 0.2;
            break;
    }
    
    verbose(2) printf("\n");
    verbose_at(1) { printf("Tuning..."); fflush(stdout); }
    verbose(2) printf("\n");
    const size_t max_results = ((size_t)(WORD_SIZE_counter/smallstep_faster_steps)+1) * ((size_t)(WORD_SIZE_counter/mediumstep_faster_steps)+1) * ((size_t)(VECTOR_SIZE_counter/vectorstep_faster_steps)+1) * 32 * (size_t)(anticiped_cache_line_bytesize*8*4/freebits_steps);
    tuning_result_type* tuning_result = malloc(max_results * sizeof(tuning_result));
    counter_t tuning_results=0;
    counter_t tuning_result_index=0;
    
    for (counter_t smallstep_faster = 0; smallstep_faster <= 0; smallstep_faster += smallstep_faster_steps) {
        for (counter_t mediumstep_faster = smallstep_faster; mediumstep_faster <= WORD_SIZE_counter; mediumstep_faster += mediumstep_faster_steps) {
            for (counter_t vectorstep_faster = mediumstep_faster; vectorstep_faster <= VECTOR_SIZE_counter; vectorstep_faster += vectorstep_faster_steps) {
                for (counter_t blocksize_kb=256; blocksize_kb>=8; blocksize_kb /= 2) {
                    for (counter_t free_bits=0; (free_bits < (anticiped_cache_line_bytesize*8*4) && (free_bits < blocksize_kb * 1024 * 8)); free_bits += freebits_steps) {
                        counter_t blocksize_bits = (blocksize_kb * 1024 * 8) - free_bits;

                        // set variables
                        tuning_results++;
                        tuning_result[tuning_result_index].maxints = maxints;
                        tuning_result[tuning_result_index].sample_duration = sample_duration;
                        tuning_result[tuning_result_index].sample_max = sample_max;
                        tuning_result[tuning_result_index].blocksize_kb = blocksize_kb;
                        tuning_result[tuning_result_index].free_bits = free_bits;
                        tuning_result[tuning_result_index].blocksize_bits = blocksize_bits;
                        tuning_result[tuning_result_index].smallstep_faster = smallstep_faster;
                        tuning_result[tuning_result_index].mediumstep_faster = mediumstep_faster;
                        tuning_result[tuning_result_index].vectorstep_faster = vectorstep_faster;
                        tuning_result[tuning_result_index].threads = threads;
                        tune_benchmark(tuning_result, tuning_result_index);

                        if ( tuning_result[tuning_result_index].avg > best_avg) {
                            best_avg = tuning_result[tuning_result_index].avg;
                            best_blocksize_bits = blocksize_bits;
                            best_smallstep_faster = smallstep_faster;
                            best_mediumstep_faster = mediumstep_faster;
                            best_vectorstep_faster = vectorstep_faster;
                            if (option_verboselevel >=2) { printf(".(>)"); tuning_result_print(tuning_result[tuning_result_index]); }
                        }
                        if (option_verboselevel >= 3) { printf("...."); tuning_result_print(tuning_result[tuning_result_index]); }
                        tuning_result_index++;
                        verbose_at(1) { printf("\rTuning...tuning %ju options..",(uintmax_t)tuning_results); fflush(stdout); }
                    }
                }
            }
        }
    }
    verbose_at(1) { printf("\rTuning...tuned %ju options..",(uintmax_t)tuning_results); }
    verbose(2) { printf("Tuning...tuned %ju options..\n",(uintmax_t)tuning_results); }
    verbose(2) {
        printf("%ju options. Inital best blocksize: %ju; best smallstep %ju; best mediumstep %ju; best vectorstep %ju\n",(uintmax_t)tuning_results,(uintmax_t)best_blocksize_bits, (uintmax_t)best_smallstep_faster,(uintmax_t)best_mediumstep_faster, (uintmax_t)best_vectorstep_faster);
        printf("Best options\n");
    }

    counter_t step=0;
    counter_t tuning_results_max = tuning_results;
    while (tuning_results>4) {
        qsort(tuning_result, (size_t)tuning_results, sizeof(tuning_result_type), compare_tuning_result);
        step++;
        verbose(2) {
            tuning_result_index = 0;
            printf("(%ju) ",(uintmax_t)step); tuning_result_print(tuning_result[tuning_result_index]);
            fflush(stdout);
            verbose(3) {
                for (counter_t tuning_result_index=0; tuning_result_index<min(40,tuning_results); tuning_result_index++) {
                    printf("..."); tuning_result_print(tuning_result[tuning_result_index]);
                }
            }
        }

        tuning_results = tuning_results / 2;

        verbose(2) printf("\n");
        for (counter_t i=0; i<tuning_results; i++) {
            verbose(1) { printf("\rTuning...found %ju options..benchmarking step %ju - tuning %3ju options",(uintmax_t)tuning_results_max,(uintmax_t)step,i); fflush(stdout); }
            tune_benchmark(tuning_result, i);
            tuning_result[i].sample_max += sample_max;
            tuning_result[i].sample_duration += 0.05;
        }
        verbose(2) printf("\n");
        
    }
    tuning_result_type best_result = tuning_result[0];
    verbose(2) {
        printf(".Best result:"); tuning_result_print(best_result);
    }
    verbose(1) printf("\n");

    free(tuning_result);
    return best_result;
}


int main(int argc, char *argv[]) {
    
    // initialize options
    double option_max_time = default_max_time;
    counter_t option_maxFactor  = default_sieve_limit;
    counter_t option_showMaxFactor = default_showMaxFactor;
    int option_verboselevel = default_verbose_level;
    int option_check = default_check_level;
    int option_tunelevel = default_tune_level;
    int option_threads = 1;
    #ifdef _OPENMP
    int max_threads = omp_get_max_threads();
    option_threads = max_threads;
    #endif

    // processing command line changes to options
    for (int arg=1; arg < argc; arg++) {
        if (strcmp(argv[arg], "--help")==0) { usage(argv[0]); }
        else if (strcmp(argv[arg], "--verbose")==0) {
            if (++arg >= argc) { fprintf(stderr, "No verbose level specified\n"); usage(argv[0]); }
            if (sscanf(argv[arg], "%d", &option_verboselevel) != 1 || option_verboselevel > 4) {
                fprintf(stderr, "Error: Invalid measurement time: %s\n", argv[arg]); usage(argv[0]);
            }
            verbose(1) printf("Verbose level set to %d\n",option_verboselevel);
        } 
        else if (strcmp(argv[arg], "--check")==0) { option_check=1; }
        else if (strcmp(argv[arg], "--tune")==0) { option_tunelevel=0;
            if (++arg >= argc) { fprintf(stderr, "No tune level specified\n"); usage(argv[0]); }
            if (sscanf(argv[arg], "%d", &option_tunelevel) != 1 || option_tunelevel > 4) {
                fprintf(stderr, "Error: Invalid tune level: %s\n", argv[arg]); usage(argv[0]);
            }
            verbose(1) printf("Tune level set to %d\n",option_tunelevel);
        }
        else if (strcmp(argv[arg], "--time")==0) { option_max_time=0;
            if (++arg >= argc) { fprintf(stderr, "No time specified\n"); usage(argv[0]); }
            if (sscanf(argv[arg], "%lf", &option_max_time) != 1 ) {
                fprintf(stderr, "Error: Invalid max time: %s\n", argv[arg]); usage(argv[0]);
            }
            verbose(1) printf("Max time is set to %d seconds\n",option_tunelevel);
        }
        else if (strcmp(argv[arg], "--show")==0) { option_showMaxFactor=0;
            if (++arg >= argc) { fprintf(stderr, "No show maximum specified\n"); usage(argv[0]); }
            if (sscanf(argv[arg], "%ju", (uintmax_t*)&option_showMaxFactor) != 1 || option_showMaxFactor > option_maxFactor) {
                fprintf(stderr, "Error: Invalid show maximum: %s\n", argv[arg]); usage(argv[0]);
            }
            verbose(1) printf("Show maximum set to %ju\n",(uintmax_t)option_showMaxFactor);
        }
        else if (strcmp(argv[arg], "--threads")==0) { 
            if (++arg >= argc) { fprintf(stderr, "No thread maximum specified\n"); usage(argv[0]); }
        #ifdef _OPENMP
            if (strcmp(argv[arg], "all")==0) option_threads = max_threads;
            else if (strcmp(argv[arg], "half")==0) option_threads = max_threads>>1;
            else if (sscanf(argv[arg], "%d", &option_threads) != 1 ) { fprintf(stderr, "Error: Invalid max threads: %s\n", argv[arg]); usage(argv[0]); }
            if (option_threads <1)  option_threads = 1;
            if (option_threads > max_threads)  option_threads = max_threads;
            verbose(1) printf("Thread maximum set to %ju\n",(uintmax_t)option_threads);
        #else
            verbose(1) printf("This is the version without multithreading - ignoring threads\n");
        #endif
        }
        else if (sscanf(argv[arg], "%ju", (uintmax_t*)&option_maxFactor) != 1) {
            fprintf(stderr, "Invalid size %s\n",argv[arg]); usage(argv[0]); 
            printf("Maximum set to %ju\n",(uintmax_t)option_maxFactor);
        }
    }

    // for debugging
    if (option_runonce) { // used for stats and debugging
        struct sieve_t* sieve = sieve_shake(option_maxFactor, default_blocksize);
        printf("\nResult set:\n");
        show_primes(sieve, option_showMaxFactor);
        int valid = validatePrimeCount(sieve,3);
        if (!valid) printf("The sieve is NOT valid\n");
        else printf("The sieve is VALID\n");
        sieve_delete(sieve);
        exit(0);
    }
        
    // if --check is needed
    if (option_check) {
        // Count the number of primes and validate the result
        verbose(1) { printf("Validating..."); fflush(stdout); }
        verbose(2) printf("\n");

        // validate algorithm - run one time for all sizes
        for (counter_t sieveSize_check = 100; sieveSize_check <= 10000000; sieveSize_check *=10) {
            verbose(2) { printf("...Checking size %ju ...",(uintmax_t)sieveSize_check); fflush(stdout); }
            struct sieve_t *sieve_check;
            for (counter_t blocksize_bits=1024; blocksize_bits<=2*1024*8; blocksize_bits *= 2) {
                verbose(3) printf(".blocksize %ju-",(uintmax_t)blocksize_bits);
                sieve_check = sieve_shake(sieveSize_check, blocksize_bits);
                int valid = validatePrimeCount(sieve_check,option_verboselevel);
                sieve_delete(sieve_check);
                if (!valid) return 0; else verbose(3) printf("valid;");
            }
            verbose(2) printf("valid\n");
        }
        verbose(1) printf("valid algorithm\n");
    }
    
    // tuning - try combinations of different settings and apply these
    counter_t best_blocksize_bits = default_blocksize;
    if (option_tunelevel) {
        tuning_result_type tuning_result = tune(option_tunelevel, option_maxFactor, option_threads, option_verboselevel);
        global_SMALLSTEP_FASTER = tuning_result.smallstep_faster;
        global_MEDIUMSTEP_FASTER = tuning_result.mediumstep_faster;
        global_VECTORSTEP_FASTER = tuning_result.vectorstep_faster;
        best_blocksize_bits = tuning_result.blocksize_bits;
    }

    // time the algorithm using the best settings
    struct timespec start_time,end_time;
    counter_t used_threads = 1;

    // this section will only by linked in if the -fopenmp option is used.
    // will default to max threads and then device by 2 for a non-hyperthreading option
    #ifdef _OPENMP
    counter_t max_tries = 4;
    for(counter_t threads=option_threads; threads>=1; threads= (threads>>1)  ) {
        omp_set_num_threads(threads);
        used_threads=threads;
        if (--max_tries ==1) threads = 2;
    #endif
        verbose(2) printf("\n");
        verbose(1) printf("Benchmarking... with blocksize %ju steps: %ju/%ju/%ju and %ju threads for %.1f seconds - Results:\n", (uintmax_t)best_blocksize_bits,(uintmax_t)global_SMALLSTEP_FASTER, (uintmax_t)global_MEDIUMSTEP_FASTER,(uintmax_t)global_VECTORSTEP_FASTER,(uintmax_t)used_threads,option_max_time );
        counter_t passes = 0;
        counter_t blocksize_bits = best_blocksize_bits;
        double elapsed_time = 0;
        struct sieve_t *sieve;
        clock_gettime(CLOCK_MONOTONIC,&start_time);
        #ifdef _OPENMP
        #pragma omp parallel reduction(+:passes)
        #endif
        for (;elapsed_time <= option_max_time;) {
            sieve = sieve_shake(option_maxFactor, blocksize_bits);//blocksize_bits);
            sieve_delete(sieve);
            passes++;
            clock_gettime(CLOCK_MONOTONIC,&end_time);
            elapsed_time = end_time.tv_sec + end_time.tv_nsec*1e-9 - start_time.tv_sec - start_time.tv_nsec*1e-9;
        }
        #ifdef _OPENMP
        printf("rogiervandam_extend_epar;%ju;%f;%ju;algorithm=other,faithful=yes,bits=1\n", (uintmax_t)passes,elapsed_time,(uintmax_t)used_threads);
        #else
        printf("rogiervandam_extend;%ju;%f;%ju;algorithm=other,faithful=yes,bits=1\n", (uintmax_t)passes,elapsed_time,(uintmax_t)used_threads);
        #endif
        verbose(1) printf("\033[0;32m(Passes - per %.1f seconds: %f - per second \033[1;33m%.1f\033[0;32m)\033[0m\n", option_max_time, option_max_time*passes/elapsed_time, passes/elapsed_time);
        verbose(1) if (used_threads>1) printf("\033[0;32m(Passes per thread (total %ju) - per %.1f seconds: %.1f - per second \033[1;33m%.1f\033[0;32m)\033[0m\n", (uintmax_t)used_threads,  option_max_time, option_max_time*passes/elapsed_time/used_threads, passes/elapsed_time/used_threads);
    #ifdef _OPENMP
    }
    #endif

    // show results for --show command line option
    if (option_showMaxFactor > 0) {
        printf("Show result set:\n");
        struct sieve_t* sieve = sieve_shake(option_maxFactor, option_maxFactor);
        show_primes(sieve, option_showMaxFactor);
        sieve_delete(sieve);
    }
}
