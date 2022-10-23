// Serial code prime sieve by Daniel Spangberg
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <math.h>
#include <string.h>
#include "monotonic_xplatform.h"

#define debug if (0)
#define sieve_limit 1000000
//#define blocksize (16*1024*8)
// #define sieve_limit 100
#define sieve_duration 5


// 32 bit
// #define TYPE uint32_t
// #define WORD_SIZE 32
// #define SHIFT 5U

// 64 bit
#define TYPE uint64_t
#define WORD_SIZE 64
#define SHIFT 6U

#define counter_t int64_t
#define bitword_t TYPE
#define bitshift_t int64_t

counter_t global_SMALLSTEP_FASTER = WORD_SIZE/4;
counter_t global_MEDIUMSTEP_FASTER = WORD_SIZE*3/4;

#define SAFE_SHIFTBIT (bitword_t)1U
#define SAFE_ZERO  (bitword_t)0U
#define SMALLSTEP_FASTER ((counter_t)global_SMALLSTEP_FASTER)//WORD_SIZE)
#define MEDIUMSTEP_FASTER ((counter_t)global_MEDIUMSTEP_FASTER)//WORD_SIZE)
#define wordindex(index) (((counter_t)index) >> (bitshift_t)SHIFT)
#define bitindex(index) (((counter_t)index)&((bitword_t)(WORD_SIZE-1)))
#define  markmask(index) (SAFE_SHIFTBIT << bitindex(index))
#define chopmask(index) ((SAFE_SHIFTBIT << bitindex(index))-SAFE_SHIFTBIT)
#define chopmask2(index) (((bitword_t)2U << bitindex(index))-SAFE_SHIFTBIT)
//#define offsetmask(index) (((bitword_t)index) & ((bitword_t)sizeof(bitword_t)*8-1))


struct sieve_state {
    bitword_t* bitarray;
    counter_t  bits;
//    counter_t  words;
    counter_t  size;
};

#include "debugtools.h"

static inline counter_t count_primes(struct sieve_state *sieve) {
    counter_t       bits = sieve->bits;
    bitword_t  *bitarray = sieve->bitarray;
    counter_t primecount = 1;    // We already have 2
    for (counter_t factor=1; factor < bits; factor++) {
        if ((bitarray[wordindex(factor)] & markmask(factor))==0) {
            if (primecount < 100) {
                debug printf("%3ld ",(uint64_t)factor*2+1);
                debug if (primecount % 10 == 0) printf("\n");
                printf(" \b"); //don't know why this is necessary workaround?
            }
            primecount++;
        }
    }
    debug puts("");
    return primecount;
}

// use cache lines as much as possible
 #define ceiling(x,y) (((x) + (y) - 1) / (y)) // Return the smallest multiple N of y such that:  x <= y * N
static inline struct sieve_state *create_sieve(counter_t maxints) {
    counter_t maxints_c = maxints;
    struct sieve_state *sieve = malloc(sizeof *sieve);//aligned_alloc(8, sizeof(struct sieve_state));
    size_t memSize = ceiling(1+(maxints/2), 64*8) * 64; //make multiple of 8
//    size_t memSize = ((1+(maxints>>(1+6+3)))<<6) ; // 1 for maxints/2, 6 for 64 bit align, 3 for 8-bit bytes -> seems much slower..

    sieve->bitarray = aligned_alloc(64, memSize );
    sieve->bits     = maxints >> SAFE_SHIFTBIT;
    sieve->size     = maxints;
//    sieve->words    = memSize/sizeof(bitword_t);

    counter_t words = memSize/sizeof(bitword_t);
    for(counter_t index_word=0; index_word <= words; index_word++) {
        sieve->bitarray[index_word] = (bitword_t)0;
    }
    return sieve;
}


static inline void delete_sieve(struct sieve_state *sieve) {
    free(sieve->bitarray);
    free(sieve);
}

static inline counter_t searchBitFalse(struct sieve_state *sieve, counter_t index) {
    // while (sieve->bitarray[wordindex(index)] & markmask(index)) {
    //     index++;
    // }
    // return index;

    counter_t index_word = wordindex(index);
    bitshift_t index_bit  = bitindex(index);
    counter_t distance = (counter_t) __builtin_ctz( ~(sieve->bitarray[index_word] >> bitindex(index)));  // take inverse to be able to use ctz
    index += distance;
    distance += index_bit;

    while (distance >= WORD_SIZE) {
        index_word++;
        distance = __builtin_ctz(~(sieve->bitarray[index_word]));
        index += distance;
    }

    return index;
}

static inline void setBitsTrue_smallstep(struct sieve_state *sieve, counter_t range_start, counter_t step, counter_t range_stop) {

    // build the pattern in a word
	bitword_t pattern = SAFE_SHIFTBIT;
    for (counter_t patternsize = step; patternsize <= WORD_SIZE; patternsize += patternsize) {
        pattern |= (pattern << patternsize);
    }

    bitshift_t shift = bitindex(range_start);
    counter_t copy_word = wordindex(range_start);
    counter_t range_stop_word = wordindex(range_stop);

    if (copy_word == range_stop_word) { // shortcut
       sieve->bitarray[copy_word] |= ((pattern << shift) & chopmask2(range_stop)) ;
       return;
    }

    // from now on, we are before range_stop_word
    // first word is special, because it should not set bits before the range_start_bit
    sieve->bitarray[copy_word] |= (pattern << shift);
    bitshift_t pattern_shift = WORD_SIZE % step;
    shift = shift % step;
    copy_word++;
    bitshift_t pattern_size = WORD_SIZE - pattern_shift;
    pattern = (pattern << shift); // correct for inital offset

    while (copy_word < range_stop_word) {
        pattern = (pattern >> pattern_shift) | (pattern << (pattern_size - pattern_shift));
        sieve->bitarray[copy_word] |= pattern;
        copy_word++;
    } 
    pattern = (pattern >> pattern_shift) | (pattern << (pattern_size - pattern_shift));
    sieve->bitarray[copy_word] |= pattern & chopmask2(range_stop);
}


static inline void applyMask(struct sieve_state *sieve, const counter_t range_start_word, const counter_t step, const counter_t range_stop, const bitword_t mask) {
    const counter_t step_2 = step * 2;
    const counter_t step_3 = step * 3;
    const counter_t step_4 = step * 4;
    const counter_t range_stop_word = wordindex(range_stop);
    const counter_t fast_loop_stop_word = (range_stop_word>step_3) ? (range_stop_word - step_3) : 0;
    counter_t index_word = range_start_word;

    while (index_word < fast_loop_stop_word) {
        sieve->bitarray[index_word         ] |= mask;
        sieve->bitarray[index_word + step  ] |= mask;
        sieve->bitarray[index_word + step_2] |= mask;
        sieve->bitarray[index_word + step_3] |= mask;
        index_word += step_4;
    }

    while (index_word < range_stop_word) {
        sieve->bitarray[index_word] |= mask;
        index_word += step;
    }

    if (index_word == range_stop_word) {
        sieve->bitarray[range_stop_word] |= (mask & chopmask2(range_stop));
    }
}

static inline void setBitsTrue_simple(struct sieve_state *sieve, counter_t range_start, counter_t step, counter_t range_stop) {
    for (counter_t index = range_start; index <= range_stop; index += step) {
        sieve->bitarray[wordindex(index)] |= markmask(index);
    }
}

static inline void setBitsTrue_mediumstep(struct sieve_state *sieve, counter_t range_start, counter_t step, counter_t range_stop) {
    counter_t range_stop_unique =  range_start + WORD_SIZE * step;

    if (range_stop_unique > range_stop) {
        for (counter_t index = range_start; index <= range_stop;) {
            counter_t index_word = wordindex(index);
            bitword_t mask = SAFE_ZERO;
            do {
                mask |= markmask(index);
                index += step;
            } while (index_word == wordindex(index));
            sieve->bitarray[index_word] |= mask;
        }
    }
    else {
        for (counter_t index = range_start; index <= range_stop_unique;) {
            counter_t index_word = wordindex(index);
            bitword_t mask = SAFE_ZERO;
            do {
                mask |= markmask(index);
                index += step;
            } while (index_word == wordindex(index));
            applyMask(sieve, index_word, step, range_stop, mask);
        }
    }
}

static inline void setBitsTrue(struct sieve_state *sieve, counter_t range_start, counter_t step, counter_t range_stop) {
    counter_t range_stop_unique =  range_start + WORD_SIZE * step;
    for (counter_t index = range_start; index <= range_stop_unique; index += step) {
        bitword_t mask = markmask(index);
        applyMask(sieve, wordindex(index), step, range_stop, mask);
    }
}

static void copyPattern_smallsize(struct sieve_state *sieve, counter_t source_start, counter_t size, counter_t destination_stop)	{
    counter_t source_word = wordindex(source_start);
    bitshift_t source_bit = bitindex(source_start);

    bitword_t pattern = (sieve->bitarray[source_word] >> source_bit) | (sieve->bitarray[source_word+1] << (WORD_SIZE-source_bit)) & chopmask2(size);
    for (bitshift_t pattern_size = size; pattern_size <= WORD_SIZE; pattern_size += pattern_size) pattern |= (pattern << pattern_size);

    counter_t copy_start = source_start + size;
    counter_t copy_word = wordindex(copy_start);
    sieve->bitarray[copy_word] |= (pattern << bitindex(copy_start));
    if (copy_start == destination_stop) return;

    bitshift_t pattern_shift = WORD_SIZE % size;
    bitshift_t pattern_size = WORD_SIZE - pattern_shift;
    bitshift_t shift = (bitshift_t) WORD_SIZE - bitindex(copy_start);

    counter_t destination_stop_word = wordindex(destination_stop);

    while (copy_word < destination_stop_word) { // = will be handled as well because increment is after this 
        copy_word++;
        sieve->bitarray[copy_word] = (pattern << (pattern_size-shift)) | (pattern >> shift);
        shift = bitindex(shift + pattern_shift);  // alternative, but faster
    }
}

static inline void copyPattern(struct sieve_state *sieve, counter_t source_start, counter_t size, counter_t destination_stop)	{
    if (size < WORD_SIZE) { // handle small: fill the second word
       copyPattern_smallsize(sieve, source_start, size, destination_stop);
       return;
    }

    counter_t source_word = wordindex(source_start);
    bitshift_t source_bit = bitindex(source_start);
    counter_t destination_stop_word = wordindex(destination_stop);
    counter_t copy_start = source_start + size;

    counter_t copy_word = wordindex(copy_start);
    counter_t copy_bit = bitindex(copy_start);
    counter_t aligned_copy_word = source_word + size; // after <<size>> words, just copy at word level
    if (aligned_copy_word > destination_stop_word) aligned_copy_word = destination_stop_word;
    bitshift_t shift = source_bit - copy_bit;

    // printf("Copying largesize from %ld to max %ld with shift %ld\n",source_start,destination_stop, shift);

    if (shift > 0) {

        bitshift_t shift_flipped = WORD_SIZE-shift;
        sieve->bitarray[copy_word] |= ((sieve->bitarray[source_word] >> shift) 
                                    | (sieve->bitarray[source_word+1] << shift_flipped))
                                    & ~chopmask(copy_bit); // because this is the first word, dont copy the extra bits in front of the source
        copy_word++;
        source_word++;

        if (copy_word > source_word+4) {
            while (copy_word+4 < aligned_copy_word) {
                bitword_t source0 = sieve->bitarray[source_word  ];
                bitword_t source1 = sieve->bitarray[source_word+1];

                sieve->bitarray[copy_word  ] = (source0 >> shift) | (source1 << shift_flipped);
                bitword_t source2 = sieve->bitarray[source_word+2];
                sieve->bitarray[copy_word+1] = (source1 >> shift) | (source2 << shift_flipped);
                bitword_t source3 = sieve->bitarray[source_word+3];
                sieve->bitarray[copy_word+2] = (source2 >> shift) | (source3 << shift_flipped);
                bitword_t source4 = sieve->bitarray[source_word+4];
                sieve->bitarray[copy_word+3] = (source3 >> shift) | (source4 << shift_flipped);
                copy_word += 4;
                source_word += 4;
            }
        }

        while (copy_word <= aligned_copy_word) {
            sieve->bitarray[copy_word] = (sieve->bitarray[source_word] >> shift) | (sieve->bitarray[source_word+1] << shift_flipped); 
            copy_word++;
            source_word++;
        }

        if (copy_word >= destination_stop_word) return;
        source_word = copy_word - size; // recalibrate
        while (copy_word <= destination_stop_word) {
            sieve->bitarray[copy_word] = sieve->bitarray[source_word]; 
            copy_word++;
            source_word++;
        }

    }

    if (shift < 0) {
        // printf("Before firstword\n");
        // dump_bitarray(sieve);
        shift = -shift;
        bitshift_t shift_flipped = WORD_SIZE-shift;
        counter_t source_lastword = wordindex(copy_start);
        sieve->bitarray[copy_word] |= ((sieve->bitarray[source_word] << shift)  // or the start in to not lose data
                                    | (sieve->bitarray[source_lastword] >> shift_flipped)) 
                                    & ~chopmask(copy_bit);  
        copy_word++;
        source_word++;

        if (copy_word > source_word+4) {
            while (copy_word+4 <= destination_stop_word) {
                bitword_t sourcen = sieve->bitarray[source_word-1];
                bitword_t source0 = sieve->bitarray[source_word  ];

                sieve->bitarray[copy_word  ] = (source0 << shift) | (sourcen >> shift_flipped);
                bitword_t source1 = sieve->bitarray[source_word+1];
                sieve->bitarray[copy_word+1] = (source1 << shift) | (source0 >> shift_flipped);
                bitword_t source2 = sieve->bitarray[source_word+2];
                sieve->bitarray[copy_word+2] = (source2 << shift) | (source1 >> shift_flipped);
                bitword_t source3 = sieve->bitarray[source_word+3];
                sieve->bitarray[copy_word+3] = (source3 << shift) | (source2 >> shift_flipped);
                copy_word += 4;
                source_word += 4;
            }
        }

        while (copy_word <= aligned_copy_word) {
            sieve->bitarray[copy_word] = (sieve->bitarray[source_word-1] >> shift_flipped) | (sieve->bitarray[source_word] << shift); 
            copy_word++;
            source_word++;
        }
        
        if (copy_word >= destination_stop_word) return;
        source_word = copy_word - size;
        while (copy_word <= destination_stop_word) {
            sieve->bitarray[copy_word] = sieve->bitarray[source_word]; 
            copy_word++;
            source_word++;
        }
    }

    if (shift == 0) { // first word can be spread over 2 words
        counter_t shift_flipped = WORD_SIZE-shift;
        sieve->bitarray[copy_word] |= ((sieve->bitarray[source_word] >> shift) 
                                    | (sieve->bitarray[source_word+1] << shift_flipped))
                                    & ~chopmask(copy_bit); // because this is the first word, dont copy the extra bits in front of the source

        while (copy_word < destination_stop_word) {
            sieve->bitarray[copy_word] = sieve->bitarray[source_word];
            source_word++;
            copy_word++;
        }
    }

    // printf("End\n");
    // dump_bitarray(sieve);
}

static inline void sieve_block_stripe(struct sieve_state *sieve, counter_t block_start, counter_t block_stop, counter_t prime_start) {
    // counter_t prime = searchBitFalse(sieve, prime_start);
    counter_t prime = prime_start;
    counter_t start = prime * prime * 2 + prime * 2;
    counter_t step  = prime * 2 + 1;

    while (start <= block_stop) {
        step  = prime * 2 + 1;
        if (step > SMALLSTEP_FASTER) break;
        if (block_start >= (prime + 1)) start = block_start + prime + prime - ((block_start + prime) % step);
        setBitsTrue_smallstep(sieve, start, step, block_stop);
        prime = searchBitFalse(sieve, prime+1 );
        start = prime * prime * 2 + prime * 2;
    } 

    while (start <= block_stop) {
        step  = prime * 2 + 1;
        if (step > MEDIUMSTEP_FASTER) break;
        if (block_start >= (prime + 1)) start = block_start + prime + prime - ((block_start + prime) % step);
        setBitsTrue_mediumstep(sieve, start, step, block_stop);
        prime = searchBitFalse(sieve, prime+1 );
        start = prime * prime * 2 + prime * 2;
    } 

    while (start <= block_stop) {
        step  = prime * 2 + 1;
        if (block_start >= (prime + 1)) start = block_start + prime + prime - ((block_start + prime) % step);
        setBitsTrue(sieve, start, step, block_stop);
        prime = searchBitFalse(sieve, prime+1 );
        start = prime * prime * 2 + prime * 2;
    } 
}

struct block {
    counter_t pattern_size; // size of pattern applied 
    counter_t pattern_start; // start of pattern
    counter_t prime; // next prime to be striped
};

// returns prime that could not be handled:
// start is too large
// range is too big
static inline struct block sieve_block(struct sieve_state *sieve, counter_t block_start, counter_t block_stop) {
    counter_t prime            = 0;
    counter_t patternsize_bits = 1;
    counter_t pattern_start    = 0;
    counter_t range            = sieve->bits;
    counter_t range_stop       = block_stop;
    struct block block;

    do {
        prime = searchBitFalse(sieve, prime+1);
        counter_t start = prime * prime * 2 + prime * 2;
        if (start > block_stop) break;

        const counter_t step  = prime * 2 + 1;
        if (block_start >= (prime + 1)) start = block_start + prime + prime - ((block_start + prime) % step);

        range_stop = block_start + patternsize_bits * step * 2;  // range is x2 so the second block cointains all multiples of primes
        block.pattern_size = patternsize_bits;
        block.pattern_start = pattern_start;
        if (range_stop > block_stop) return block; //range_stop = block_stop;

        if (patternsize_bits>1) {
            pattern_start = block_start | patternsize_bits;
            copyPattern(sieve, pattern_start, patternsize_bits, range_stop);
        }
        patternsize_bits *= step;

        if      (step < SMALLSTEP_FASTER)  setBitsTrue_smallstep(sieve, start, step, range_stop);
        else if (step < MEDIUMSTEP_FASTER) setBitsTrue_mediumstep(sieve, start, step, range_stop);
        else                               setBitsTrue(sieve, start, step, range_stop);
        block.prime = prime;
    } while (range_stop < block_stop);

    return block;
}

static inline struct sieve_state *sieve(counter_t maxints, counter_t blocksize) {
    struct sieve_state *sieve = create_sieve(maxints);
    bitword_t*bitarray       = sieve->bitarray;
    counter_t prime          = 0;
    counter_t range          = sieve->bits;

    // sieve_block_stripe(sieve, 0, sieve->bits, 1);
    // return sieve;

    counter_t block_start = 0;
    counter_t block_stop = blocksize-1;

    // fill the whole sieve bij adding en copying incrementally
    struct block block = sieve_block(sieve, 0, sieve->bits);
    copyPattern(sieve, block.pattern_start, block.pattern_size, sieve->bits);
    prime = block.prime;
//    printf("Copyied Start %ld size %ld bits %ld prime %ld\n",(uint64_t)block.patternstart, (uint64_t)block.patternsize, (uint64_t)sieve->bits,(uint64_t) prime);

    do {
        if (block_stop > sieve->bits) block_stop = sieve->bits;
        prime = searchBitFalse(sieve, prime);
        sieve_block_stripe(sieve, block_start, block_stop, prime);
        // sieve_block_stripe(sieve, block_start, block_stop, prime+1);
        block_start += blocksize;
        block_stop += blocksize;
    } while (block_start <= sieve->bits);

    return sieve;
}

// https://stackoverflow.com/questions/31117497/fastest-integer-square-root-in-the-least-amount-of-instructions
static inline unsigned isqrt(unsigned long val) {
    unsigned long temp, g=0, b = 0x8000, bshft = 15;
    do {
        if (val >= (temp = (((g << 1) + b)<<bshft--))) {
           g += b;
           val -= temp;
        }
    } while (b >>= 1);
    return g;
}

static inline void deepAnalyzePrimes(struct sieve_state *sieve) {
    printf("DeepAnalyzing\n");
    counter_t range_to =  sieve->bits;
    counter_t warn_prime = 0;
    counter_t warn_nonprime = 0;
    for (counter_t prime = 1; prime < range_to; prime++ ) {
        if ((sieve->bitarray[wordindex(prime)] & markmask(prime))==0) {                   // is this a prime?
            counter_t q = (isqrt(prime*2+1)|1)*2+1;
            for(counter_t c=1; c<=q; c++) {
                if ((prime*2+1) % (c*2+1) == 0 && (c*2+1) != (prime*2+1)) {
                    if (warn_prime++ < 30) {
                        printf("Number %ld (%ld) was marked prime, but %ld * %ld = %ld\n", (uint64_t)prime*2+1, (uint64_t)prime,  (uint64_t)c*2+1, (uint64_t)(prime*2+1)/(c*2+1), (uint64_t)prime*2+1 );
                    }
                }
            }
        }
        else {
            counter_t q = (isqrt(prime*2+1)|1)*2+1;
            counter_t c_prime = 0;
            for(counter_t c=1; c<=q; c++) {
                if ((prime*2+1) % (c*2+1) == 0 && (c*2+1) != (prime*2+1)) {
                    c_prime++;
                    break;
                }
            }
            if (c_prime==0) {
                if (warn_nonprime++ < 30) {
                    printf("Number %ld (%ld) was marked non-prime, but no factors found. So it is prime\n", (uint64_t)prime*2+1,(uint64_t) prime);
                }
            }
        }
    }
}


int validatePrimeCount(struct sieve_state *sieve, int verboselevel) {

    counter_t primecount = count_primes(sieve);
    counter_t valid_primes;
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
        default:            valid_primes=-1;
    }

    int valid = (valid_primes == primecount);
    if (valid  && verboselevel >= 4) printf("Result: Sievesize %ld is expected to have %ld primes. Algoritm produced %ld primes\n",(uint64_t)sieve->size,(uint64_t)valid_primes,(uint64_t)primecount );
    if (!valid && verboselevel >= 2) printf("No valid result. Sievesize %ld was expected to have %ld primes, but algoritm produced %ld primes\n",(uint64_t)sieve->size,(uint64_t)valid_primes,(uint64_t)primecount );
    if (!valid && verboselevel >= 3) deepAnalyzePrimes(sieve);
    return (valid);
}

int main(int argc, char **argv) {
    counter_t maxints  = sieve_limit;

    if (argc>1) sscanf(argv[1],"%lu",&maxints);

    struct timespec start_time,end_time;

    // The initial time
    struct sieve_state *sieve_instance;

    // Count the number of primes and validate the result
	printf("Validating... \n\n");

	for (counter_t blocksize_bits=1024; blocksize_bits<=64*1024*8; blocksize_bits *= 2) {
		for (counter_t sieveSize_check = 100; sieveSize_check <= 100000000; sieveSize_check *=10) {
			// validate algorithm - run one time
//            printf("Checking size %ld blocksize %ld",sieveSize_check,blocksize_bits);
            sieve_instance = sieve(sieveSize_check, blocksize_bits);
            counter_t primecount = count_primes(sieve_instance);
            int valid = validatePrimeCount(sieve_instance,3);
            delete_sieve(sieve_instance);
            if (!valid) return 0;
		}
	}

    printf("Valid algoritm. Tuning...\n");
    double best_avg = 0;
    counter_t best_blocksize_bits = 0;
    counter_t best_smallstep_faster = 0;
    counter_t best_mediumstep_faster = 0;
    
    for (counter_t smallstep_faster = 0; smallstep_faster <= WORD_SIZE/2; smallstep_faster += 8) {
        global_SMALLSTEP_FASTER = smallstep_faster;
        for (counter_t mediumstep_faster = smallstep_faster+8; mediumstep_faster <= WORD_SIZE; mediumstep_faster += 8) {
            global_MEDIUMSTEP_FASTER = mediumstep_faster;
            for (counter_t blocksize_kb=32; blocksize_kb>=32; blocksize_kb /= 2) {
                for (counter_t free_bits=4096; free_bits < 8192 && free_bits < blocksize_kb * 1024 * 8; free_bits += 512) {
                    counter_t passes = 0;
                    counter_t blocksize_bits = blocksize_kb * 1024 * 8 - free_bits;
                    double elapsed_time = 0;
                    double sample_time = 0.1;
                    double start_time = monotonic_seconds();
                    while (elapsed_time <= sample_time) {
                        sieve_instance = sieve(maxints, blocksize_bits);//blocksize_bits);
                        delete_sieve(sieve_instance);
                        passes++;
                        elapsed_time = monotonic_seconds()-start_time;
                    }
                    double avg = passes/elapsed_time;
                    if (avg > best_avg) {
                        best_avg = avg;
                        best_blocksize_bits = blocksize_bits;
                        best_smallstep_faster = smallstep_faster;
                        best_mediumstep_faster = mediumstep_faster;

                        printf("blocksize_bits %ld; blocksize %3ldkb; free_bits %5ld; smallstep: %ld; mediumstep %ld; passes %3ld; time %f;average %f\n", (uint64_t)blocksize_bits, (uint64_t)blocksize_kb,(uint64_t)free_bits,(uint64_t)best_smallstep_faster,(uint64_t)best_mediumstep_faster,(uint64_t)passes,elapsed_time,avg);
                    }
                }
            }
        }
    }
    printf("Tuned algoritm. Best blocksize: %ld; best smallstep %ld; best mediumstep %ld\n",(uint64_t)best_blocksize_bits, (uint64_t)best_smallstep_faster,(uint64_t)best_mediumstep_faster);
    global_SMALLSTEP_FASTER = best_smallstep_faster;
    global_MEDIUMSTEP_FASTER = best_mediumstep_faster;

    double max_time = sieve_duration;
    if (best_blocksize_bits > 0) {
        printf("Benchmarking...\n");
        counter_t passes = 0;
        counter_t blocksize_bits = best_blocksize_bits;
        double elapsed_time = 0;
//        double start_time = monotonic_seconds();
        clock_gettime(CLOCK_MONOTONIC,&start_time);
        while (elapsed_time <= max_time) {
            sieve_instance = sieve(maxints, blocksize_bits);//blocksize_bits);
            delete_sieve(sieve_instance);
            passes++;
//            elapsed_time = monotonic_seconds()-start_time;
            clock_gettime(CLOCK_MONOTONIC,&end_time);
            elapsed_time = end_time.tv_sec + end_time.tv_nsec*1e-9 - start_time.tv_sec - start_time.tv_nsec*1e-9;

        }
        printf("rogiervandam_memcopy;%ld;%f;1;algorithm=other,faithful=yes,bits=1\n", (uint64_t)passes,elapsed_time);
    }
}
