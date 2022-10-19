// Serial code prime sieve by Daniel Spangberg
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <math.h>
#include <string.h>

#define WORD_SIZE 32
#define counter_t uint32_t
#define bitword_t uint32_t
#define  markmask(factor) ((bitword_t)1U << (factor&31))
#define offsetmask(factor) (factor & (sizeof(bitword_t)*8-1))
#define wordindex(factor) (factor >> 5U)

// Return the smallest multiple N of y such that:  x <= y * N
#define ceiling(x,y) (((x) + (y) - 1) / (y))

#define debug if (0)
#define sieve_limit 1000000
#define sieve_duration 5

static inline void printBits(size_t const size, void const * const ptr)
{
    unsigned char *b = (unsigned char*) ptr;
    unsigned char byte;
    int i, j;

    for (i = size-1; i >= 0; i--) {
        for (j = 7; j >= 0; j--) {
            byte = (b[i] >> j) & 1;
            if (byte==0) printf(".");
            else printf("%u", byte);
        }
        printf(" ");
    }
}

struct sieve_state {
    bitword_t    *bitarray;
    counter_t     bits;
};

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


static inline void dump_bitarray(struct sieve_state *sieve) {
    counter_t       bits = sieve->bits;
    bitword_t  *bitarray = sieve->bitarray;
    printf("Dumping sieve with %d bits\n",bits);
    for (counter_t word=0; word <= 1+(bits >> 5); word+=2) {
        printf("word %2d %2d ",word+1,word);
        printBits(8,&bitarray[word]);
        printf(" %6d - %6d\n", word*32+63, word*32);

    }
    puts("");
}

static inline counter_t count_primes(struct sieve_state *sieve) {
    counter_t       bits = sieve->bits;
    bitword_t  *bitarray = sieve->bitarray;
    counter_t primecount = 1;    // We already have 2
    for (counter_t factor=1; factor < bits; factor++) {
        if ((bitarray[wordindex(factor)] & markmask(factor))==0) {
            if (primecount < 100) {
                debug printf("%3d ",factor*2+1);
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
static inline struct sieve_state *create_sieve(int maxints) {
    struct sieve_state *sieve = aligned_alloc(8, sizeof(struct sieve_state));
    counter_t memSize = ceiling(1+maxints/2, 64*8) * 64; //make multiple of 8
    debug printf("MemSize %d bytes  %d bits\n", memSize, memSize * 8);
    sieve->bitarray = aligned_alloc(64, memSize );
    sieve->bits     = maxints >> 1;
    for(counter_t i=0; i <= (memSize/sizeof(bitword_t)); i++) {
        sieve->bitarray[i] = (bitword_t)0;
    }
    return sieve;
}

// struct sieve_state *create_sieve(int maxints) {
//     struct sieve_state *sieve_state = malloc(sizeof *sieve_state);
//     sieve_state->bitarray = calloc( maxints / 2 / sizeof(bitword_t)+1 , sizeof(bitword_t) );
//     sieve_state->bits     = maxints >> 1;
//     return sieve_state;
// }

static inline void delete_sieve(struct sieve_state *sieve) {
    free(sieve->bitarray);
    free(sieve);
}

static inline counter_t searchBitFalse(struct sieve_state *sieve, counter_t index) {
    while (sieve->bitarray[wordindex(index)] & markmask(index)) {
        index++;
    }
    return index;
}

static inline void setBitsTrue(struct sieve_state *sieve, counter_t range_start, counter_t step, counter_t range_stop) {
    if (step > 16) {
        counter_t range_stop_unique =  range_start + 32 * step;
        if (range_stop_unique > range_stop) {
            for (counter_t index = range_start; index < range_stop; index += step) {
                sieve->bitarray[wordindex(index)] |= markmask(index);
            }
            return;
        }

        counter_t range_stop_word = range_stop >> 5U;
        for (counter_t index = range_start; index < range_stop_unique; index += step) {
            counter_t wordOffset = index >> 5U;
            bitword_t mask = markmask(index);
            do {
                sieve->bitarray[wordOffset] |= mask;
                wordOffset += step; // pattern repeats on word level after {step} words
            } while (wordOffset <= range_stop_word);
        }
        return;
    }   

    counter_t index = range_start;
    counter_t wordOffset = wordindex(index);
    bitword_t wordValue = sieve->bitarray[wordOffset];

    while (index < range_stop) {
        wordValue |= markmask(index);
        index += step;
        counter_t newWordOffset = wordindex(index);
        if (newWordOffset != wordOffset) {
            sieve->bitarray[wordOffset] = wordValue;
            wordOffset = newWordOffset;
            wordValue = sieve->bitarray[wordOffset];
        }
    }
    sieve->bitarray[wordOffset] = wordValue;
}

static inline void copyBlocks(struct sieve_state *sieve, counter_t source_start, counter_t destination_start, counter_t destination_stop)	{
		const counter_t size = destination_start - source_start;
		counter_t copy_start = destination_start;

		if (size < WORD_SIZE*2) { // handle small: fill the second word
			counter_t copy_max = WORD_SIZE*2 + source_start;
			for (counter_t index=0; index<size; index++) {
                counter_t index_fromstart = source_start + index;
				if (sieve->bitarray[wordindex(index_fromstart)] & markmask(index_fromstart)) {
					counter_t copy_index = destination_start + index;
					while (copy_index < copy_max) {
						sieve->bitarray[wordindex(copy_index)] |= markmask(copy_index);
						copy_index += size;
					}
				}
			}
			while (copy_start < WORD_SIZE*2) copy_start += size;
		}

		counter_t source_word = wordindex(source_start);
		counter_t copy_word = wordindex(copy_start);
		counter_t destination_stop_word = wordindex(destination_stop);
		int shift = (source_start & 31) - (copy_start & 31); // can be negative
		bitword_t dest_wordValue = 0;

		if (shift > 0) {
            counter_t shift_flipped = WORD_SIZE-shift;
            dest_wordValue =  sieve->bitarray[source_word] >> shift;
            dest_wordValue |= sieve->bitarray[source_word+1] << shift_flipped;
            sieve->bitarray[copy_word] |= dest_wordValue; // or the start in to not lose data
    
            while (copy_word++ <= destination_stop_word) {
                source_word++;
                dest_wordValue =  sieve->bitarray[source_word] >> shift;
                dest_wordValue |= sieve->bitarray[source_word+1] << shift_flipped;
                sieve->bitarray[copy_word] = dest_wordValue; 
			}
            return;
        }
		if (shift < 0) {
            shift = -shift;
            counter_t shift_flipped = WORD_SIZE-shift;
            dest_wordValue =  sieve->bitarray[source_word] << shift;
            dest_wordValue |= sieve->bitarray[source_word-1] >> shift_flipped;
            sieve->bitarray[copy_word] |= dest_wordValue; // or the start in to not lose data
            while (copy_word++ <= destination_stop_word) {
                source_word++;
                dest_wordValue =  sieve->bitarray[source_word] << shift;
                dest_wordValue |= sieve->bitarray[source_word-1] >> shift_flipped;
                sieve->bitarray[copy_word] = dest_wordValue; 
            }
            return;

        }

        if (shift == 0) {
            while (copy_word++ <= destination_stop_word) {
                 sieve->bitarray[copy_word] = sieve->bitarray[source_word];
                source_word++;
            }
        }

	}

static inline struct sieve_state *run_sieve(counter_t maxints) {
    struct sieve_state *sieve = create_sieve(maxints);
    bitword_t            *bitarray  = sieve->bitarray;
    counter_t factor         = 1;
    counter_t blocksize_bits = 1;
    counter_t range          = 3;
    counter_t q              = (counter_t)(1+(isqrt(maxints)>>1U));

    while (factor <= q) {
        const counter_t start = factor * factor * 2 + factor * 2;
        const counter_t step  = factor * 2 + 1;

        if (range < sieve->bits) { // check if we should copy previous results
            range = blocksize_bits * step * 2;  // range is x2 so the second block cointains all multiples of primes
            if (range > sieve->bits) range = sieve->bits;
            copyBlocks(sieve, blocksize_bits, blocksize_bits*2, range);
            blocksize_bits = blocksize_bits * step;
        }

        setBitsTrue(sieve, start, step, range);
        factor = searchBitFalse(sieve, factor+1);
    }
    return sieve;
}



static inline void deepAnalyzePrimes(struct sieve_state *sieve) {
    printf("DeepAnalyzing\n");
    counter_t range_to =  sieve->bits;
    counter_t warn_prime = 0;
    counter_t warn_nonprime = 0;
    for (counter_t factor = 1; factor < range_to; factor++ ) {
        if ((sieve->bitarray[wordindex(factor)] & markmask(factor))==0) {                   // is this a prime?
            counter_t q = (isqrt(factor*2+1)|1)*2+1;
            for(counter_t c=1; c<=q; c++) {
                if ((factor*2+1) % (c*2+1) == 0 && (c*2+1) != (factor*2+1)) {
                    if (warn_prime++ < 30) {
                        printf("Number %d (%d) was marked prime, but %d * %d = %d\n", factor*2+1, factor,  c*2+1, (factor*2+1)/(c*2+1), factor*2+1 );
                    }
                }
            }
        }
        else {
            counter_t q = (isqrt(factor*2+1)|1)*2+1;
            counter_t c_factor = 0;
            for(counter_t c=1; c<=q; c++) {
                if ((factor*2+1) % (c*2+1) == 0 && (c*2+1) != (factor*2+1)) {
                    c_factor++;
                    break;
                }
            }
            if (c_factor==0) {
                if (warn_nonprime++ < 30) {
                    printf("Number %d (%d) was marked non-prime, but no factors found. So it is prime\n", factor*2+1, factor);
                }
            }
        }
    }
}

int main(int argc, char **argv) {
    counter_t maxints  = sieve_limit;
    double    max_time = sieve_duration;
    if (argc>1) sscanf(argv[1],"%u",&maxints);

//    printf(" 0 has %d trailing zeros\n", __builtin_ctz(0));
//    printf("~0 has %d trailing zeros\n", __builtin_ctz(~0));
//    printf(" 8 has %d trailing zeros\n", __builtin_ctz(8));

    counter_t valid_primes;
    switch(maxints) {
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

    struct timespec start_time,end_time;
    counter_t passes = 0;

    // The initial time

    struct sieve_state *sieve;

    // Count the number of primes and validate the result
    sieve = run_sieve(maxints);
    counter_t primecount = count_primes(sieve);
    int valid = (primecount==valid_primes);

    if (!valid) {
        printf("No valid result. Sievesize %d was expected to have %d primes, but algoritm produced %d primes\n",maxints,valid_primes,primecount );
//        deepAnalyzePrimes(sieve);
    }

    delete_sieve(sieve);

    if (valid) {
        printf("Valid algoritm. Benchmarking...\n");
        double elapsed_time = 0;
        clock_gettime(CLOCK_MONOTONIC,&start_time);
        while (elapsed_time <= max_time) {
            sieve = run_sieve(maxints);
            delete_sieve(sieve);
            passes++;
            clock_gettime(CLOCK_MONOTONIC,&end_time);
            elapsed_time = end_time.tv_sec + end_time.tv_nsec*1e-9 - start_time.tv_sec - start_time.tv_nsec*1e-9;
        }
        printf("rogiervandam_memcopy;%d;%f;1;algorithm=other,faithful=yes,bits=1\n", passes,elapsed_time);
//        printf("valid=%d   primes %d\n",valid, primecount);
    }
}
