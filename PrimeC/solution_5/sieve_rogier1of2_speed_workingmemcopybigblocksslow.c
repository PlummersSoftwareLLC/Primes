// Serial code prime sieve by Daniel Spangberg
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <math.h>
#include <string.h>
//#include <x86intrin.h>
#include <immintrin.h>

#define counter_t uint32_t
#define bitword_t uint32_t
#define  markmask(factor) ((bitword_t)1U << factor)
#define offsetmask(factor) (factor & (sizeof(bitword_t)*8-1))
#define wordindex(factor) (factor >> 5U)

// Return the smallest multiple N of y such that:  x <= y * N
#define ceiling(x,y) (((x) + (y) - 1) / (y))

#define debug if (0)
#define sieve_limit 1000000
#define sieve_duration 5

struct sieve_state {
    bitword_t    *bitarray;
    counter_t     sieveSizeInBits;
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

// use cache lines as much as possible
static inline struct sieve_state *create_sieve(int maxints) {
    struct sieve_state *sieve = aligned_alloc(8, sizeof(struct sieve_state));
    counter_t memSize = ceiling(1+maxints/2, 64*8) * 64; //make multiple of 8
    debug printf("MemSize %d bytes  %d bits\n", memSize, memSize * 8);
    sieve->bitarray = aligned_alloc(64, memSize );
    sieve->sieveSizeInBits     = maxints >> 1;
    for(counter_t i=0; i <= (memSize/sizeof(bitword_t)); i++) {
        sieve->bitarray[i] = (bitword_t)0;
    }
    return sieve;
}

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

static inline struct sieve_state *run_sieve(counter_t maxints) {
    struct sieve_state *sieve = create_sieve(maxints);
    bitword_t            *bitarray  = sieve->bitarray;
    const counter_t sieveSizeInBits = sieve->sieveSizeInBits;

    counter_t blocksize_bits = 3 << 8;
    counter_t range = blocksize_bits << 1;
    counter_t factor = 1;

    while (factor <= sieveSizeInBits) {
        const counter_t start = factor * factor * 2 + factor * 2;
        const counter_t step  = factor * 2 + 1;

        if (start > range) break;

        for (counter_t product = start; product < range; product += step) {
            bitarray[wordindex(product)] |= markmask(product);
        }

        factor = searchBitFalse(sieve, factor+1);

        if (range < sieveSizeInBits) {
            const counter_t blocksize_bits_new = blocksize_bits * (factor*2+1);
            range = blocksize_bits_new << 1;
            if ((range << 1) > sieveSizeInBits) {
                range = sieveSizeInBits;
            }

            const counter_t blocksize_word = blocksize_bits >> 8;
            const counter_t range_in_word = range >> 8;
            for (counter_t blockpart=0; blockpart < blocksize_word; blockpart++) {
                counter_t block_word_offset = blocksize_word + blockpart;
                void *src = &bitarray[block_word_offset*8];
                __m256i tmp0 = _mm256_load_si256((const void *)src);
                while (block_word_offset < range_in_word) {
                    block_word_offset += blocksize_word;
                    void *dst = &bitarray[block_word_offset*8];
                    _mm256_stream_si256(dst, tmp0);
                }
            }
            blocksize_bits = blocksize_bits_new;
        }
    }
    return sieve;
}

static inline counter_t count_primes(struct sieve_state *sieve) {
    counter_t       bits = sieve->sieveSizeInBits;
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
    puts("");
    return primecount;
}

static inline void deepAnalyzePrimes(struct sieve_state *sieve) {
    printf("DeepAnalyzing\n");
    counter_t range_to =  sieve->sieveSizeInBits;
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
        deepAnalyzePrimes(sieve);
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
        printf("rogiervandam1of2_speed;%d;%f;1;algorithm=base,faithful=yes,bits=1\n", passes,elapsed_time);
        printf("valid=%d   primes %d\n",valid, primecount);
    }
}
