// Serial code prime sieve by Daniel Spangberg
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <math.h>

#define counter_t uint32_t
#define bitword_t uint32_t
#define  markmask(factor) ((bitword_t)1U << factor)
#define offsetmask(factor) (factor & 31U)
#define wordindex(factor) (factor >> 5U)
#define debug if (0)

void printBits(size_t const size, void const * const ptr)
{
    unsigned char *b = (unsigned char*) ptr;
    unsigned char byte;
    int i, j;

    for (i = size-1; i >= 0; i--) {
        for (j = 7; j >= 0; j--) {
            byte = (b[i] >> j) & 1;
            printf("%u", byte);
        }
        printf(" ");
    }
}

struct sieve_state {
    bitword_t    *bitarray;
    counter_t     bits;
};

// https://stackoverflow.com/questions/31117497/fastest-integer-square-root-in-the-least-amount-of-instructions
static unsigned isqrt(unsigned long val) {
    unsigned long temp, g=0, b = 0x8000, bshft = 15;
    do {
        if (val >= (temp = (((g << 1) + b)<<bshft--))) {
           g += b;
           val -= temp;
        }
    } while (b >>= 1);
    return g;
}


void dump_bitarray(struct sieve_state *sieve) {
    counter_t       bits = sieve->bits;
    bitword_t  *bitarray = sieve->bitarray;
    for (counter_t word=0; word <= (bits >> 6); word+=2) {
        printf("word %2d %2d ",word+1,word);
        printBits(8,&bitarray[word]);
        printf(" %6d - %6d\n", word*64+63, word*64);

    }
    puts("");
}

struct sieve_state *create_sieve(int maxints) {
    struct sieve_state *sieve = aligned_alloc(8, sizeof(struct sieve_state));
    counter_t memSize = ((maxints >> 7) + 1) << 3; //make multiple of 8
    debug printf("MemSize %d\n", memSize);
    sieve->bitarray = aligned_alloc(8, memSize );
    sieve->bits     = maxints >> 1;
    for(counter_t i=0; i <= (memSize/sizeof(bitword_t)); i++) {
        sieve->bitarray[i] = (bitword_t)0;
    }
//    printf("returning sieve\n");
//    debug dump_bitarray(sieve);
    return sieve;
}

void delete_sieve(struct sieve_state *sieve) {
    free(sieve->bitarray);
    free(sieve);
}

static inline counter_t searchBitFalse(struct sieve_state *sieve, counter_t index) {
    counter_t wordOffset = wordindex(index);
    counter_t bitOffset  = offsetmask(index);

    do {
        bitword_t current_word = sieve->bitarray[wordOffset];
//        printf("Shifting by %3d   ", bitOffset);  printBits(4, &current_word);
        current_word >>= bitOffset;
//        printf("Result            "); printBits(4, &current_word);
        counter_t trailing_zeros = __builtin_ctz(~current_word);

        if (trailing_zeros < 32) {
//            printf("trailing zeros final %d\n", trailing_zeros);
//            printf("returning\n" );
            return (index + trailing_zeros);
        }
        else {
//            printf("Next word %d with leading_zeros %d index %d\n", wordOffset, trailing_zeros, index );
            wordOffset++;
            bitOffset = 0;
            index += 32 - trailing_zeros;
//            printf("Next word %d with leading_zeros %d index %d\n", wordOffset, trailing_zeros, index );
        }
    } while (index < sieve->bits);

    return sieve->bits;

/*
    do {
        bitword_t current_word = bitarray[wordOffset] | (markmask(bitOffset) - 1);
        counter_t leading_zeros = __builtin_clz(current_word & ~-~current_word);

        if (leading_zeros) {
            return (wordOffset << 5) | (32-leading_zeros);
        }
        wordOffset++;
        bitOffset = 0;
    } while (1);
*/
}

static inline struct sieve_state *run_sieve(counter_t maxints) {
    struct sieve_state *sieve = create_sieve(maxints);
    bitword_t   *bitarray  = sieve->bitarray;
    counter_t         bits = sieve->bits;
    counter_t            q = (counter_t)(1+(isqrt(maxints)>>1U));

    debug printf("running sieve with q %d\n", 2*q+1);
    for (counter_t factor=1; factor <= q; ) {
        counter_t step  = factor * 2 + 1;
        counter_t start = factor * factor * 2 + factor * 2;

//        printf("marking multiples of factor %d\n",factor);

        for (counter_t multiple = start; multiple < bits; multiple += step) {
//            printf("Marking %d as non-prime\n", multiple*2+1 );
            bitarray[wordindex(multiple)] |= markmask(multiple);
        }

//        printf("searching next bit\n");
/*
        do {
            factor++;
        } while (bitarray[wordindex(factor)] & markmask(factor));
*/
        factor = searchBitFalse(sieve, factor+1);
//        printf("next factor %d\n",factor*2+1);
    }
//    printf("sieve run finished\n");
    return sieve;
}

static inline counter_t count_primes(struct sieve_state *sieve) {
    counter_t       bits = sieve->bits;
    bitword_t  *bitarray = sieve->bitarray;
    counter_t primecount = 1;    // We already have 2
//    printf("\ncounting primes\n");
    for (counter_t factor=1; factor < bits; factor++) {
//        if ((factor-1) % 32 ==0) { printf("\n"); printBits(4, &bitarray[wordindex(factor)]);  }
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

void deepAnalyzePrimes(struct sieve_state *sieve) {
    printf("DeepAnalyzing\n");
    counter_t count = 1;
    counter_t range_to =  sieve->bits;
    for (counter_t factor = 1; factor < range_to; factor++ ) {
        if ((sieve->bitarray[wordindex(factor)] & markmask(factor))==0) {                   // is this a prime?
            counter_t q = (isqrt(factor*2+1)|1)*2+1;
            for(counter_t c=2; c<=q; c++) {
                if ((factor*2+1) % c == 0 && c != (factor*2+1)) {
                    printf("Number %d was marked prime, but %d * %d = %d\n", factor*2+1, c*2+1, (factor*2+1)/c, factor*2+1 );
                    if (count++ >= 100) return;
                }
            }
        }
        else {
            counter_t q = (isqrt(factor*2+1)|1)*2+1;
            counter_t c_factor = 0;
            for(counter_t c=2; c<=q; c++) {
                if ((factor*2+1) % c == 0 && c != (factor*2+1)) {
                    c_factor++;
                    break;
                }
            }
            if (c_factor==0) {
                printf("Number %d was marked non-prime, but no factors found. So it is prime\n", factor*2+1);
            }
        }
    }
}

int main(int argc, char **argv) {
    counter_t maxints  = 1000000;
    double    max_time = 5;
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

    double elapsed_time = 0;
    clock_gettime(CLOCK_MONOTONIC,&start_time);
    while (elapsed_time <= max_time) {
        sieve = run_sieve(maxints);
        delete_sieve(sieve);
        passes++;
        clock_gettime(CLOCK_MONOTONIC,&end_time);
        elapsed_time = end_time.tv_sec + end_time.tv_nsec*1e-9 - start_time.tv_sec - start_time.tv_nsec*1e-9;
    }

    // Count the number of primes and validate the result
    sieve = run_sieve(maxints);
    counter_t primecount = count_primes(sieve);
    int valid = (primecount==valid_primes);
    printf("rogiervandam1of2_speed;%d;%f;1;algorithm=base,faithful=yes,bits=1\n", passes,elapsed_time);
    printf("valid=%d   primes %d\n",valid, primecount);

    deepAnalyzePrimes(sieve);
    delete_sieve(sieve);
}
