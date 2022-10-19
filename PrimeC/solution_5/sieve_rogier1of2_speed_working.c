// Serial code prime sieve by Daniel Spangberg
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <math.h>

#define counter_t uint32_t
#define bitword_t uint32_t
#define  markmask(factor) ((bitword_t)1U << factor)
#define wordindex(factor) (factor >> 5U)

struct sieve_state {
    bitword_t    *bitarray;
    unsigned int  bits;
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

struct sieve_state *create_sieve(int maxints) {
    struct sieve_state *sieve_state = malloc(sizeof *sieve_state);
    sieve_state->bitarray = calloc( maxints / 2 / sizeof(bitword_t)+1 , sizeof(bitword_t) );
    sieve_state->bits     = maxints >> 1;
    return sieve_state;
}

void delete_sieve(struct sieve_state *sieve_state) {
    free(sieve_state->bitarray);
    free(sieve_state);
}

struct sieve_state *run_sieve(counter_t maxints) {
    struct sieve_state *sieve_state = create_sieve(maxints);
    bitword_t   *bitarray  = sieve_state->bitarray;
    counter_t         bits = sieve_state->bits;
    counter_t            q = (counter_t)(1+(isqrt(maxints)>>1U));

    for (counter_t factor=1; factor < q; ) {
        counter_t step  = factor * 2 + 1;
        counter_t start = factor * factor * 2 + factor * 2;

        for (counter_t multiple = start; multiple < bits; multiple += step) {
            bitarray[wordindex(multiple)] |= markmask(multiple);
        }

        do {
            factor++;
        } while (bitarray[wordindex(factor)] & markmask(factor));
    }
    return sieve_state;
}

counter_t count_primes(struct sieve_state *sieve_state) {
    counter_t       bits = sieve_state->bits;
    bitword_t  *bitarray = sieve_state->bitarray;
    counter_t primecount = 1;    // We already have 2
    for (counter_t factor=1; factor < bits; factor++) {
        if (bitarray[wordindex(factor)] & markmask(factor)) {
            printf(" \b"); //don't know why this is necessary workaround?
        }
        else primecount++;
    }
    return primecount;
}

int main(int argc, char **argv) {
    counter_t maxints  = 1000000;
    double    max_time = 5;
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
        default:    valid_primes=-1;
    }

    struct timespec start_time,end_time;
    counter_t passes = 0;

    // The initial time

    struct sieve_state *sieve_state;

    double elapsed_time = 0;
    clock_gettime(CLOCK_MONOTONIC,&start_time);
    while (elapsed_time <= max_time) {
        sieve_state = run_sieve(maxints);
        delete_sieve(sieve_state);
        passes++;
        clock_gettime(CLOCK_MONOTONIC,&end_time);
        elapsed_time = end_time.tv_sec + end_time.tv_nsec*1e-9 - start_time.tv_sec - start_time.tv_nsec*1e-9;
    }

    // Count the number of primes and validate the result
    sieve_state = run_sieve(maxints);
    counter_t primecount = count_primes(sieve_state);
    printf("rogiervandam1of2_speed;%d;%f;1;algorithm=base,faithful=yes,bits=1\n", passes,elapsed_time);
    printf("valid=%d   primes %d\n",(primecount==valid_primes), primecount);
    delete_sieve(sieve_state);
}
