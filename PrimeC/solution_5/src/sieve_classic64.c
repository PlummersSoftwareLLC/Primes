// Sieve algorithm by Rogier van Dam - 2025
// Find all primes up to <max int> using the Sieve of Eratosthenes (https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes)

// This file includes all the building blocks for the sieve algorithm "classic style"

#include "generic/timepriority.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>

static char algorithm_name[] = "rogiervandam_classic64bit";
static char algorithm_type[] = "base";
#define ALGORITHM_CLASSIC 1

// include helper functions
#include "generic/settings.h"
#include "benchmark/sieve_options.h"
#include "bitstorage/bitstorage_search.h"
#include "sieve/sieve_calc.h"
#include "sieve/sieve_manager.h"

// This is the main module that directs all the work
// sieve_size in a real number that is the maximum in the sieve (not in bits)

#define bitbucket_t uint64_t

static struct sieve_t* shakeSieve(const counter_t sieve_size)
{
    struct sieve_t *sieve = sieve_create(sieve_size);
    bitbucket_t* bitstorage = __builtin_assume_aligned(sieve->bitstorage, cache_line_bytes);
    const counter_t sieve_bits = sieve->bits;
    const counter_t prime_max = prime_stop(sieve_bits);

    verbose5( printf("\nShaking sieve to find all primes up to %ju\n",(uintmax_t)sieve_size); )

    sieve_clear(sieve);
    counter_t prime = 1;
 
    while (prime < prime_max) {
        const counter_t step  = prime * 2 + 1;
        const counter_t start = prime * (step + 1);

        // #pragma GCC ivdep
        #pragma GCC unroll 32
        for(counter_t i=start; i < sieve_bits; i += step) {
            bitstorage[index_type(i, bitbucket_t)] |= markmask_calc_type(i,bitbucket_t);
        }

        // #pragma GCC ivdep
        #pragma GCC unroll 32
        for (prime++; bitstorage[index_type(prime, bitbucket_t)] & markmask_type(prime, bitbucket_t); prime++);
    }

    // return the completed sieve
    return sieve;
}

#include "benchmark/sieve_main.h"