// Sieve algorithm by Rogier van Dam - 2025
// Find all primes up to <max int> using the Sieve of Eratosthenes (https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes)

// This file includes all the building blocks for the sieve algorithm "base"
// This enables the compiler to optimize the code better

#include "generic/timepriority.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>

static char algorithm_name[] = "rogiervandam_base";
static char algorithm_type[] = "base";
#define ALGORITHM_BASE 1

// include helper functions
#include "generic/settings.h"
#include "benchmark/sieve_options.h"
#include "bitstorage/bitstorage_search.h"
#include "bitstorage/bitstorage_setBitsTrue_base.h"
// #include "bitstorage/bitstorage_setBitsTrue.h"
#include "sieve/sieve_calc.h"
#include "sieve/sieve_manager.h"

/* This is the main module that directs all the work
   sieve_size in a real number that is the maximum in the sieve (not in bits)
   block_size is in bits and determines how large the blocks are which are processed 
*/
static struct sieve_t* shakeSieve(const counter_t sieve_size)
{
    struct sieve_t *sieve = sieve_create(sieve_size);
    void* bitstorage = __builtin_assume_aligned(sieve->bitstorage, cache_line_bytes);
    const counter_t sieve_bits = sieve->bits;
    const counter_t prime_max = prime_stop(sieve_bits);

    // use globals as constant
    const counter_t stripeprime_faster = global_stripeprime_faster;
    const counter_t blocksize_bits     = global_blocksize_bits;
    
    verbose5(  printf("\nShaking sieve to find all primes up to %ju with blocksize %ju\n",(uintmax_t)sieve_size,(uintmax_t)blocksize_bits); )

    // code for algorithm = base
    sieve_clear(sieve);

    for (counter_t block_start = 0; block_start < sieve_bits; block_start += blocksize_bits) {
        const counter_t block_stop = block_start + blocksize_bits;
        const counter_t range_stop = min(sieve_bits, block_stop);
        counter_t prime = 1;

        while (prime < prime_max) {
            register const counter_t step  = prime * 2 + 1;
            register counter_t start = compute_start(prime, block_start);
            setBitsTrue_base(bitstorage, start, step, range_stop);
            prime = searchBitFalse(bitstorage, prime);
        }
    } 
    
    // return the completed sieve
    return sieve;
}

#include "benchmark/sieve_main.h"