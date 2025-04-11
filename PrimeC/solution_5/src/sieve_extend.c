// Sieve algorithm by Rogier van Dam - 2025
// Find all primes up to <max int> using the Sieve of Eratosthenes (https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes)

// This file includes all the building blocks for the sieve algorithm "extend"
// This enables the compiler to optimize the code better

#include "generic/timepriority.h"
#include <stdio.h>
#include <stdlib.h> // for malloc, free, exit and getenv
#include <time.h>
#include <stdint.h>

static char algorithm_name[] = "rogiervandam_extend";
static char algorithm_type[] = "other";

// include helper functions
#include "generic/settings.h"
#include "benchmark/sieve_options.h"
#include "bitstorage/bitstorage_search.h"
#include "bitstorage/bitstorage_setBitsTrue.h"
#include "bitstorage/bitstorage_continuePattern.h"
#include "sieve/sieve_calc.h"
#include "sieve/sieve_manager.h"
#include "sieve/sieve_stripe.h"
#include "sieve/sieve_extend.h"

/* This is the main module that directs all the work
   sieve_size in a real number that is the maximum in the sieve (not in bits)
   block_size is in bits and determines how large the blocks are which are processed 
*/
static struct sieve_t* shakeSieve(const counter_t sieve_size)
{
    struct sieve_t *sieve      = sieve_create(sieve_size);
    const counter_t sieve_bits = sieve->bits;
    const counter_t prime_max  = prime_stop(sieve_bits);

    // use globals as constant - these get optimized
    const counter_t stripeprime_faster  = global_stripeprime_faster;
    const counter_t blocksize_bits      = global_blocksize_bits;
    const counter_t algorithm           = global_algorithm;

    verbose5({
        printf("\nShaking sieve to find all primes up to %ju by marking multiples of all primes up to %ju\n", (uintmax_t)sieve_size, (uintmax_t)usqrt(sieve_size));
        printf("Using compressed primes up to %ju with sieve size %ju and blocksize %ju\n",(uintmax_t)prime_max, (uintmax_t)sieve_bits,(uintmax_t)blocksize_bits);
    })

    switch( algorithm ) 
    {
        case 1:
        {
            // fill the entire sieve for lower primes by adding en copying incrementally
            counter_t prime = extendSieveBlock0(sieve->bitstorage, sieve_bits);
            
            // continue from last the prime that was processed and stripe off the multiples of this prime
            // repeat until it is faster to do this block by block
            prime = stripeSieve(sieve->bitstorage, sieve_bits, prime, stripeprime_faster);

            // process the remaining primes block by block to minimize cache misses
            stripeSieveBlockByBlock(sieve->bitstorage, sieve_bits, blocksize_bits, prime, prime_max);
        } break;

        case 2: // process extend and stripe block by block
        {
            counter_t prime_next = extendSieveBlockByBlock(sieve->bitstorage, sieve_bits, blocksize_bits, stripeprime_faster);
            stripeSieveBlockByBlock(sieve->bitstorage, sieve_bits, blocksize_bits/2, prime_next, prime_max);
        } break;

        // case 3: // process everything block by block -- can be set via --set a3 on command line
        // {
        //     stripeSieveBlockByBlock(sieve->bitstorage, sieve_bits, blocksize_bits, 1, prime_max);
        // } break;
    }

    // return the completed sieve
    return sieve;
} 

#include "benchmark/sieve_main.h"
