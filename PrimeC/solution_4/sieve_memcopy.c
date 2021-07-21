/*
Sieve algorithm for drag-race
by Rogier van Dam, 21-07-2021

Based on a simple bitarray for the odd numbers.
Combined with a scheme to speed up marking the multiples by copying whole words for longer ranges.
This increases the potential to keep using the CPU level 1 caches.

Inspired by @davepl, Daniel Spångberg and Mike Koss
*/
#include <stdio.h>   // printf
#include <stdint.h>  // uint32_t
#include <stdlib.h>  // malloc
#include <time.h>    // clock

#define counter_t uint32_t
#define bitword_t uint32_t
#define bitword_bytes ((counter_t)sizeof(bitword_t))
#define bitword_shiftbits 5U
#define  markmask(factor) ((bitword_t)1U << (factor&31))
#define wordindex(factor) (factor >> bitword_shiftbits)

#define default_sieveSize 1000000
#define default_sieveDuration 5
#define default_verbose 0

struct sieve {
    bitword_t    *bitArray;
    counter_t     sieveSizeInBits;
};

static inline struct sieve *create_sieve(counter_t sieveSize) {
    struct sieve *sieve = malloc(sizeof(struct sieve));
    const counter_t sieveSizeFullWordsInBytes = (1+(sieveSize >> (bitword_shiftbits+1U))) << bitword_shiftbits;
    sieve->sieveSizeInBits = sieveSize >> 1U;
    sieve->bitArray        = malloc(sieveSizeFullWordsInBytes);

    for(counter_t i=0; i<(sieveSizeFullWordsInBytes/bitword_bytes); ++i) {
        sieve->bitArray[i] = (bitword_t)0;
    }
    return sieve;
}

static inline void delete_sieve(struct sieve *sieve) {
    free(sieve->bitArray);
    free(sieve);
}

static inline struct sieve *run_sieve(counter_t maxints) {
    struct sieve *sieve = create_sieve(maxints);

    counter_t blocksize_bits = 3 << bitword_shiftbits;
    counter_t range = blocksize_bits << 1;
    counter_t factor = 1;

    while (factor <= sieve->sieveSizeInBits) {
        const counter_t start = factor * factor * 2 + factor * 2;
        const counter_t step  = factor * 2 + 1;

        if (start > range) break;

        for (counter_t product = start; product < range; product += step) {
            sieve->bitArray[wordindex(product)] |= markmask(product);
        }

        do factor++; while (sieve->bitArray[wordindex(factor)] & markmask(factor));

        if (range < sieve->sieveSizeInBits) {
            const counter_t blocksize_bits_new = blocksize_bits * (factor*2+1);
            range = blocksize_bits_new << 1;
            if ((range << 1) > sieve->sieveSizeInBits) {
                range = sieve->sieveSizeInBits;
            }

            const counter_t blocksize_word = blocksize_bits >> bitword_shiftbits;
            const counter_t range_word = range >> bitword_shiftbits;
            for (counter_t blockpart=0; blockpart < blocksize_word; blockpart++) {
                counter_t block_offset_word = blocksize_word + blockpart;
                const bitword_t original = sieve->bitArray[block_offset_word];
                while (block_offset_word < range_word) {
                    block_offset_word += blocksize_word;
                    sieve->bitArray[block_offset_word] = original;
                }
            }
            blocksize_bits = blocksize_bits_new;
        }
    }
    return sieve;
}

static inline counter_t count_primes(struct sieve *sieve) {
    counter_t primecount = 1;
    for (counter_t factor=1; factor < sieve->sieveSizeInBits; factor++) {
        if ((sieve->bitArray[wordindex(factor)] & markmask(factor))==0) {
            primecount++;
        }
    }
    return primecount;
}

int main(int argc, char **argv) {
    counter_t sieveSize = default_sieveSize;
    double    max_time  = default_sieveDuration;

    // variable sieveSize to block compilter from using fixed size in compilation
    if (argc>1) sscanf(argv[1],"%u",&sieveSize);

    // Count the number of primes and validate the result
    counter_t known_sieveSize[]  = { 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000 };
    counter_t known_PrimeCount[] = {  4,  25,  168,  1229,   9592,   78498,   664579,   5761455,   50847534 };
    counter_t valid = 0, found = 0;

    for (counter_t sizeIndex = 0; sizeIndex < 9; sizeIndex++) {
        if (sieveSize == known_sieveSize[sizeIndex]) {
            found++;
            if (default_verbose) printf("Checking sieve size %-10d...", known_sieveSize[sizeIndex]);
            struct sieve *sieve   = run_sieve(known_sieveSize[sizeIndex]);
            counter_t primeCount  = count_primes(sieve);
            delete_sieve(sieve);
            counter_t validPrimes = known_PrimeCount[sizeIndex];
            if (primeCount==validPrimes) {
                if (default_verbose) printf("...valid result: %d\n", primeCount);
                valid++;
            }
            else printf("...No valid result. Sievesize %d was expected to have %d primes, but algoritm produced %d primes\n",known_PrimeCount[sizeIndex], validPrimes, primeCount);
        }
    }
    if (found==0) printf("Can't validate the algorithm becauses the number of primes for limit %d is unkown.\n", sieveSize);

    if (valid==1) {
        if (default_verbose) printf("Valid algoritm. Benchmarking...with sieve size %d\n", sieveSize);

        struct timespec start_time, end_time;
        counter_t passes = 0;
        double elapsed_time = 0;
        clock_gettime(CLOCK_MONOTONIC,&start_time);
        while (elapsed_time <= max_time) {
            struct sieve *sieve = run_sieve(sieveSize);
            clock_gettime(CLOCK_MONOTONIC,&end_time);
            passes++;
            elapsed_time = end_time.tv_sec + end_time.tv_nsec*1e-9 - start_time.tv_sec - start_time.tv_nsec*1e-9;
            delete_sieve(sieve);
        }
        printf("rogiervandam;%d;%f;1;algorithm=other,faithful=yes,bits=1\n", passes,elapsed_time);
    }


}
