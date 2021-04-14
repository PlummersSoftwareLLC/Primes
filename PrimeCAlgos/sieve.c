/*
Various prime sieve algorithms speed tested.

April 2021
by Mike Koss (mike@mckoss.com)

Acknowledgements:

@danielspaangberg - For showing me the best gcc optimization flags.

@ednl - Directing me to the very extensive algorithm analysis:
    https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes#Computational_analysis

@Kinematics - Pointing out how to skip 1 in 3 inner loop composites
    in the mod 6 variants.

*/
#include <assert.h>  // assert
#include <math.h>    // sqrt
#include <stdint.h>  // uint32_t
#include <stdio.h>   // printf
#include <stdlib.h>  // calloc
#include <time.h>    // clock, CLOCKS_PER_SEC
#include <string.h>  // strcmp

#include "prime-check.h"

#define TRUE (1)
#define FALSE (0)
#define BOOL int

// 32-bit words seem to yield the highest performance for bit-masking ops
#define WORD uint64_t
#define BITS_PER_WORD (int)(sizeof(WORD) * 8)
#define BYTE unsigned char

#define indexOf(n) ((n) / 2 / BITS_PER_WORD)
#define maskOf(n) (WORD)1 << (n / 2) % BITS_PER_WORD
#define allocOf(n) indexOf(n) + 1

int maxNumber = 1e6;

//
// Simple prime number sieve - one byte per number.
//
// maxNumber - find all primes strictly LESS than this number.
//
int countPrimesBytes(int maxNumber, BOOL fNeedCount) {
    // Starts off zero-initialized.
    BYTE *buffer = (BYTE *)calloc(maxNumber + 1, 1);
    unsigned int maxFactor = sqrt(maxNumber) + 1;

    // We get 2 for "free" since we ignore even numbers.
    int count = 1;
    unsigned int p;

    // Look for next prime
    for (p = 3; p <= maxFactor; p += 2) {
        // A 1 means it's composite - keep searching.
        if (buffer[p]) {
            continue;
        }
        count++;
        // printf("%d, ", p);

        // No need to start less than p^2 since all those
        // multiples have already been marked.
        for (unsigned int m = p * p; m < maxNumber; m += 2 * p) {
            buffer[m] = TRUE;
        }
    }

    // Add all the remaining primes above sqrt(maxNumber)
    if (fNeedCount) {
        for (; p < maxNumber; p += 2) {
            if (buffer[p] == 0) {
                count++;
                // printf("%d, ", q);
            }
        }
    }

    free(buffer);

    return count;
}

//
// Simple prime number sieve - bitmapped.
//
// This is 20% FASTER than the version that allocates half
// the memory (presumably because of inner loop code calculating bit
// masks a little more complicated than the naive version!)
//
// maxNumber - find all primes strictly LESS than this number.
//
// A bitmap where:
//
//   0: Prime
//   1: Composite
//
int countPrimes(int maxNumber, BOOL fNeedCount) {
    // Starts off zero-initialized.
    WORD *buffer = (WORD *)calloc(allocOf(maxNumber), sizeof(WORD));
    unsigned int maxFactor = sqrt(maxNumber) + 1;

    // We get 2 for "free" since we ignore even numbers.
    int count = 1;
    unsigned int p;

    // Look for next prime (ignore even numbers)
    for (p = 3; p <= maxFactor; p += 2) {
        // A 1 bit means it's composite - keep searching.
        if (buffer[indexOf(p)] & maskOf(p)) {
            continue;
        }
        count++;
        // printf("%d, ", p);

        // The following loop is the hotspot for this algorithm
        // executing about 800,000 times for a scan up to 1 million.
        // I tried pre-calculating masks - but that just slowed
        // it down.

        // No need to start less than p^2 since all those
        // multiples have already been marked.
        for (unsigned int m = p * p; m < maxNumber; m += 2 * p) {
            buffer[indexOf(m)] |= maskOf(m);
        }
    }

    // Count all the remaining primes above sqrt(maxNumber)
    if (fNeedCount) {
        for (; p < maxNumber; p += 2) {
            if (buffer[indexOf(p)] & maskOf(p)) {
                continue;
            }
            count++;
            // printf("%d, ", q);
        }
    }

    free(buffer);

    return count;
}

//
// Prime number sieve - full bitmapped but ignoring multiples
// of 2 and 3.  Only testing numbers congrent to 1 or 5 (mod 6).
//
// maxNumber - find all primes strictly LESS than this number.
//
int countPrimes2of6(int maxNumber, BOOL fNeedCount) {
    // Starts off zero-initialized.
    WORD *buffer = (WORD *)calloc(allocOf(maxNumber), sizeof(WORD));
    unsigned int maxFactor = sqrt(maxNumber) + 1;

    // We get 2 and 3 for "free" since we ignore their multiples.
    int count = 2;
    unsigned int p;
    unsigned int step;

    // Look for next prime
    for (p = 5, step = 2; p <= maxFactor; p += step, step = 6 - step) {
        // A 1 bit means it's composite - keep searching.
        if (buffer[indexOf(p)] & maskOf(p)) {
            continue;
        }
        count++;
        // printf("%d, ", p);

        // The following loop is the hotspot for this algorithm
        // executing about 800,000 times for a scan up to 1 million.
        // I tried pre-calculating masks - but that just slowed
        // it down.

        // No need to start less than p^2 since all those
        // multiples have already been marked.
        // Similarly skip all but those prime multiples that
        // are not congruent to 1 or 5.
        unsigned int iStep = step;
        for (unsigned int m = p * p; m < maxNumber; m += p * iStep, iStep = 6 - iStep) {
            buffer[indexOf(m)] |= maskOf(m);
        }
    }

    // Count all the remaining primes above sqrt(maxNumber)
    if (fNeedCount) {
        for (; p < maxNumber; p += step, step = 6 - step) {
            if (buffer[indexOf(p)] & maskOf(p)) {
                continue;
            }
            count++;
            // printf("%d, ", p);
        }
    }

    free(buffer);

    return count;
}

//
// Prime number sieve - full bitmapped but ignoring
// multiples of 2, 3, and 5.  Only testing numbers
// congruent to:
//
//   1, 7, 11, 13, 17, 19, 23, and 29 (mod 30)
//
// maxNumber - find all primes strictly LESS than this number.
//
int countPrimes8of30(int maxNumber, BOOL fNeedCount) {
    // Starts off zero-initialized.
    WORD *buffer = (WORD *)calloc(allocOf(maxNumber), sizeof(WORD));
    unsigned int maxFactor = sqrt(maxNumber) + 1;

    // Only numbers congruent to candidates mod 30 can be prime.
    unsigned int candidates[8] = {1, 7, 11, 13, 17, 19, 23, 29};

    // Cached bitmaps and index offsets for bit twiddling loop.
    WORD masks[BITS_PER_WORD];
    unsigned int offsets[BITS_PER_WORD];

    // Build a stepping map.
    unsigned int steps[8];
    for (int i = 0; i < 8; i++) {
        steps[i] = (candidates[(i + 1) % 8] - candidates[i] + 30) % 30;
    }

    // We get 2, 3 and 5 for "free" since we ignore their multiples.
    int count = 3;
    unsigned int p;

    // First step is from 7 to 11 (steps[1]).
    unsigned int step = 1;

    // Look for next prime
    for (p = 7; p <= maxFactor; p += steps[step], step = (step + 1) % 8) {
        // A 1 bit means it's composite - keep searching.
        if (buffer[indexOf(p)] & maskOf(p)) {
            continue;
        }

        count++;
        // if (p < 1000) printf("%d, ", p);

        // The following loop is the hotspot for this algorithm.
        // No need to start less than p^2 since all those
        // multiples have already been marked.

        // Performance optimization: since the bit mask we or
        // into the word (and the index offset added to the base)
        // RECUR every 64 iterations of this loop (for 64-bit words), we
        // can precalculate them and use them over and over until the end
        // of the sieve array.

        unsigned int base = indexOf(p * p);
        unsigned int cumOffset = 0;

        for (int i = 0; i < BITS_PER_WORD; i++) {
            masks[i] = 0;
        }

        int iUsed = 0;
        int offset = 0;

        for (int i = 0, m = p * p; i < BITS_PER_WORD; i++, m += 2 * p) {
            masks[iUsed] |= maskOf(m);
            offset = indexOf(m + 2 * p) - indexOf(m);
            // Don't advance to a new word unless the next
            // bit is in the same word!
            if (offset != 0) {
                offsets[iUsed] = offset;
                iUsed++;
                cumOffset += offset;
            }
        }

        // In this case, the bits in the last word can just
        // be merged to the first.
        if (offset == 0) {
            masks[0] |= masks[iUsed];
        }

        // In all cases, iUsed will be 1 BEYOND the last mask used.

        // Now just rip through the array or-ing in these masks in an
        // identical pattern.
        unsigned int iStop = indexOf(maxNumber);
        unsigned int i = base;
        for (; i <= iStop - cumOffset;) {
            for (int j = 0; j < iUsed; j++) {
                buffer[i] |= masks[j];
                i = i + offsets[j];
            }
        }

        // Finish last few words being careful about array bounds.
        for (int j = 0; j < iUsed && i <= iStop; j++) {
            buffer[i] |= masks[j];
            i = i + offsets[j];
        }
    }

    // Count all the remaining primes above sqrt(maxNumber)
    if (fNeedCount) {
        for (; p < maxNumber; p += steps[step], step = (step + 1) % 8) {
            if (buffer[indexOf(p)] & maskOf(p)) {
                continue;
            }
            count++;
            // if (p < 1000) printf("%d, ", p);
        }
    }

    free(buffer);

    return count;
}

//
// Only numbers congruent to 1 or 5 (mod 6) are stored.
//
// maxNumber - find all primes strictly LESS than this number.
//
// A bitmap where:
//
//   0: Prime
//   1: Composite
//
// Bits in lsb order:
// 1, 5, 7, 11, 13, 17, 19, 23,
// (2 numbers in every 6, i.e., 1/3 of the numbers stored)
//
#define indexOf6(n) (n / 3) / BITS_PER_WORD
#define maskOf6(n) (WORD)1 << (n / 3) % BITS_PER_WORD
#define allocOf6(n) indexOf6(n) + 1
int countPrimesMod6(int maxNumber, BOOL fNeedCount) {
    // Starts off zero-initialized.
    WORD *buffer = (WORD *)calloc(allocOf6(maxNumber), sizeof(WORD));
    unsigned int maxFactor = sqrt(maxNumber) + 1;

    // We get 2 and 3 for "free".
    int count = 2;
    unsigned int p;
    unsigned int step;

    // Alternate steps by 2 and 4.
    for (p = 5, step = 2; p <= maxFactor; p += step, step = 6 - step) {
        // A 1 bit means it's composite - keep searching.
        if (buffer[indexOf6(p)] & maskOf6(p)) {
            continue;
        }
        count++;
        // printf("%d, ", p);

        // The following loop is the hotspot for this algorithm
        // executing about 800,000 times for a scan up to 1 million.
        // I tried pre-calculating masks - but that just slowed
        // it down.

        // No need to start less than p^2 since all those
        // multiples have already been marked.
        // Similarly skip all but those prime multiples that
        // are not congruent to 1 or 5.
        unsigned int iStep = step;
        for (unsigned int m = p * p; m < maxNumber; m += p * iStep, iStep = 6 - iStep) {
            buffer[indexOf6(m)] |= maskOf6(m);
        }
    }

    // Add all the remaining primes above sqrt(maxNumber)
    // Since this is our hotspot - unroll every other loop iteration.
    if (fNeedCount) {
        for (; p < maxNumber; p += step, step = 6 - step) {
            if (buffer[indexOf6(p)] & maskOf6(p)) {
                continue;
            }
            count++;
            // printf("%d, ", q);
        }
    }

    free(buffer);

    return count;
}

void timedTest(int secs, int primeFinder(int, int), char *title) {
    clock_t startTicks = clock();
    clock_t currentTicks;
    int passes = 1;

    clock_t limitTicks = secs * CLOCKS_PER_SEC + startTicks;

    // Check first for accuracy.
    int primeCount = primeFinder(maxNumber, TRUE);
    for (int i = 0; i < NUM_CHECKS; i++) {
        if (primeChecks[i].n == maxNumber) {
            if (primeCount != primeChecks[i].piN) {
                printf("%s Expected %d primes - but found %d!\n", title,
                    primeChecks[i].piN, primeCount);
                assert(FALSE);
            }
        }
    }

    while (TRUE) {
        currentTicks = clock();
        if (currentTicks >= limitTicks) {
            break;
        }
        passes++;
        // Dave's Garage algos did not compute the total count
        // in the timed loop - just implemented the sieve.
        primeFinder(maxNumber, FALSE);
    }

    float elapsed = (float) currentTicks - startTicks;

    printf(
        "%30s: Found %d primes in %5d passes in %0.1f seconds (%0.3f ms per "
        "pass).\n",
        title, primeCount, passes, elapsed / CLOCKS_PER_SEC,
        elapsed / passes * 1000 / CLOCKS_PER_SEC);
    // Don't buffer output since there is a lot of delay between tests.
    fflush(stdout);
}

void usage(char *name) {
    fprintf(stderr, "Usage: %s --help --secs <s> --size <N>\n", name);
    exit(1);
}

int main(int argc, char *argv[]) {
    int secs = 5;

    for (int iarg = 1; iarg < argc; iarg++) {
        if (strcmp(argv[iarg], "--help") == 0) {
            usage(argv[0]);
        } else if (strcmp(argv[iarg], "--secs") == 0) {
            if (iarg + 1 == argc) {
                fprintf(stderr,
                        "Error: Missing number of seconds to measure.\n");
                usage(argv[0]);
            }
            iarg++;
            if (sscanf(argv[iarg], "%d", &secs) != 1 || secs < 1) {
                fprintf(stderr, "Error: Invalid measurement time: %s\n",
                        argv[iarg]);
                usage(argv[0]);
            }
        } else if (strcmp(argv[iarg], "--size") == 0) {
            if (iarg + 1 == argc) {
                fprintf(stderr,
                        "Error: Missing size (range) of sieve.\n");
                usage(argv[0]);
            }
            iarg++;
            if (sscanf(argv[iarg], "%d", &maxNumber) != 1 || maxNumber < 1000) {
                fprintf(stderr, "Error: Invalid sieve size: %s\n",
                        argv[iarg]);
                usage(argv[0]);
            }
        } else {
            fprintf(stderr, "Error: Unknown argument: %s\n", argv[iarg]);
            usage(argv[0]);
        }
    }

    printf("Calculate primes up to %d.\n", maxNumber);
    printf("Timer resolution: %d ticks per second.\n", CLOCKS_PER_SEC);
    printf("Word size: %d bits.\n\n", BITS_PER_WORD);

    timedTest(secs, countPrimesBytes, "Byte-map - 1 of 2 tested");
    timedTest(secs, countPrimes, "Bit-map - 1 of 2 tested");
    timedTest(secs, countPrimes2of6, "Bit-map - 2 of 6 tested");
    timedTest(secs, countPrimes8of30, "Bit-map - 8 of 30 tested");
    timedTest(secs, countPrimesMod6, "1/3 Bit-map");

    return (0);
}
