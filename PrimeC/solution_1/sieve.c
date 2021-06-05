/*
Drag-race entry for pure C version of prime sieve.

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
#include <stdint.h>  // uint32_t
#include <stdio.h>   // printf
#include <stdlib.h>  // calloc
#include <time.h>    // clock, CLOCKS_PER_SEC
#include <string.h>  // strcmp

#include "prime-check.h"

#define TRUE (1)
#define FALSE (0)
#define BOOL int

#define WORD uint64_t
#define BITS_PER_WORD (int)(sizeof(WORD) * 8)
#define BYTE unsigned char

#define indexOf(n) ((n) / 2 / BITS_PER_WORD)
#define maskOf(n) (WORD)1 << (n / 2) % BITS_PER_WORD
#define allocOf(n) indexOf(n) + 1

int maxNumber = 1e6;
BOOL fDebug = FALSE;

typedef struct {
    int maxNumber;
    WORD *buffer;
} SIEVE;

unsigned int usqrt(int n)
{
    unsigned int x;
    unsigned int xLast;

    xLast = 0;
    x = n / 2;

    while (x != xLast) {
        xLast = x;
        x = (x + n / x) / 2;
    }
    return x;
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
SIEVE *primes8of30(int maxNumber) {
    // Starts off zero-initialized.
    WORD *buffer = (WORD *)calloc(allocOf(maxNumber), sizeof(WORD));
    unsigned int maxFactor = usqrt(maxNumber) + 1;

    // Allocate sieve "object"
    SIEVE *psieve = calloc(1, sizeof(SIEVE));
    psieve->buffer = buffer;
    psieve->maxNumber = maxNumber;

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

    // The wrap-around buffer pattern is expected to overwrite the
    // 7 and 11 primes as composite, erroneously.
    buffer[indexOf(7)] &= ~maskOf(7);
    buffer[indexOf(11)] &= ~maskOf(11);

    return psieve;
}

int countPrimes(SIEVE *psieve) {
    WORD *buffer = psieve->buffer;
    int maxNumber = psieve->maxNumber;

    // 2, 3, and 5 are known prime
    int count = 3;
    if (fDebug) {
        printf("2, 3, 5, ");
    }

    // Only numbers congruent to candidates mod 30 can be prime.
    unsigned int candidates[8] = {1, 7, 11, 13, 17, 19, 23, 29};

    // Build a stepping map.
    unsigned int steps[8];
    for (int i = 0; i < 8; i++) {
        steps[i] = (candidates[(i + 1) % 8] - candidates[i] + 30) % 30;
    }

    // First step is from 7 to 11 (steps[1]).
    unsigned int step = 1;

    for (unsigned int p = 7; p < maxNumber; p += steps[step], step = (step + 1) % 8) {
        if ((buffer[indexOf(p)] & maskOf(p)) == 0) {
            count++;
            if (fDebug && p < 1000) {
                printf("%d, ", p);
            }
        }
    }

    if (fDebug) {
        printf("...\n");
    }

    return count;
}

// Free up resources for sieve object
void freeSieve(SIEVE *psieve) {
    free(psieve->buffer);
    free(psieve);
}

void timedTest(int secs, SIEVE *primeFinder(int), char *title) {
    clock_t startTicks = clock();
    clock_t currentTicks;

    // One pass confirms accuracy.
    int passes = 1;

    clock_t limitTicks = secs * CLOCKS_PER_SEC + startTicks;

    // Check first for accuracy against known values.
    SIEVE *psieve = primeFinder(maxNumber);
    int primeCount = countPrimes(psieve);
    freeSieve(psieve);

    if (fDebug) {
        printf("Found %d primes up to %d.\n\n", primeCount, maxNumber);
    }

    for (int i = 0; i < NUM_CHECKS; i++) {
        if (primeChecks[i].n == maxNumber) {
            if (primeCount != primeChecks[i].piN) {
                printf("%s Expected %d primes - but found %d!\n", title,
                    primeChecks[i].piN, primeCount);
                assert(FALSE);
            }
        }
    }

    fflush(stdout);

    while (TRUE) {
        currentTicks = clock();
        if (currentTicks >= limitTicks) {
            break;
        }
        passes++;
        psieve = primeFinder(maxNumber);
        freeSieve(psieve);
    }

    float elapsed = (float) currentTicks - startTicks;

    printf("%s;%d;%0.1f;1;algorithm=wheel,faithful=yes,bits=1\n", title, passes, elapsed / CLOCKS_PER_SEC);
    fflush(stdout);
}

void usage(char *name) {
    fprintf(stderr, "Usage: %s --help --secs <s> --size <N> --debug\n", name);
    exit(1);
}

int main(int argc, char *argv[]) {
    int secs = 5;

    for (int iarg = 1; iarg < argc; iarg++) {
        if (strcmp(argv[iarg], "--help") == 0) {
            usage(argv[0]);
        } else if (strcmp(argv[iarg], "--debug") == 0) {
            fDebug = TRUE;
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

    if (fDebug) {
        printf("Calculate primes up to %d.\n", maxNumber);
        printf("Timer resolution: %d ticks per second.\n", (int) CLOCKS_PER_SEC);
        printf("Word size: %d bits.\n", BITS_PER_WORD);
        printf("\n");
    }

    timedTest(secs, primes8of30, "mckoss-c830");

    return (0);
}