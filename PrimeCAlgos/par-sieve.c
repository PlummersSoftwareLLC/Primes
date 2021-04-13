/*
Various prime sieve algorithms speed tested.

April 2021
by Mike Koss (mike@mckoss.com)
*/
#include <assert.h>   // assert
#include <math.h>     // sqrt
#include <pthread.h>  // pthread_t
#include <stdint.h>   // uint32_t
#include <stdio.h>    // printf
#include <stdlib.h>   // calloc
#include <string.h>   // strncmp
#include <time.h>     // clock, CLOCKS_PER_SEC

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

// Primes to ten million.
#define MAX_NUMBER 10000000
#define EXPECTED_PRIMES 664579

BOOL fShowWork = FALSE;

int numThreads = 4;

typedef struct {
    BYTE *buffer;
    int pStart;
    int pEnd;
} SWEEP_VARS;

//
// The parallel processing part - marking composites for primes between
// pStart to pEnd.  Number is prime iff buffer[p] == 0.
//
void *sweep(SWEEP_VARS *psweepVars) {
    int pEnd = psweepVars->pEnd;
    BYTE *buffer = psweepVars->buffer;

    for (int p = psweepVars->pStart; p < pEnd; p += 2) {
        if (buffer[p]) {
            continue;
        }
        int count = 0;
        // This is the inner-most loop gating performance.
        for (unsigned int m = p * p; m < MAX_NUMBER; m += 2 * p) {
            buffer[m] = TRUE;
            count++;
        }
        // printf("%d marks %d (to %d)\n", p, count, pEnd);
    }

    return NULL;
}

//
// Simple prime number sieve - one byte per number.
//
// maxNumber - find all primes strictly LESS than this number.
//
// This parallel version seeks to utilize multiple numThreads to
// speed up the sieve.  The approach will be:
//
// - On main thread, find the primes up to sqrt(maxNumber).
// - Calculate how much "work" is needed to mark all the
//   composites from these primes.
// - Divvy the work uniformly to N workers.
// - Once all workers are complete, use the main thread to
//   count the primes.
//
//  Note that there are no locks other than waiting for thread
//  completion, as all writes are idempotent (don't matter if)
//  two numThreads try to write a 1 in the same location.
//
int countPrimesBytesPar(int maxNumber, BOOL fNeedCount) {
    // Starts off zero-initialized.
    BYTE *buffer = (BYTE *)calloc(maxNumber + 1, 1);
    unsigned int maxPrime = sqrt(maxNumber);
    unsigned int sqrtMaxPrime = sqrt(maxPrime);

    pthread_t *threads =
        (pthread_t *)calloc((size_t)numThreads, sizeof(pthread_t));
    SWEEP_VARS *vars =
        (SWEEP_VARS *)calloc((size_t)numThreads, sizeof(SWEEP_VARS));

    // We get 2 for "free" since we ignore even numbers.
    int count = 1;
    unsigned int p;

    // Quick sieve to mark all the primes to maxPrime
    for (p = 3; p <= sqrtMaxPrime; p += 2) {
        // A 1 means it's composite - keep searching.
        if (buffer[p]) {
            continue;
        }
        // printf("%d, ", p);

        // No need to start less than p^2 since all those
        // multiples have already been marked.
        for (unsigned int m = p * p; m <= maxPrime; m += 2 * p) {
            buffer[m] = TRUE;
        }
    }

    int totalWork = 0;

    for (p = 3; p <= maxPrime; p += 2) {
        // Will need to mark (max - p^2)/p == max/p - p composites.
        if (buffer[p] == 0) {
            totalWork += (maxNumber / p - p) / 2;
            // printf("%d, ", p);
        }
    }

    // printf("\nTotal work: %d\n", totalWork);

    int workRemaining = totalWork;

    p = 3;
    for (int i = 0; i < numThreads && p <= maxPrime; i++) {
        int work = 0;
        vars[i].buffer = buffer;
        vars[i].pStart = p;

        if (i == numThreads - 1) {
            p = maxPrime;
            work = workRemaining;
        } else {
            while (work < totalWork / numThreads && p <= maxPrime) {
                work += (maxNumber / p - p) / 2;
                p += 2;
                // Advance to next prime.
                while (buffer[p] != 0 && p <= maxPrime) {
                    p += 2;
                }
            }
        }

        if (fShowWork && fNeedCount) {
            printf("%d. Scan from %d to %d (%0.3f M-marks).\n", i + 1,
                   vars[i].pStart, p - 2, (float)work / 1000000);
        }

        workRemaining -= work;
        vars[i].pEnd = p;

        // Do the last tranche on the main thread.
        if (i == numThreads - 1) {
            sweep(&vars[i]);
        } else {
            pthread_create(&threads[i], NULL, (void *(*)(void *))sweep, &vars[i]);
        }
    }

    fflush(stdout);

    // Wait for all threads to terminate.
    for (int i = 0; i < numThreads; i++) {
        pthread_join(threads[i], NULL);
    }

    // Add all the remaining primes above sqrt(maxNumber)
    if (fNeedCount) {
        for (p = 3; p < maxNumber; p += 2) {
            if (buffer[p] == 0) {
                count++;
                // if (p < 1000) printf("%d, ", p);
            }
        }
    }

    free(buffer);
    free(threads);
    free(vars);

    return count;
}

void timedTest(int secs, int primeFinder(int, int), char *title) {
    clock_t startTicks;
    clock_t currentTicks;
    int passes = 1;

    startTicks = clock();

    long limitTicks = secs * CLOCKS_PER_SEC + startTicks;

    // Check first for accuracy.
    int primeCount = primeFinder(MAX_NUMBER, TRUE);

    if (primeCount != EXPECTED_PRIMES) {
        printf("%s Expected %d primes - but found %d!\n", title,
               EXPECTED_PRIMES, primeCount);
        assert(FALSE);
    }

    while (TRUE) {
        currentTicks = clock();
        if (currentTicks >= limitTicks) {
            break;
        }
        passes++;
        // Dave's Garage algos did not compute the total count
        // in the timed loop - just implemented the sieve.
        primeFinder(MAX_NUMBER, FALSE);
    }

    printf("%30s: %5d passes completed in %d seconds (%0.3f ms per pass) (%d threads).\n",
           title, passes, secs,
           (float)secs / passes * 1000, numThreads);
    // Don't buffer output since there is a lot of delay between tests.
    fflush(stdout);
}

void usage(char *name) {
    fprintf(stderr, "Usage: %s --help --secs <s> --threads <n> --show-work\n", name);
    exit(1);
}

int main(int argc, char *argv[]) {
    int secs = 5;

    for (int iarg = 1; iarg < argc; iarg++) {
        if (strcmp(argv[iarg], "--help") == 0) {
            usage(argv[0]);
        } else if (strcmp(argv[iarg], "--threads") == 0) {
            if (iarg + 1 == argc) {
                fprintf(stderr, "Error: Missing number of numThreads.\n");
                usage(argv[0]);
            }
            iarg++;
            if (sscanf(argv[iarg], "%d", &numThreads) != 1 || numThreads < 1 ||
                numThreads > 100) {
                fprintf(stderr, "Error: Invalid number of numThreads: %s\n",
                        argv[iarg]);
                usage(argv[0]);
            }
        } else if (strcmp(argv[iarg], "--secs") == 0) {
            if (iarg + 1 == argc) {
                fprintf(stderr, "Error: Missing number of seconds to measure.\n");
                usage(argv[0]);
            }
            iarg++;
            if (sscanf(argv[iarg], "%d", &secs) != 1 || secs < 1) {
                fprintf(stderr, "Error: Invalid measurement time: %s\n", argv[iarg]);
                usage(argv[0]);
            }
        } else if (strcmp(argv[iarg], "--show-work") == 0) {
            fShowWork = TRUE;
        } else {
            fprintf(stderr, "Error: Unknown argument: %s\n", argv[iarg]);
            usage(argv[0]);
        }
    }

    printf("Calculate primes up to %d.\n", MAX_NUMBER);
    printf("Timer resolution: %d ticks per second.\n", CLOCKS_PER_SEC);
    printf("Word size: %d bits.\n", BITS_PER_WORD);
    printf("\n");
    fflush(stdout);

    timedTest(secs, countPrimesBytesPar, "Parallel Byte-map - 1 of 2 tested");

    return (0);
}
