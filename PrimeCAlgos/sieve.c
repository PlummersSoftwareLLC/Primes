/*
Various prime sieve algorithms speed tested.

April 2021
by Mike Koss (mike@mckoss.com)
*/
#include <time.h>       // clock, CLOCKS_PER_SEC
#include <stdio.h>      // printf
#include <stdlib.h>     // calloc
#include <assert.h>     // assert
#include <math.h>       // sqrt

#define MEASUREMENT_SECS (5)
#define TRUE (1)
#define FALSE (0)

#define WORD unsigned long
#define BITS_PER_WORD (sizeof(WORD) * 8)
#define BYTE unsigned char

// Primes to one million.
#define MAX_NUMBER 1000000L
#define EXPECTED_PRIMES 78498L

//
// Simple prime number sieve - one byte per number.
//
// maxNumber - find all primes strictly LESS than this number.
//
int countPrimesBytes(int maxNumber, int fNeedCount) {
   // Starts off zero-initialized.
   BYTE *buffer = (BYTE *) calloc(maxNumber + 1, 1);
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
#define indexOf(n) n / BITS_PER_WORD
#define maskOf(n) (WORD) 1 << n % BITS_PER_WORD
#define allocOf(n) indexOf(n) + 1
int countPrimes(int maxNumber, int fNeedCount) {
   // Starts off zero-initialized.
   WORD *buffer = (WORD *) calloc(allocOf(maxNumber), sizeof(WORD));
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
#define indexOf(n) n / BITS_PER_WORD
#define maskOf(n) (WORD) 1 << n % BITS_PER_WORD
#define allocOf(n) indexOf(n) + 1
int countPrimes2of6(int maxNumber, int fNeedCount) {
   // Starts off zero-initialized.
   WORD *buffer = (WORD *) calloc(allocOf(maxNumber), sizeof(WORD));
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
      // Note: we are setting bits we will never test (are not
      // congruent to 1 or 5 mod 6).  But, the extra check for
      // mod 6 in the inner loop here would make it slower.
      for (unsigned int m = p * p; m < maxNumber; m += 2 * p) {
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
#define indexOf(n) n / BITS_PER_WORD
#define maskOf(n) (WORD) 1 << n % BITS_PER_WORD
#define allocOf(n) indexOf(n) + 1
int countPrimes8of30(int maxNumber, int fNeedCount) {
   // Starts off zero-initialized.
   WORD *buffer = (WORD *) calloc(allocOf(maxNumber), sizeof(WORD));
   unsigned int maxFactor = sqrt(maxNumber) + 1;

   // Only numbers congruent to candidates mod 30 can be prime.
   unsigned int candidates[8] = {1, 7, 11, 13, 17, 19, 23, 29};

   // Build a quick-lookup map.
   unsigned int isModCandidate[30] = {FALSE};
   for (int i = 0; i < 8; i++) {
      isModCandidate[i] = TRUE;
   }

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
      // printf("%d, ", p);

      // The following loop is the hotspot for this algorithm.
      // No need to start less than p^2 since all those
      // multiples have already been marked.
      for (unsigned int m = p * p; m < maxNumber; m += 2 * p) {
         buffer[indexOf(m)] |= maskOf(m);
      }
   }

   // Count all the remaining primes above sqrt(maxNumber)
   if (fNeedCount) {
      for (; p < maxNumber; p += steps[step], step = (step + 1) % 8) {
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
// Only odd (mod 2) prime number sieve.
//
// maxNumber - find all primes strictly LESS than this number.
//
// A bitmap where:
//
//   0: Prime
//   1: Composite
//
// Bits in lsb order:
// 1, 3, 5, 7, 9, ... 
//
// 1-based - only odd numbers
// HACK - Remove -1 since integer division ROUNDS DOWN ANYWAY.
// SPEEDUP BY 5%!
#define indexOf2(n) (n) / 2 / BITS_PER_WORD
#define maskOf2(n) (WORD) 1 << ((n) / 2) % BITS_PER_WORD
#define allocOf2(n) indexOf2(n) + 1
int countPrimesMod2(int maxNumber, int fNeedCount) {
   // Starts off zero-initialized.
   WORD *buffer = (WORD *) calloc(allocOf2(maxNumber), sizeof(WORD));
   unsigned int maxFactor = sqrt(maxNumber) + 1;

   // We get 2 for "free".
   int count = 1;
   unsigned int p;

   // Look for next prime
   for (p = 3; p <= maxFactor; p += 2) {
      // A 1 bit means it's composite - keep searching.
      if (buffer[indexOf2(p)] & maskOf2(p)) {
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
         buffer[indexOf2(m)] |= maskOf2(m);
      }
   }

   // Add all the remaining primes above sqrt(maxNumber)
   if (fNeedCount) {
      for (unsigned int q = p; q < maxNumber; q += 2) {
         if (buffer[indexOf2(q)] & maskOf2(q)) {
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
// Only numbers congruent to 1 or 5 (mod 6) are stored.
//
// maxNumber - find all primes strictly LESS than this number.
//
// A bitmap where:
//
//   0: Prime
//   1: Composite
//
//
// Bits in lsb order:
// 1, 5, 7, 11, 13, 17, 19, 23, 
// (2 numbers in every 6, i.e., 1/3 of the numbers stored)
//
// Using 32-bit WORDS
// (n - 1) / 3 / 32 => word address in buffer
// ((n - 1) / 3) % 32 => bit address in byte
//
#define indexOf6(n) (n/3) / BITS_PER_WORD
#define maskOf6(n) (WORD) 1 << (n/3) % BITS_PER_WORD
#define allocOf6(n) indexOf6(n) + 1
int countPrimesMod6(int maxNumber, int fNeedCount) {
   // Starts off zero-initialized.
   WORD *buffer = (WORD *) calloc(allocOf6(maxNumber), sizeof(WORD));
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
      for (unsigned int m = p * p; m < maxNumber; m += 2 * p) {
         unsigned int mod = m % 6;
         if (mod == 1 || mod == 5) {
            buffer[indexOf6(m)] |= maskOf6(m);
         }
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

void timedTest(int primeFinder(int, int), char *title) {
   clock_t startTicks;
   clock_t currentTicks;
   int passes = 1;

   startTicks = clock();

   long limitTicks = MEASUREMENT_SECS * CLOCKS_PER_SEC + startTicks;

   // Check first for accuracy.
   int primeCount = primeFinder(MAX_NUMBER, TRUE);
   if (primeCount != EXPECTED_PRIMES) {
      printf("%s Expected %ld primes - but found %ld!\n",
               title, EXPECTED_PRIMES, primeCount);
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

   printf("%30s: %5d passes completed in %d seconds (%0.3f ms per pass).\n",
          title, passes, MEASUREMENT_SECS, (float) MEASUREMENT_SECS / passes * 1000);
   // Don't buffer output since there is a lot of delay between tests.
   fflush(stdout);
}

int main() {
   printf("Calculate primes up to %d.\n", MAX_NUMBER);
   printf("Timer resolution: %d ticks per second.\n", CLOCKS_PER_SEC);
   printf("Word size: %d bits.\n\n", BITS_PER_WORD);

   timedTest(countPrimesBytes, "Byte-map - 1 of 2 tested");
   timedTest(countPrimes, "Bit-map - 1 of 2 tested");
   timedTest(countPrimes2of6, "Bit-map - 2 of 6 tested");
   timedTest(countPrimes8of30, "Bit-map - 8 of 30 tested");
   timedTest(countPrimesMod2, "1/2 Bit-map");
   timedTest(countPrimesMod6, "1/3 Bit-map");

   return(0);
}

