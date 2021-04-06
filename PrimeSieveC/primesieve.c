#include <stdio.h>    // printf
#include <stdbool.h>  // bool, true, false
#include <time.h>     // clock, CLOCKS_PER_SEC

#define LIM  (1000000U)  // count number of primes which are smaller than this
#define SIZE (LIM >> 1)  // uneven number count
#define FIVESEC (CLOCKS_PER_SEC * 5)

typedef struct {
    unsigned int limit, count;
} VALID;

const static VALID valid[] = {
    {          10U, 4U         },
    {         100U, 25U        },
    {        1000U, 168U       },
    {       10000U, 1229U      },
    {      100000U, 9592U      },
    {     1000000U, 78498U     },
    {    10000000U, 664579U    },
    {   100000000U, 5761455U   },
    {  1000000000U, 50847534U  },
    // { 10000000000U, 455052511U },
};

static unsigned char isprime[SIZE] = {0};  // only keep track of uneven numbers

static bool isvalid(unsigned int limit, unsigned int count)
{
    for (unsigned int i = 0; i < sizeof(valid) / sizeof(*valid); ++i) {
        if (valid[i].limit == limit) {
            return valid[i].count == count;
        }
    }
    return false;
}

int main(void)
{
    unsigned int i, fac, nxt, dif, count, passes = 0;
    clock_t stop = clock() + FIVESEC;

    do {

        // Init
        for (i = 1; i < SIZE; ++i) {
            isprime[i] = 1;
        }

        // Sieve
        fac = 1;  // first factor to try is 3 = index 1 in the uneven numbers
        nxt = 4;  // first multiple to scratch is factor * 3 = index 4 (skip factor * 2 is which is even)
        dif = 3;  // step to next scratch is factor * 2 = index difference 3 (skip even multiples)
        while (nxt < SIZE) {
            if (isprime[fac]) {
                for (i = nxt; i < SIZE; i += dif) {
                    isprime[i] = 0;  // uneven multiple = not a prime
                }
            }
            ++fac;
            nxt += 3;
            dif += 2;
        }

        // Count loops
        ++passes;

    } while (clock() < stop);

    // Count primes
    count = (LIM >= 2);  // 2 is a prime
    for (i = 1; i < SIZE; ++i) {
        count += isprime[i];
    }

    // Result
    printf("Passes: %u, Limit: %u, Count: %u, Valid: %s\n",
        passes, LIM, count, isvalid(LIM, count) ? "yes" : "no");
    return 0;
}
