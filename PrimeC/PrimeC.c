// ---------------------------------------------------------------------------
// PrimeC.c : based on Dave's Garage Prime Sieve in C++
// ---------------------------------------------------------------------------

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

#define FALSE 0
#define TRUE 1

#define SIEVE_SIZE 1000000L

struct valid_result { long upto; long count; } ;
struct valid_result valid_results[]=
      {
            {          10L, 4         },                // Historical data for validating our results - the number of primes
            {         100L, 25        },               // to be found under some limit, such as 168 primes under 1000
            {        1000L, 168       },
            {       10000L, 1229      },
            {      100000L, 9592      },
            {     1000000L, 78498     },
            {    10000000L, 664579    },
            {   100000000L, 5761455   },
            {  1000000000L, 50847534  },
            { 10000000000L, 455052511 }
      };

u_int64_t field[SIEVE_SIZE/64];

int resetBits()
{
    memset(field,~0,SIEVE_SIZE/64*8);
}

u_int64_t getBit(long n)
{
    return field[n>>6]&(1L<<(n%64));
}

void unsetBit(long n)
{
    field[n>>6]&=~(1L<<(n%64));
}

int countPrimes()
{
    int count = 1;
    for (int i = 3; i < SIEVE_SIZE; i+=2)
        if (getBit(i)) count++;
    return count;
}

int validateResults()
{
    for (int i=0;i<sizeof(valid_results)/sizeof(struct valid_result);i++)
        if (valid_results[i].upto==SIEVE_SIZE)
    	    return valid_results[i].count == countPrimes();
    return FALSE;
}

void runSieve()
{
    int factor = 3;
    int q = sqrt(SIEVE_SIZE);

    while (factor <= q)
    {
        for (int num = factor * factor; num < SIEVE_SIZE; num += factor * 2)
            unsetBit(num);

        // searching for the next prime factor
	for (factor+=2;factor<SIEVE_SIZE;factor+=2)
		if (getBit(factor)) break;
    }
}

void printResults(int showResults, double duration, int passes)
{
    int count = 1;
    if (showResults)
    {
        printf("2, ");

        for (int num = 3; num <= SIEVE_SIZE; num+=2)
        {
            if (getBit(num))
                printf("%d, ", num);
        }

        printf("\n");
    }

    printf("Passes: %d, Time: %lf, Avg: %lf, Limit: %ld, Count: %d, Valid: %d\n", 
                 passes,
                 duration,
                 duration / passes,
                 SIEVE_SIZE,
                 countPrimes(),
                 validateResults());
}


int main()
{
    int passes = 0;
    clock_t start=clock();

    while (TRUE)
    {
        resetBits();
        runSieve();
        passes++;
        if ((clock()-start) >= 5*CLOCKS_PER_SEC) break;
    } 

    printResults(FALSE, (clock()-start)/CLOCKS_PER_SEC, passes);
}
