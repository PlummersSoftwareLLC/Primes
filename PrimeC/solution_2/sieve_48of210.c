// Serial code prime sieve by Daniel Spangberg
// Only count 48 of 210 (2*3*5*7), there are only 48 numbers not divisible by 2,3,5,7 within this range
// Discussions and code sharing with @mckoss & @Kinematics
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <math.h>

#ifdef COMPILE_64_BIT
#define TYPE uint64_t
#define MASK 0x3FU
#define SHIFT 6U
#else
#define TYPE uint32_t
#define MASK 0x1FU
#define SHIFT 5U
#endif

// Steps array for finding the next number not divisible by 2,3,5,7
// Prehalved for inner loops
static unsigned int steps[48]={
5,1,2,1,2,3,1,3,2,1,2,3,3,1,3,2,1,3,2,3,4,2,1,2,1,
 2,4,3,2,3,1,2,3,1,3,3,2,1,2,3,1,3,2,1,2,1,5,1
};

struct sieve_state {
  TYPE *a;
  unsigned int maxints;
};

struct sieve_state *create_sieve(int maxints) {
  struct sieve_state *sieve_state=malloc(sizeof *sieve_state);
  // We need to store only odd integers, so only half the number of integers
  sieve_state->a=calloc(maxints/2/sizeof(TYPE)+1,sizeof(TYPE));
  sieve_state->maxints=maxints;
  return sieve_state;
}

void delete_sieve(struct sieve_state *sieve_state) {
  free(sieve_state->a);
  free(sieve_state);
}

void run_sieve(struct sieve_state *sieve_state) {
  unsigned int maxints=sieve_state->maxints;
  TYPE *a=sieve_state->a;
  unsigned int maxintsh=maxints>>1;
  unsigned int q=(unsigned int)sqrt(maxints)+1U;
  unsigned int step=1U; // From 11 to 13
  unsigned int inc=steps[step]; // Next increment in steps array
  // Only check integers not divisible by 2, 3, 5, or 7
  unsigned int factorh=11U>>1U; // We already have 2, 3, 5, and 7
  unsigned int qh=q>>1U;
  while (factorh<=qh) {
    // Search for next prime
    if (a[factorh>>SHIFT]&((TYPE)1<<(factorh&MASK))) {
      factorh+=inc;
      if (++step==48U) step=0U; // End of steps array, start from the beginning
      inc=steps[step];
      continue;
    }
    // Mask all integer multiples of this prime
    unsigned int factor=(factorh<<1U)+1U;
    for (unsigned int i=(factor*factor)>>1U; i<maxintsh; i+=factor)
      a[i>>SHIFT]|=(TYPE)1<<(i&MASK);
    factorh+=inc;
    if (++step==48U) step=0U; // End of steps array, start from the beginning
    inc=steps[step];
  }
}

unsigned int count_primes(struct sieve_state *sieve_state) {
  unsigned int maxints=sieve_state->maxints;
  TYPE *a=sieve_state->a;
  unsigned int ncount=4; // We already have 2, 3, 5, and 7 ...
  unsigned int factor=11; // ...
  unsigned int step=1; // From 11 to 13
  unsigned int inc=steps[step]<<1U; // Next increment in steps array
  while (factor<=maxints) {
    if (!(a[factor>>(SHIFT+1U)]&((TYPE)1<<((factor>>1U)&MASK))))
      ncount++;
    factor+=inc;
    if (++step==48U) step=0U; // End of steps array, start from the beginning
    inc=steps[step]<<1U;
  }
  return ncount;
}

int main(int argc, char **argv) {
  int maxints=1000000;
  struct timespec t,t2;
  if (argc>1)
    sscanf(argv[1],"%d",&maxints);
  int valid_primes;
  switch(maxints) {
  case 10:
    valid_primes=4;
    break;
  case 100:
    valid_primes=25;
    break;
  case 1000:
    valid_primes=168;
    break;
  case 10000:
    valid_primes=1229;
    break;
  case 100000:
    valid_primes=9592;
    break;
  case 1000000:
    valid_primes=78498;
    break;
  case 10000000:
    valid_primes=664579;
    break;
  case 100000000:
    valid_primes=5761455;
    break;
  case 1000000000:
    valid_primes=50847534;
    break;
  default:
    valid_primes=-1;
  }
  int passes=0;
  // The initial time
  clock_gettime(CLOCK_MONOTONIC,&t);
  struct sieve_state *sieve_state;
  while (1) {
    sieve_state=create_sieve(maxints);
    run_sieve(sieve_state);
    passes++;
    clock_gettime(CLOCK_MONOTONIC,&t2);
    double elapsed_time=t2.tv_sec+t2.tv_nsec*1e-9-t.tv_sec-t.tv_nsec*1e-9;
    if (elapsed_time>=5.) {
      // Count the number of primes and validate the result
      int nprimes=count_primes(sieve_state);
      //printf("valid=%d ",(nprimes==valid_primes));
      printf("danielspaangberg_48of210;%d;%f;1;algorithm=wheel,faithful=yes,bits=1\n", passes,elapsed_time);
      break;
    }
    delete_sieve(sieve_state);
  }
  return 0;
}
