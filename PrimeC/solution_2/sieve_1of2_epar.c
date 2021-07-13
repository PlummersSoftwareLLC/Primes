// Embarrassingly parallel code prime sieve by Daniel Spangberg
// Run multiple threads independend of each other
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <math.h>
#ifdef _OPENMP
#include <omp.h>
#endif

#ifdef COMPILE_64_BIT
#define TYPE uint64_t
#define MASK 0x3FU
#define SHIFT 6U
#else
#define TYPE uint32_t
#define MASK 0x1FU
#define SHIFT 5U
#endif

struct sieve_state {
  TYPE *a;
  unsigned int maxints;
};

struct sieve_state *create_sieve(int maxints) {
  struct sieve_state *sieve_state=malloc(sizeof *sieve_state);
  // We need to store only odd integers, so only half the number of integers
  sieve_state->a=calloc((maxints + 16*sizeof(TYPE) - 1) / 16 / sizeof(TYPE), sizeof(TYPE));
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
  unsigned int maxintsh=maxints>>1U;
  unsigned int q=(unsigned int)sqrt(maxints)+1U;
  // Only check odd integers
  unsigned int factorh=1U;
  unsigned int qh=q>>1U;
  while (factorh<=qh) {
    // Search for next prime
    unsigned int i, factor;
    // If the bit is set we know it is not prime, so continue searching
    for (i=factorh; i<maxintsh; i++)
      if (!(a[i>>SHIFT]&((TYPE)1<<(i&MASK))))
	break;
    factor=(i<<1U)+1U;
    factorh=i+1;
    // Mask all integer multiples of this prime
    i=(factor*factor)>>1U;
    for (; i<maxintsh; i+=factor)
      a[i>>SHIFT]|=(TYPE)1<<(i&MASK);
  }
}

int count_primes(struct sieve_state *sieve_state) {
  int maxints=sieve_state->maxints;
  TYPE *a=sieve_state->a;
  int ncount=1; // We already have 2
  int maxintsh=maxints>>1;
  for (int i=1; i<maxintsh; i++)
    if (!(a[i>>SHIFT]&(TYPE)1<<(i&MASK)))
      ncount++;
  return ncount;
}

int main(int argc, char **argv) {
  int maxints=1000000;
  struct timespec t,t2;
  int passes=0;
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
  int valid_result=1; // Will be changed by any thread that computes the result
  struct timespec st,et;
  clock_gettime(CLOCK_MONOTONIC,&st);
#ifdef _OPENMP
#pragma omp parallel reduction(+:passes)
#endif
  {
    // The initial time
    clock_gettime(CLOCK_MONOTONIC,&t);
    struct sieve_state *sieve_state;
    while (1) {
      sieve_state=create_sieve(maxints);
      run_sieve(sieve_state);
      passes++;
      clock_gettime(CLOCK_MONOTONIC,&t2);
      if ((t2.tv_sec+t2.tv_nsec*1e-9-t.tv_sec-t.tv_nsec*1e-9)>=5.) {
	// Count the number of primes and validate the result
	int nprimes=count_primes(sieve_state);
	if (nprimes!=valid_primes)
	  valid_result=0;
	break;
      }
      delete_sieve(sieve_state);
    }
  }
  clock_gettime(CLOCK_MONOTONIC,&et);
  double elapsed_time=et.tv_sec+et.tv_nsec*1e-9-st.tv_sec-st.tv_nsec*1e-9;
  //printf("valid=%d ",valid_result);
  printf("danielspaangberg_1of2_epar;%d;%f;%d;algorithm=base,faithful=yes,bits=1\n", passes,elapsed_time,omp_get_max_threads());
  return 0;
}
