// Thread parallel code prime sieve by Daniel Spangberg
// Parallelizing the bit setting part of the code
// Only count 480 of 2310 (2*3*5*7*11), there are only 480 numbers not divisible by 2,3,5,7,11 within this range
// Discussions and code sharing with @mckoss & @Kinematics
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

// Steps array for finding the next number not divisible by 2,3,5,7,11
// Pre halved
static unsigned int steps[480]={
 6,2,1,2,3,1,3,2,1,2,3,3,1,3,2,1,3,2,3,4,2,1,2,1,2,
 7,2,3,1,5,1,3,3,2,1,2,3,1,5,1,2,1,6,5,1,2,1,2,3,1,
 3,2,3,3,3,1,3,2,1,3,2,3,4,2,1,2,3,4,3,5,1,2,3,1,3,
 3,2,1,2,3,1,3,2,1,3,5,1,5,1,2,1,2,3,4,2,1,2,6,1,3,
 2,1,3,2,3,6,1,2,1,2,4,3,2,3,1,2,3,1,3,5,1,2,3,1,3,
 2,1,2,1,5,1,5,1,2,3,3,1,3,3,2,3,3,1,3,2,1,3,2,3,4,
 2,1,3,2,4,3,2,3,1,2,3,4,3,2,1,5,1,3,2,1,2,1,5,1,5,
 1,2,1,2,4,3,2,1,2,3,3,1,3,2,4,2,3,4,2,1,2,1,2,4,3,
 2,3,3,3,1,3,3,2,1,2,3,1,3,2,1,2,1,5,1,5,1,3,2,3,1,
 3,2,1,2,3,3,4,2,1,3,5,4,2,1,2,1,2,4,5,3,1,2,4,3,3,
 2,1,2,3,1,3,2,3,1,5,1,5,1,2,1,2,3,1,3,2,1,2,3,3,1,
 3,3,3,2,3,4,2,1,2,1,2,4,3,2,4,2,3,1,3,3,2,1,2,3,4,
 2,1,2,1,5,1,5,1,2,1,2,3,1,5,1,2,3,4,3,2,1,3,2,3,4,
 2,3,1,2,4,3,2,3,1,2,3,1,3,3,2,3,3,1,3,3,2,1,5,1,5,
 1,2,1,2,3,1,3,2,1,5,3,1,3,2,1,3,2,3,4,2,1,2,1,6,3,
 2,3,1,2,3,1,6,2,1,2,4,3,2,1,2,1,5,1,5,3,1,2,3,1,3,
 2,1,2,3,3,1,3,2,1,5,3,4,3,2,1,2,4,3,2,3,1,2,3,1,3,
 3,3,2,3,1,3,2,1,2,1,5,6,1,2,1,5,1,3,2,1,2,3,3,1,5,
 1,3,2,7,2,1,2,1,2,4,3,2,3,1,2,3,1,3,3,2,1,2,3,1,3,
 2,1,2,6,1
};

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
#ifdef _OPENMP
  int numthreads=omp_get_max_threads();
#endif
  unsigned int maxints=sieve_state->maxints;
  TYPE *a=sieve_state->a;
  unsigned int maxintsh=maxints>>1;
  unsigned int q=(unsigned int)sqrt(maxints)+1U;
  unsigned int step=1U; // From 13 to 17
  unsigned int inc=steps[step]; // Next increment in steps array
  // Only check integers not divisible by 2, 3, 5, 7, or 11
  unsigned int factorh=13U>>1U; // We already have 2, 3, 5, 7, and 11
  unsigned int qh=q>>1U;
  while (factorh<=qh) {
    // Search for next prime
    if (a[factorh>>SHIFT]&((TYPE)1<<(factorh&MASK))) {
      factorh+=inc;
      if (++step==480U) step=0U; // End of steps array, start from the beginning
      inc=steps[step];
      continue;
    }
    // Mask all integer multiples of this prime
    unsigned int factor=(factorh<<1U)+1U;
    // Use as many threads as possible if the workload is large enough
#ifdef _OPENMP
    if (maxints>factor*factor) {
      unsigned int worksize=(maxints-factor*factor)/(2*factor);
      if (worksize>(numthreads*300)) {
#pragma omp parallel
	{
	  unsigned int ithread=omp_get_thread_num();
	  unsigned int firstbit=(factor*factor)>>1U;
	  unsigned int lastbit=maxintsh;
	  unsigned int workbits=(lastbit-firstbit)/factor;
	  unsigned int threadworkbits=workbits/numthreads;
	  unsigned int myfirstbit=firstbit+factor*threadworkbits*ithread;
	  unsigned int mylastbit=firstbit+factor*threadworkbits*(ithread+1U)-factor;
	  // Make sure I handle full words and do not start in a word updating by the previous thread
	  if (ithread!=0U) {
	    while (1) {
	      unsigned int previous_thread_lastbit=myfirstbit-factor;
	      unsigned int my_first_word=myfirstbit>>SHIFT;
	      unsigned int previous_thread_last_word=previous_thread_lastbit>>SHIFT;
	      if (my_first_word!=previous_thread_last_word)
		break;
	      else
		myfirstbit+=factor;
	    }
	  }
	  if (ithread==numthreads-1)
	    mylastbit=lastbit-1;
	  else {
	    // Make sure I continue with more bits until the next cpu can handle the bits
	    while (1) {
	      unsigned int next_thread_firstbit=mylastbit+factor;
	      unsigned int my_last_word=mylastbit>>SHIFT;
	      unsigned int next_thread_first_word=next_thread_firstbit>>SHIFT;
	      if (my_last_word!=next_thread_first_word)
		break;
	      else
		mylastbit+=factor;
	    }
	  }
	  for (unsigned int m = myfirstbit; m <= mylastbit; m += factor)
	    a[m>>SHIFT]|=(TYPE)1<<(m&MASK);
	}
      }
      else {
	for (unsigned int m = (factor*factor)>>1; m < maxintsh; m += factor)
	  a[m>>SHIFT]|=(TYPE)1<<(m&MASK);
      }
    }
#else
    for (unsigned int m = (factor*factor)>>1; m < maxintsh; m += factor)
      a[m>>SHIFT]|=(TYPE)1<<(m&MASK);
#endif
    factorh+=inc;
    if (++step==480U) step=0U; // End of steps array, start from the beginning
    inc=steps[step];
  }
}

unsigned int count_primes(struct sieve_state *sieve_state) {
  unsigned int maxints=sieve_state->maxints;
  TYPE *a=sieve_state->a;
  unsigned int ncount=5; // We already have 2, 3, 5, 7, and 11 ...
  unsigned int factor=13; // ...
  unsigned int step=1; // From 13 to 17
  unsigned int inc=steps[step]<<1U; // Next increment in steps array
  while (factor<=maxints) {
    if (!(a[factor>>(SHIFT+1U)]&((TYPE)1<<((factor>>1U)&MASK))))
      ncount++;
    factor+=inc;
    if (++step==480U) step=0U; // End of steps array, start from the beginning
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
      printf("danielspaangberg_480of2310_par;%d;%f;%d;algorithm=wheel,faithful=yes,bits=1\n", passes,elapsed_time,omp_get_max_threads() );
      break;
    }
    delete_sieve(sieve_state);
  }
  return 0;
}
