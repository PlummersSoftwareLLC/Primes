// Serial code prime sieve by Daniel Spangberg
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <math.h>

/*
#ifdef COMPILE_64_BIT
#define TYPE uint64_t
#define MASK 0x3FU
#define SHIFT 6U
#else
#define TYPE int32_t
#define MASK 0x1FU
#define SHIFT 5U
#endif
*/
#define counter_t int
#define bitword_t unsigned char
#define regdb_t unsigned long long
#define markmask(pos) (1U << pos)
#define prime_roots_ull 1021784272097
#define lookup_root_ull 542798467285129344
#define nonprime 8
counter_t prime_roots[8]  = { 1,7,11,13,17,19,23,29};
//                            0   2   4   6   8   10  12  14  16  18  20  22  24  26  28
counter_t lookup_roots[30] = { 0, nonprime, nonprime,  1, nonprime,  2,   3, nonprime,  4,  5, nonprime,  6, nonprime, nonprime,  7 };

regdb_t make_prime_roots_db() {
    regdb_t make_prime_roots_db = (regdb_t) 0U;
    for (int pos = 0; pos<8; pos++) {
        regdb_t prime_root = prime_roots[pos];
        make_prime_roots_db |= (prime_root << (pos*5));
    }
    return make_prime_roots_db;
}

regdb_t make_lookup_root_db() {
    regdb_t make_lookup_root_db = (regdb_t) 0U;
    for (int pos = 0; pos<=15; pos++) {
        make_lookup_root_db |= ((regdb_t)lookup_roots[pos] << (pos*4));
    }
    return make_lookup_root_db;
}

static inline counter_t lookup_root(counter_t remainder) {
    return lookup_roots[remainder];
//    return (15 & (lookup_root_ull >> (remainder * 4)));
}


/*
counter_t lookup_root[30] = {
//                            0   1   2   3   4   5   6   7   8   9
                              31,  0, 31, 31, 31, 31, 31,  1, 31, 31,
                              31,  2, 31,  3, 31, 31, 31,  4, 31,  5,
                              31, 31, 31,  6, 31, 31, 31, 31, 31, 7
};
*/

struct sieve_state {
  bitword_t *bitarray;
  counter_t maxints;
};

struct sieve_state create_sieve(int maxints) {
  struct sieve_state sieve_state;
  sieve_state.maxints  = maxints;
//  sieve_state.a=calloc(maxints * 8 / 15 /sizeof(TYPE)+1,sizeof(TYPE));
  sieve_state.bitarray = calloc(maxints * 8 / 15  * 2/ sizeof(bitword_t), sizeof(bitword_t));
  return sieve_state;
}

void delete_sieve(struct sieve_state sieve_state) {
  free(sieve_state.bitarray);
}

static inline void setBitTrue(bitword_t *bitarray, counter_t index) {
  counter_t bitoffset  = lookup_root( (index % 30) >> 1 );
  if (bitoffset == nonprime) return;
  counter_t byteoffset = (counter_t) index / 30;
  bitarray[ byteoffset ] |= markmask(bitoffset);
}

static inline bitword_t testBitTrue(bitword_t *bitarray, counter_t index) {
  counter_t bitoffset  = lookup_root( (index % 30) >> 1 );
  if (bitoffset == nonprime) return (bitword_t) 1;
  counter_t byteoffset = (counter_t) index / 30;
  return (bitarray[ byteoffset ] >> bitoffset) & 1;
}

static inline counter_t  searchBitFalse(bitword_t *bitarray, counter_t index) {
    counter_t bitoffset  = lookup_root( (index % 30) >> 1 );
/*
    if (bitoffset == nonprime) {
        printf("Shouldn't happen %d %d %d %d\n", index, bitoffset, ((index % 30)>>1),lookup_root((index % 30)>>1));
        return (bitword_t) 1; // shouldn't happen
    }
    */
    counter_t byteoffset = (counter_t) index / 30;

    do  {
        bitoffset++;
        bitoffset &= 7; // only count to seven.
        if (bitoffset==0) {
            byteoffset++;
        }
    } while ((bitarray[ byteoffset ] >> bitoffset) & 1);
    return byteoffset * 30 + prime_roots[ bitoffset ];

}

void run_sieve(struct sieve_state sieve) {
  counter_t maxints   = sieve.maxints;
  counter_t q = (counter_t) sqrt(maxints )+1U;
  counter_t factor=7;
  /*
  while( factor <= q ) {
    for ( counter_t i = factor * factor; i < maxints; i += (factor << 1)) {
      setBitTrue(sieve.bitarray, i);
    }

    factor = searchBitFalse(sieve.bitarray, factor);
  }
  */
}

counter_t count_primes(struct sieve_state sieve) {
  counter_t maxints         =sieve.maxints;
  counter_t ncount=3; // We already have 2,3,5
  printf("2,3,5,");
  for (counter_t i=7; i<maxints; i+=2) {
    if (!testBitTrue(sieve.bitarray, i)) {
      ncount++;
      if (ncount < 50) {
          printf("%d,",i);
      }
    }
  }
  printf("\n");
  return ncount;
}

int main(int argc, char **argv) {

    printf("sizeof %d\n", (int)sizeof(bitword_t));

    regdb_t prime_roots_db = make_prime_roots_db();
    regdb_t lookup_root_db = make_lookup_root_db();

    for(int pos=0; pos<16; pos++) {
        printf("pos %2d  Lookup %2d  db %d\n", pos, lookup_roots[pos], lookup_root(pos));
    }

    printf("prime_roots_db %llu   lookup_root_db %d \n",prime_roots_db,lookup_root_db );
//  printf("Version 2\n");
  int maxints=1000000;
  struct timespec t,t2;
  int passes=0;
  if (argc>1) sscanf(argv[1],"%d",&maxints);
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
  // The initial time
  clock_gettime(CLOCK_MONOTONIC,&t);
  struct sieve_state sieve_state;
  while (1) {
    sieve_state=create_sieve(maxints);
    run_sieve(sieve_state);
    passes++;
    clock_gettime(CLOCK_MONOTONIC,&t2);
    double elapsed_time=t2.tv_sec+t2.tv_nsec*1e-9-t.tv_sec-t.tv_nsec*1e-9;
    if (elapsed_time>=5 ) { //|| passes > 0) {
      // Count the number of primes and validate the result
      int nprimes=count_primes(sieve_state);
      printf("rogiervandam;%d;%f;1;algorithm=wheel,faithful=yes,bits=1\n", passes,elapsed_time);
      printf("valid=%d primes=%d\n",(nprimes==valid_primes), (int)nprimes);
      break;
    }
    delete_sieve(sieve_state);
  }
  return 0;
}
