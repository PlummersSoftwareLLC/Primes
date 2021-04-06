#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <math.h>

// Based on PrimeCPP.cpp
// Only store odd numbers.
// Reorganize code to minimize some operations

void inline primeset(uint8_t *a, unsigned int prime) {
  unsigned int i=prime>>3;
  unsigned int bp=prime & 0x7U;
  a[i]|=(1<<bp);
}

uint8_t inline primeget(uint8_t *a, unsigned int prime) {
  unsigned int i=prime>>3;
  unsigned int bp=prime & 0x7U;
  return a[i]&(1<<bp);
}

uint8_t *arralloc(unsigned int numbits) {
  size_t numints=numbits>>3;
  if (numbits & 0x7U)
    numints++;
  return calloc(numints,1);
}

void cleararray(uint8_t *a, unsigned int numbits) {
  size_t numints=numbits>>3;
  if (numbits & 0x7U)
    numints++;
  for (int i=0; i<numints; i++)
    a[i]=0U;
}

void findprimes(uint8_t *a, int maxints) {
  int maxintsh=maxints>>1;
  int factor=3;
  int q=(int)sqrt(maxints);
  while (factor<=q) {
    int factorh=factor>>1;
    for (int i=factorh; i<maxintsh; i++) {
      if (!primeget(a,i)) {
	factor=(i<<1)+1;
	break;
      }
    }
    int f2h=(factor*factor)>>1;
    for (int i=f2h; i<maxintsh; i+=factor)
      primeset(a,i);
    factor+=2;
  }
}

int countprimes(uint8_t *a, int maxints) {
  int maxintsh=maxints>>1;
  int ncount=1;
  for (int i=1; i<maxintsh; i++)
    if (!primeget(a,i)) {
      ncount++;
    }
  return ncount;
}

int main(int argc, char **argv) {
  uint8_t *a;
  clock_t t;
  int maxints=1000000;
  int count;
  if (argc>1)
    sscanf(argv[1],"%d",&maxints);
  a=arralloc(maxints/2);
  count=0;
  t=clock();
  do {
    cleararray(a,maxints/2);
    findprimes(a,maxints);
    count++;
  } while ((double)(clock()-t)/CLOCKS_PER_SEC<5.);
  int nprimes=countprimes(a, maxints);
  printf("Passes: %d, Limit: %d, Count: %d\n", count, maxints, nprimes);
  return 0;
}
