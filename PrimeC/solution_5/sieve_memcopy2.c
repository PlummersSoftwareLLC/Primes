// Serial code prime sieve by Daniel Spangberg
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <math.h>
#include <string.h>


// Copyright 2013 Alex Reece.
//
// A cross platform monotonic timer.

//#include <unistd.h>

#define NANOS_PER_SECF 1000000000.0
#define USECS_PER_SEC 1000000

#if _POSIX_TIMERS > 0 && defined(_POSIX_MONOTONIC_CLOCK)
  // If we have it, use clock_gettime and CLOCK_MONOTONIC.

  #include <time.h>

  double monotonic_seconds() {
    struct timespec time;
    // Note: Make sure to link with -lrt to define clock_gettime.
    clock_gettime(CLOCK_MONOTONIC, &time);
    return ((double) time.tv_sec) + ((double) time.tv_nsec / (NANOS_PER_SECF));
  }

#elif defined(__APPLE__)
  // If we don't have CLOCK_MONOTONIC, we might be on a Mac. There we instead
  // use mach_absolute_time().

  #include <mach/mach_time.h>

  static mach_timebase_info_data_t info;
  static void __attribute__((constructor)) init_info() {
    mach_timebase_info(&info);
  }

  double monotonic_seconds() {
    uint64_t time = mach_absolute_time();
    double dtime = (double) time;
    dtime *= (double) info.numer;
    dtime /= (double) info.denom;
    return dtime / NANOS_PER_SECF;
  }

#elif defined(_MSC_VER)
  // On Windows, use QueryPerformanceCounter and QueryPerformanceFrequency.

  #include <windows.h>

  static double PCFreq = 0.0;

  // According to http://stackoverflow.com/q/1113409/447288, this will
  // make this function a constructor.
  // TODO(awreece) Actually attempt to compile on windows.
  static void __cdecl init_pcfreq();
  __declspec(allocate(".CRT$XCU")) void (__cdecl*init_pcfreq_)() = init_pcfreq;
  static void __cdecl init_pcfreq() {
    // Accoring to http://stackoverflow.com/a/1739265/447288, this will
    // properly initialize the QueryPerformanceCounter.
    LARGE_INTEGER li;
    int has_qpc = QueryPerformanceFrequency(&li);
    assert(has_qpc);

    PCFreq = ((double) li.QuadPart) / 1000.0;
  }

  double monotonic_seconds() {
    LARGE_INTEGER li;
    QueryPerformanceCounter(&li);
    return ((double) li.QuadPart) / PCFreq;
  }

#else
  // Fall back to rdtsc. The reason we don't use clock() is this scary message
  // from the man page:
  //     "On several other implementations, the value returned by clock() also
  //      includes the times of any children whose status has been collected via
  //      wait(2) (or another wait-type call)."
  //
  // Also, clock() only has microsecond accuracy.
  //
  // This whitepaper offered excellent advice on how to use rdtscp for
  // profiling: http://download.intel.com/embedded/software/IA/324264.pdf
  //
  // Unfortunately, we can't follow its advice exactly with our semantics,
  // so we're just going to use rdtscp with cpuid.
  //
  // Note that rdtscp will only be available on new processors.

  #include <stdint.h>

  static inline uint64_t rdtsc() {
    uint32_t hi, lo;
    asm volatile("rdtscp\n"
                 "movl %%edx, %0\n"
                 "movl %%eax, %1\n"
                 "cpuid"
                 : "=r" (hi), "=r" (lo) : : "%rax", "%rbx", "%rcx", "%rdx");
    return (((uint64_t)hi) << 32) | (uint64_t)lo;
  }

  static uint64_t rdtsc_per_sec = 0;
  static void __attribute__((constructor)) init_rdtsc_per_sec() {
    uint64_t before, after;

    before = rdtsc();
    usleep(USECS_PER_SEC);
    after = rdtsc();

    rdtsc_per_sec = after - before;
  }

  double monotonic_seconds() {
    return (double) rdtsc() / (double) rdtsc_per_sec;
  }

#endif


#define WORD_SIZE 32
#define counter_t uint32_t
#define bitword_t uint32_t

// the constant below is a cache of all the possible bit masks
//const bitword_t offset_mask[] = {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536,131072,262144,524288,1048576,2097152,4194304,8388608,16777216,33554432,67108864,134217728,268435456,536870912,1073741824,2147483648};

#define  markmask(factor) ((bitword_t)1U << (factor&31))
//#define  markmask(factor) (offset_mask[factor&31])
#define offsetmask(factor) (factor & (sizeof(bitword_t)*8-1))
#define wordindex(factor) (factor >> 5U)

// Return the smallest multiple N of y such that:  x <= y * N
#define ceiling(x,y) (((x) + (y) - 1) / (y))


#define debug if (0)
#define sieve_limit 10000000
#define sieve_duration 5

static inline void printBits(size_t const size, void const * const ptr)
{
    unsigned char *b = (unsigned char*) ptr;
    unsigned char byte;
    int i, j;

    for (i = size-1; i >= 0; i--) {
        for (j = 7; j >= 0; j--) {
            byte = (b[i] >> j) & 1;
            if (byte==0) printf(".");
            else printf("%u", byte);
        }
        printf(" ");
    }
}

struct sieve_state {
    bitword_t    *bitarray;
    counter_t     bits;
};

// https://stackoverflow.com/questions/31117497/fastest-integer-square-root-in-the-least-amount-of-instructions
static inline unsigned isqrt(unsigned long val) {
    unsigned long temp, g=0, b = 0x8000, bshft = 15;
    do {
        if (val >= (temp = (((g << 1) + b)<<bshft--))) {
           g += b;
           val -= temp;
        }
    } while (b >>= 1);
    return g;
}


static inline void dump_bitarray(struct sieve_state *sieve) {
    counter_t       bits = sieve->bits;
    bitword_t  *bitarray = sieve->bitarray;
    printf("Dumping sieve with %d bits\n",bits);
    for (counter_t word=0; word <= 1+(bits >> 5); word+=2) {
        printf("word %2d %2d ",word+1,word);
        printBits(8,&bitarray[word]);
        printf(" %6d - %6d\n", word*32+63, word*32);

    }
    puts("");
}

static inline counter_t count_primes(struct sieve_state *sieve) {
    counter_t       bits = sieve->bits;
    bitword_t  *bitarray = sieve->bitarray;
    counter_t primecount = 1;    // We already have 2
    for (counter_t factor=1; factor < bits; factor++) {
        if ((bitarray[wordindex(factor)] & markmask(factor))==0) {
            if (primecount < 100) {
                debug printf("%3d ",factor*2+1);
                debug if (primecount % 10 == 0) printf("\n");
                printf(" \b"); //don't know why this is necessary workaround?
            }
            primecount++;
        }
    }
    debug puts("");
    return primecount;
}

// use cache lines as much as possible
static inline struct sieve_state *create_sieve(int maxints) {
    struct sieve_state *sieve = aligned_alloc(8, sizeof(struct sieve_state));
    // counter_t memSize = ceiling(1+maxints/2, 64*8) * 64; //make multiple of 8
    // debug printf("MemSize %d bytes  %d bits\n", memSize, memSize * 8);
    // sieve->bitarray = aligned_alloc(64, memSize );
    sieve->bitarray = calloc((maxints>>5)+1,sizeof(bitword_t));
    sieve->bits     = maxints >> 1;
    // for(counter_t i=0; i <= (memSize/sizeof(bitword_t)); i++) {
    //     sieve->bitarray[i] = (bitword_t)0;
    // }
    return sieve;
}

// struct sieve_state *create_sieve(int maxints) {
//     struct sieve_state *sieve_state = malloc(sizeof *sieve_state);
//     sieve_state->bitarray = calloc( maxints / 2 / sizeof(bitword_t)+1 , sizeof(bitword_t) );
//     sieve_state->bits     = maxints >> 1;
//     return sieve_state;
// }

static inline void delete_sieve(struct sieve_state *sieve) {
    free(sieve->bitarray);
    free(sieve);
}

static inline counter_t searchBitFalse(struct sieve_state *sieve, counter_t index) {
    while (sieve->bitarray[wordindex(index)] & markmask(index)) {
        index++;
    }
    return index;
}

static inline void setBitsTrue(struct sieve_state *sieve, counter_t range_start, counter_t step, counter_t range_stop) {
    if (step > 16) {
        counter_t range_stop_unique =  range_start + 32 * step;
        if (range_stop_unique > range_stop) {
            for (counter_t index = range_start; index < range_stop; index += step) {
                sieve->bitarray[wordindex(index)] |= markmask(index);
            }
            return;
        }

        counter_t range_stop_word = wordindex(range_stop);
        for (counter_t index = range_start; index < range_stop_unique; index += step) {
            counter_t wordOffset = wordindex(index);
            bitword_t mask = markmask(index);
            do {
                sieve->bitarray[wordOffset] |= mask;
                wordOffset += step; // pattern repeats on word level after {step} words
            } while (wordOffset <= range_stop_word);
        }
        return;
    }   

    counter_t index = range_start;
    counter_t wordOffset = wordindex(index);
    bitword_t wordValue = sieve->bitarray[wordOffset];

    while (index < range_stop) {
        wordValue |= markmask(index);
        index += step;
        counter_t newWordOffset = wordindex(index);
        if (newWordOffset != wordOffset) {
            sieve->bitarray[wordOffset] = wordValue;
            wordOffset = newWordOffset;
            wordValue = sieve->bitarray[wordOffset];
        }
    }
    sieve->bitarray[wordOffset] = wordValue;

}

static inline void copyBlocks(struct sieve_state *sieve, counter_t source_start, counter_t destination_start, counter_t destination_stop)	{
		const counter_t size = destination_start - source_start;
		counter_t copy_start = destination_start;

		if (size < WORD_SIZE*2) { // handle small: fill the second word
			counter_t copy_max = WORD_SIZE*2 + source_start;
			for (counter_t index=0; index<size; index++) {
                counter_t index_fromstart = source_start + index;
				if (sieve->bitarray[wordindex(index_fromstart)] & markmask(index_fromstart)) {
					counter_t copy_index = destination_start + index;
					while (copy_index < copy_max) {
						sieve->bitarray[wordindex(copy_index)] |= markmask(copy_index);
						copy_index += size;
					}
				}
			}
			while (copy_start < WORD_SIZE*2) copy_start += size;
		}

		counter_t source_word = wordindex(source_start);
		counter_t copy_word = wordindex(copy_start);
		counter_t destination_stop_word = wordindex(destination_stop);
		int shift = (source_start & 31) - (copy_start & 31); // can be negative
		bitword_t dest_wordValue = 0;

		if (shift > 0) {
            counter_t shift_flipped = WORD_SIZE-shift;
            dest_wordValue =  sieve->bitarray[source_word] >> shift;
            dest_wordValue |= sieve->bitarray[source_word+1] << shift_flipped;
            sieve->bitarray[copy_word] |= dest_wordValue; // or the start in to not lose data

            copy_word++;
            source_word++;

            if (copy_word > source_word+4) {
                while (copy_word+4 <= destination_stop_word) {
                    bitword_t source0 = sieve->bitarray[source_word  ];
                    bitword_t source1 = sieve->bitarray[source_word+1];
                    bitword_t source2 = sieve->bitarray[source_word+2];
                    bitword_t source3 = sieve->bitarray[source_word+3];
                    bitword_t source4 = sieve->bitarray[source_word+4];

                    sieve->bitarray[copy_word  ] = (source0 >> shift) | (source1 << shift_flipped);
                    sieve->bitarray[copy_word+1] = (source1 >> shift) | (source2 << shift_flipped);
                    sieve->bitarray[copy_word+2] = (source2 >> shift) | (source3 << shift_flipped);
                    sieve->bitarray[copy_word+3] = (source3 >> shift) | (source4 << shift_flipped);
                    copy_word += 4;
                    source_word += 4;
                }
            }

            while (copy_word < destination_stop_word) {
                sieve->bitarray[copy_word] = (sieve->bitarray[source_word] >> shift) | (sieve->bitarray[source_word+1] << shift_flipped); 
                copy_word++;
                source_word++;
			}
            return;
        }
		if (shift < 0) {
            shift = -shift;
            counter_t shift_flipped = WORD_SIZE-shift;
            dest_wordValue =  sieve->bitarray[source_word] << shift;
            dest_wordValue |= sieve->bitarray[source_word-1] >> shift_flipped;
            sieve->bitarray[copy_word] |= dest_wordValue; // or the start in to not lose data

            copy_word++;
            source_word++;

            if (copy_word > source_word+4) {
                while (copy_word+4 <= destination_stop_word) {
                    bitword_t sourcen = sieve->bitarray[source_word-1];
                    bitword_t source0 = sieve->bitarray[source_word  ];
                    bitword_t source1 = sieve->bitarray[source_word+1];
                    bitword_t source2 = sieve->bitarray[source_word+2];
                    bitword_t source3 = sieve->bitarray[source_word+3];

                    sieve->bitarray[copy_word  ] = (source0 << shift) | (sourcen >> shift_flipped);
                    sieve->bitarray[copy_word+1] = (source1 << shift) | (source0 >> shift_flipped);
                    sieve->bitarray[copy_word+2] = (source2 << shift) | (source1 >> shift_flipped);
                    sieve->bitarray[copy_word+3] = (source3 << shift) | (source2 >> shift_flipped);
                    copy_word += 4;
                    source_word += 4;
                }
            }

            while (copy_word < destination_stop_word) {
                sieve->bitarray[copy_word] = (sieve->bitarray[source_word] << shift) | (sieve->bitarray[source_word-1] >> shift_flipped); 
                copy_word++;
                source_word++;
            }
            return;

        }

        if (shift == 0) {
//            printf("shift 0\n");
            while (copy_word++ <= destination_stop_word) {
                 sieve->bitarray[copy_word] = sieve->bitarray[source_word];
                source_word++;
            }
        }

	}

static inline struct sieve_state *run_sieve(counter_t maxints) {
    struct sieve_state *sieve = create_sieve(maxints);
    bitword_t            *bitarray  = sieve->bitarray;
    counter_t factor         = 1;
    counter_t blocksize_bits = 1;
    counter_t range          = 3;
    counter_t q              = (counter_t)(1+(isqrt(maxints)>>1U));

    while (factor <= q) {
        const counter_t start = factor * factor * 2 + factor * 2;
        const counter_t step  = factor * 2 + 1;

        if (factor > 9) {
            if (range < sieve->bits) { // check if we should copy previous results
                range = blocksize_bits * step * 2;  // range is x2 so the second block cointains all multiples of primes
                if (range > sieve->bits) range = sieve->bits;
                copyBlocks(sieve, blocksize_bits, blocksize_bits*2, range);
                blocksize_bits = blocksize_bits * step;
            }
        }

        setBitsTrue(sieve, start, step, range);
        factor = searchBitFalse(sieve, factor+1);
    }
    return sieve;
}



static inline void deepAnalyzePrimes(struct sieve_state *sieve) {
    printf("DeepAnalyzing\n");
    counter_t range_to =  sieve->bits;
    counter_t warn_prime = 0;
    counter_t warn_nonprime = 0;
    for (counter_t factor = 1; factor < range_to; factor++ ) {
        if ((sieve->bitarray[wordindex(factor)] & markmask(factor))==0) {                   // is this a prime?
            counter_t q = (isqrt(factor*2+1)|1)*2+1;
            for(counter_t c=1; c<=q; c++) {
                if ((factor*2+1) % (c*2+1) == 0 && (c*2+1) != (factor*2+1)) {
                    if (warn_prime++ < 30) {
                        printf("Number %d (%d) was marked prime, but %d * %d = %d\n", factor*2+1, factor,  c*2+1, (factor*2+1)/(c*2+1), factor*2+1 );
                    }
                }
            }
        }
        else {
            counter_t q = (isqrt(factor*2+1)|1)*2+1;
            counter_t c_factor = 0;
            for(counter_t c=1; c<=q; c++) {
                if ((factor*2+1) % (c*2+1) == 0 && (c*2+1) != (factor*2+1)) {
                    c_factor++;
                    break;
                }
            }
            if (c_factor==0) {
                if (warn_nonprime++ < 30) {
                    printf("Number %d (%d) was marked non-prime, but no factors found. So it is prime\n", factor*2+1, factor);
                }
            }
        }
    }
}

int main(int argc, char **argv) {
    counter_t maxints  = sieve_limit;
    double    max_time = sieve_duration;
    if (argc>1) sscanf(argv[1],"%u",&maxints);

//    printf(" 0 has %d trailing zeros\n", __builtin_ctz(0));
//    printf("~0 has %d trailing zeros\n", __builtin_ctz(~0));
//    printf(" 8 has %d trailing zeros\n", __builtin_ctz(8));

    counter_t valid_primes;
    switch(maxints) {
        case 10:            valid_primes = 4;        break;
        case 100:           valid_primes = 25;       break;
        case 1000:          valid_primes = 168;      break;
        case 10000:         valid_primes = 1229;     break;
        case 100000:        valid_primes = 9592;     break;
        case 1000000:       valid_primes = 78498;    break;
        case 10000000:      valid_primes = 664579;   break;
        case 100000000:     valid_primes = 5761455;  break;
        case 1000000000:    valid_primes = 50847534; break;
        default:            valid_primes=-1;
    }

    struct timespec start_time,end_time;
    counter_t passes = 0;

    // The initial time

    struct sieve_state *sieve;

    // Count the number of primes and validate the result
    sieve = run_sieve(maxints);
    counter_t primecount = count_primes(sieve);
    int valid = (primecount==valid_primes);

    if (!valid) {
        printf("No valid result. Sievesize %d was expected to have %d primes, but algoritm produced %d primes\n",maxints,valid_primes,primecount );
//        deepAnalyzePrimes(sieve);
    }

    delete_sieve(sieve);

    if (valid) {
        printf("Valid algoritm. Benchmarking...\n");
        double elapsed_time = 0;
        double start_time = monotonic_seconds();
        while (elapsed_time <= max_time) {
            sieve = run_sieve(maxints);
            delete_sieve(sieve);
            passes++;
            elapsed_time = monotonic_seconds()-start_time;
        }
        printf("rogiervandam_memcopy;%d;%f;1;algorithm=other,faithful=yes,bits=1\n", passes,elapsed_time);
//        printf("valid=%d   primes %d\n",valid, primecount);
    }
}
