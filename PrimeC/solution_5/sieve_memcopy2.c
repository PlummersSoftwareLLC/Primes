// Serial code prime sieve by Daniel Spangberg
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <math.h>
#include <string.h>


#define debug if (0)
#define sieve_limit 1000000
//#define blocksize (16*1024*8)
// #define sieve_limit 100
#define sieve_duration 5



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


#define wordindex(index) ((index) >> 5U)
#define bitindex(index) ((index)&31)
#define  markmask(index) ((bitword_t)1U << bitindex(index))
//#define  markmask(factor) (offset_mask[factor&31])
#define offsetmask(index) ((index) & (sizeof(bitword_t)*8-1))

// Return the smallest multiple N of y such that:  x <= y * N
#define ceiling(x,y) (((x) + (y) - 1) / (y))

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
    counter_t     size;
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
            // if (primecount < 100) {
            //     debug printf("%3d ",factor*2+1);
            //     debug if (primecount % 10 == 0) printf("\n");
            //     printf(" \b"); //don't know why this is necessary workaround?
            // }
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
    sieve->size     = maxints;
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

    // counter_t index_word = wordindex(index);
    // counter_t index_bit  = bitindex(index);

    // bitword_t word_value = sieve->bitarray[index_word];
    // counter_t distance = 0;
    // word_value >>= index_bit; // first word is special because some bits have to be skipped
    // while (word_value & 1) { 
    //     word_value >>= 1;
    //     distance++;
    // }
    // index += distance;
    // distance += index_bit;

    // while (distance >= 32) {
    //     index_word++;
    //     word_value = sieve->bitarray[index_word];
    //     distance = 0;
    //     while (word_value & 1) { 
    //         word_value >>= 1;
    //         distance++;
    //     }
    //     index += distance;
    // }

    // return index;
}

static inline void setBitsTrue(struct sieve_state *sieve, counter_t range_start, counter_t step, counter_t range_stop) {
//    printf("Setbitstrue %d-%d step %d\n",range_start, range_stop, step);
    // for (counter_t index = range_start; index < range_stop; index += step) {
    //     // printf("Setting bit %d true\n", index);
    //     sieve->bitarray[wordindex(index)] |= markmask(index);
    // }
    // return;

    if (step > WORD_SIZE) {
        counter_t range_stop_unique =  range_start + WORD_SIZE * step;
        if (range_stop_unique > range_stop) {
            for (counter_t index = range_start; index <= range_stop; index += step) {
                sieve->bitarray[wordindex(index)] |= markmask(index);
            }
            return;
        }

        counter_t range_stop_word = wordindex(range_stop);
        counter_t range_stop_bit  =  markmask(range_stop);
        for (counter_t index = range_start; index <= range_stop_unique; index += step) {
            counter_t index_bit = bitindex(index);
            bitword_t mask = (1 << index_bit);
            counter_t loop_stop_word = range_stop_word + ((index_bit <= range_stop_bit) ? 1 : 0);

            for (counter_t index_word = wordindex(index); index_word < loop_stop_word; index_word += step) {
                sieve->bitarray[index_word] |= mask;
            }
        }
        return;
    }   

	bitword_t pattern = 1;
    counter_t patternsize = step;
    for (; patternsize < WORD_SIZE; patternsize += step) {
        pattern |= (1 << patternsize);
    }
    patternsize -= step;
    counter_t pattern_shift = WORD_SIZE - patternsize; // the amount a pattern drifts (>>) at each word increment

    counter_t shift = bitindex(range_start);
    counter_t range_stop_word = wordindex(range_stop);
    counter_t range_stop_bit = bitindex(range_stop);
    counter_t copy_word = wordindex(range_start);

    if (copy_word == range_stop_word) { // shortcut
       sieve->bitarray[copy_word] |= ((pattern << shift) & (2 << ((range_stop_bit&31))) - 1 );
       return;
    }

    // from now on, we are before range_stop_word
    // first word is special, because it should not set bits before the range_start_bit
    sieve->bitarray[copy_word] |= (pattern << shift);
    shift = shift % step;
    copy_word++;

    while (copy_word < range_stop_word) {
        if (shift < pattern_shift) shift += step; // prevent shift going negative
        shift -= pattern_shift; 
        sieve->bitarray[copy_word] |= pattern << shift;
        copy_word++;
    } 

    if (shift < pattern_shift) shift += step;
    shift -= pattern_shift; 
    sieve->bitarray[copy_word] |= ((pattern << shift) & (2 << ((range_stop_bit&31))) - 1 );

}

static inline void copyPattern(struct sieve_state *sieve, counter_t source_start, counter_t size, counter_t destination_stop)	{
    counter_t source_word = wordindex(source_start);
    counter_t source_bit = bitindex(source_start);
    counter_t destination_stop_word = wordindex(destination_stop);
    counter_t copy_start = source_start + size;
    counter_t copy_word = wordindex(copy_start);
    counter_t copy_bit = bitindex(copy_start);

    if (size < WORD_SIZE) { // handle small: fill the second word
        printf("Copying from %d to max %d with size %d\n",source_start,destination_stop, size);

        bitword_t pattern = (sieve->bitarray[source_word] >> source_bit);
        pattern |= sieve->bitarray[source_word+1] << (WORD_SIZE-source_bit);
        pattern &= ((2 << size) -1);

        counter_t patternsize = size;
        while (patternsize < WORD_SIZE) {
            pattern |= (pattern << patternsize);
            patternsize += size;
            copy_start += size;
        }
        patternsize -= size;
        counter_t pattern_shift = WORD_SIZE - patternsize;
        sieve->bitarray[copy_word] |= (pattern << copy_bit);

        counter_t shift = WORD_SIZE - copy_bit;
        while (copy_word < destination_stop_word) { // = will be handled as well because increment is after this 
            copy_word++;
            sieve->bitarray[copy_word] = (pattern << (patternsize-shift)) | (pattern >> shift);
            shift += pattern_shift; 
            if (shift > WORD_SIZE) shift -= WORD_SIZE; // TODO: check if needed
        }
        return;
    }

    counter_t shift = source_bit - copy_bit;
    bitword_t dest_wordValue = 0;

    printf("Copying from %d to max %d with shift %d\n",source_start,destination_stop, shift);

    if (shift > 0) {
        counter_t shift_flipped = WORD_SIZE-shift;
        dest_wordValue =  sieve->bitarray[source_word] >> shift;
        dest_wordValue |= sieve->bitarray[source_word+1] << shift_flipped;
        dest_wordValue &= ~((1<<copy_bit)-1); // because this is the first word, dont copy the extra bits in front of the source
        sieve->bitarray[copy_word] |= dest_wordValue; // or the start in to not lose data

        // if (copy_word > source_word+4) {
        //     while (copy_word+4 <= destination_stop_word) {
        //         bitword_t source0 = sieve->bitarray[source_word  ];
        //         bitword_t source1 = sieve->bitarray[source_word+1];
        //         bitword_t source2 = sieve->bitarray[source_word+2];
        //         bitword_t source3 = sieve->bitarray[source_word+3];
        //         bitword_t source4 = sieve->bitarray[source_word+4];

        //         sieve->bitarray[copy_word  ] = (source0 >> shift) | (source1 << shift_flipped);
        //         sieve->bitarray[copy_word+1] = (source1 >> shift) | (source2 << shift_flipped);
        //         sieve->bitarray[copy_word+2] = (source2 >> shift) | (source3 << shift_flipped);
        //         sieve->bitarray[copy_word+3] = (source3 >> shift) | (source4 << shift_flipped);
        //         copy_word += 4;
        //         source_word += 4;
        //     }
        // }

        while (copy_word < destination_stop_word) {
            copy_word++;
            source_word++;
            sieve->bitarray[copy_word] = (sieve->bitarray[source_word] >> shift) | (sieve->bitarray[source_word+1] << shift_flipped); 
        }
        return;
    }
    if (shift < 0) {
        shift = -shift;
        counter_t shift_flipped = WORD_SIZE-shift;
        counter_t source_lastword = wordindex(source_start + size);
        dest_wordValue =  sieve->bitarray[source_word] << shift;
        dest_wordValue |= sieve->bitarray[source_lastword] >> shift_flipped;
        dest_wordValue &= ~((1<<copy_bit)-1); // because this is the first word, dont copy the extra bits in front of the source
        sieve->bitarray[copy_word] |= dest_wordValue; // or the start in to not lose data

        // if (copy_word > source_word+4) {
        //     while (copy_word+4 <= destination_stop_word) {
        //         bitword_t sourcen = sieve->bitarray[source_word-1];
        //         bitword_t source0 = sieve->bitarray[source_word  ];
        //         bitword_t source1 = sieve->bitarray[source_word+1];
        //         bitword_t source2 = sieve->bitarray[source_word+2];
        //         bitword_t source3 = sieve->bitarray[source_word+3];

        //         sieve->bitarray[copy_word  ] = (source0 << shift) | (sourcen >> shift_flipped);
        //         sieve->bitarray[copy_word+1] = (source1 << shift) | (source0 >> shift_flipped);
        //         sieve->bitarray[copy_word+2] = (source2 << shift) | (source1 >> shift_flipped);
        //         sieve->bitarray[copy_word+3] = (source3 << shift) | (source2 >> shift_flipped);
        //         copy_word += 4;
        //         source_word += 4;
        //     }
        // }

        while (copy_word < destination_stop_word) {
            copy_word++;
            source_word++;
            dest_wordValue = (sieve->bitarray[source_word-1] >> shift_flipped);
            dest_wordValue = (sieve->bitarray[source_word] << shift);
            sieve->bitarray[copy_word] = dest_wordValue; 
        }
        return;

    }

    if (shift == 0) { // first word can be spread over 2 words
        counter_t shift_flipped = WORD_SIZE-shift;
        dest_wordValue =  sieve->bitarray[source_word] >> shift;
        dest_wordValue |= sieve->bitarray[source_word+1] << shift_flipped;
        dest_wordValue &= ~((1<<copy_bit)-1); // because this is the first word, dont copy the extra bits in front of the source
        sieve->bitarray[copy_word] |= dest_wordValue; // or the start in to not lose data

        while (copy_word < destination_stop_word) {
            sieve->bitarray[copy_word] = sieve->bitarray[source_word];
            source_word++;
            copy_word++;
        }
    }

}

static inline void sieve_block(struct sieve_state *sieve, counter_t block_start, counter_t block_stop) {
    counter_t prime            = 0;
    counter_t patternsize_bits = 1;
    counter_t range            = sieve->bits;
    counter_t range_stop       = block_stop;

    do {
        prime = searchBitFalse(sieve, prime+1);
        counter_t start = prime * prime * 2 + prime * 2;
        if (start > block_stop) break;

        const counter_t step  = prime * 2 + 1;
        if (block_start >= (prime + 1)) start = block_start + prime + prime - ((block_start + prime) % step);

        range_stop = block_start + patternsize_bits * step * 2;  // range is x2 so the second block cointains all multiples of primes
        if (range_stop > block_stop) range_stop = block_stop;

        if (patternsize_bits>1) {
            counter_t pattern_start = (block_start==0) ? patternsize_bits : 0;
            copyPattern(sieve, pattern_start, patternsize_bits, range_stop);
        }
        patternsize_bits *= step;

        setBitsTrue(sieve, start, step, range_stop);
    } while (range_stop < block_stop);

    while (1) {
        prime = searchBitFalse(sieve, prime+1);
        counter_t start = prime * prime * 2 + prime * 2;
        if (start > block_stop) break;
        const counter_t step  = prime * 2 + 1;
        if (block_start >= (prime + 1)) start = block_start + prime + prime - ((block_start + prime) % step);
        setBitsTrue(sieve, start, step, block_stop);
    } 
}

static inline struct sieve_state *sieve(counter_t maxints, counter_t blocksize) {
    struct sieve_state *sieve = create_sieve(maxints);
    bitword_t*bitarray       = sieve->bitarray;
    counter_t prime          = 1;
    counter_t range          = sieve->bits;

    counter_t block_start = 0;
    counter_t block_stop = blocksize-1;

    do {
        if (block_stop > sieve->bits) block_stop = sieve->bits;
        sieve_block(sieve, block_start, block_stop);
        block_start += blocksize;
        block_stop += blocksize;
    } while (block_start <= sieve->bits);

    return sieve;
}

static inline void deepAnalyzePrimes(struct sieve_state *sieve) {
    printf("DeepAnalyzing\n");
    counter_t range_to =  sieve->bits;
    counter_t warn_prime = 0;
    counter_t warn_nonprime = 0;
    for (counter_t prime = 1; prime < range_to; prime++ ) {
        if ((sieve->bitarray[wordindex(prime)] & markmask(prime))==0) {                   // is this a prime?
            counter_t q = (isqrt(prime*2+1)|1)*2+1;
            for(counter_t c=1; c<=q; c++) {
                if ((prime*2+1) % (c*2+1) == 0 && (c*2+1) != (prime*2+1)) {
                    if (warn_prime++ < 30) {
                        printf("Number %d (%d) was marked prime, but %d * %d = %d\n", prime*2+1, prime,  c*2+1, (prime*2+1)/(c*2+1), prime*2+1 );
                    }
                }
            }
        }
        else {
            counter_t q = (isqrt(prime*2+1)|1)*2+1;
            counter_t c_prime = 0;
            for(counter_t c=1; c<=q; c++) {
                if ((prime*2+1) % (c*2+1) == 0 && (c*2+1) != (prime*2+1)) {
                    c_prime++;
                    break;
                }
            }
            if (c_prime==0) {
                if (warn_nonprime++ < 30) {
                    printf("Number %d (%d) was marked non-prime, but no factors found. So it is prime\n", prime*2+1, prime);
                }
            }
        }
    }
}


int validatePrimeCount(struct sieve_state *sieve, int verboselevel) {

    counter_t primecount = count_primes(sieve);
    counter_t valid_primes;
    switch(sieve->size) {
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

    int valid = (valid_primes == primecount);
    printf("Result: Sievesize %d is expected to have %d primes. Algoritm produced %d primes\n",sieve->size,valid_primes,primecount );
    if (!valid && verboselevel >= 1) printf("No valid result. Sievesize %d was expected to have %d primes, but algoritm produced %d primes\n",sieve->size,valid_primes,primecount );
    if (!valid && verboselevel >= 1) deepAnalyzePrimes(sieve);
    return (valid);
}

int main(int argc, char **argv) {
    counter_t maxints  = sieve_limit;
    double    max_time = sieve_duration;
 //   counter_t blocksize = 16*1024*8;

    if (argc>1) sscanf(argv[1],"%u",&maxints);

    struct timespec start_time,end_time;

    // The initial time
    struct sieve_state *sieve_instance;

    // Count the number of primes and validate the result
	printf("Validating... \n\n");

	for (counter_t blocksize_bits=1024; blocksize_bits<=64*1024*8; blocksize_bits *= 2) {
		for (counter_t sieveSize_check = 100; sieveSize_check <= 100000000; sieveSize_check *=10) {
			// validate algorithm - run one time
            printf("Checking size %d blocksize %d",sieveSize_check,blocksize_bits);
            sieve_instance = sieve(sieveSize_check, blocksize_bits);
            counter_t primecount = count_primes(sieve_instance);
            int valid = validatePrimeCount(sieve_instance,2);
            delete_sieve(sieve_instance);
            if (!valid) return 0;
		}
	}

    printf("Valid algoritm. Benchmarking...\n");
    for (counter_t blocksize_kb=128; blocksize_kb>=1; blocksize_kb /= 2) {
        counter_t passes = 0;
        counter_t blocksize_bits = blocksize_kb * 1024 * 8;
        double elapsed_time = 0;
        double start_time = monotonic_seconds();
        while (elapsed_time <= max_time) {
            sieve_instance = sieve(maxints, blocksize_bits);//blocksize_bits);
            delete_sieve(sieve_instance);
            passes++;
            elapsed_time = monotonic_seconds()-start_time;
        }
        printf("rogiervandam_memcopy-%dkb;%d;%f;1;algorithm=other,faithful=yes,bits=1\n", blocksize_kb,passes,elapsed_time);
    }
}
