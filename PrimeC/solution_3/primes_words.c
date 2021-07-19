#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <math.h>
#include <string.h>

#define BITZERO 0U

#define ON BITZERO

#ifdef COMPILE_64_BIT
#define TYPE uint64_t
#define MASK 63U
#define SHIFT 6U
#define MEMSHIFT 7U
#define SHIFTSIZE 3U
#else
#define TYPE uint32_t
#define MASK 31U 
#define SHIFT 5U
#define MEMSHIFT 6U
#define SHIFTSIZE 2U 
#endif

struct sieve_state {
  TYPE *bit_array;
  unsigned int limit;
  unsigned int nr_of_words;
};

struct sieve_state *create_sieve(int limit) {
  struct sieve_state *sieve_state=malloc(sizeof *sieve_state);

  sieve_state->nr_of_words=(limit >> MEMSHIFT) + 1;
  sieve_state->bit_array=calloc(sieve_state->nr_of_words,sizeof(TYPE));
  sieve_state->limit=limit;
  return sieve_state;
}

void delete_sieve(struct sieve_state *sieve_state) {
  free(sieve_state->bit_array);
  free(sieve_state);
}

void repeat_words_2_end (
    struct sieve_state *sieve_state,
    unsigned int word_offset, 
    TYPE *word_values, 
    unsigned int size
    ) {
    // size * sizeof(TYPE) == size << SHIFTSIZE;    
    size_t mem_size = size << SHIFTSIZE; 
    unsigned int start_at = word_offset;

    while ( (start_at + size ) < sieve_state->nr_of_words ) {
        memcpy(&(sieve_state->bit_array[start_at]), word_values, mem_size);
        start_at += (size );
    }
    
    int i =0;
    while (start_at < sieve_state->nr_of_words) {
        sieve_state->bit_array[start_at] = word_values[i];
        start_at++;
        i++;
        if (i == size) {
            i = 0;
        }
    }
}

void setBit(struct sieve_state *sieve_state,unsigned int index) {
    unsigned int word_offset = index >> SHIFT;                // 1 word = 2ˆ5 = 32 bit, so shift 5, much faster than /32
    unsigned int offset  = index & MASK;                      // use & (and) for remainder, faster than modulus of /32
    sieve_state->bit_array[word_offset] |=  (TYPE) 1 << offset;
}

TYPE getBit (struct sieve_state *sieve_state,unsigned int index) {
    unsigned int word_offset = index >> SHIFT;  
    unsigned int offset  = index & MASK;
    return sieve_state->bit_array[word_offset] & (TYPE) 1 << offset;     // use a mask to only get the bit at position bitOffset.
}
/*
    Purpose:
    This function calculates a segment of the sieve.

    The calculations starts are start_nr under the assumption that smaller
    numbers are already calculated

    Up to and including the end_nr are crossed out.

    If stop_prime is specified, the calculation stops after the
    crossout for this prime is completed
*/
void run_sieve_segment(
    struct sieve_state *sieve_state,
    unsigned int start_nr_idx,
    unsigned int end_nr,
    unsigned int stop_prime_idx 
) {
    unsigned int factor_index = start_nr_idx;
    unsigned int prime;
    unsigned int max_index=(end_nr>>1U)  - ((end_nr & 1U) == 1U ?  0U:1U);
    unsigned int q=(unsigned int)sqrt(end_nr);
    unsigned int q_index=q>>1U;

    while (factor_index <= q_index) {
        // search next
        if ( getBit(sieve_state,factor_index) == ON ) {
            prime = (factor_index << 1U)+1U;
            // crossout
            for (unsigned int num = ((prime * prime)>>1U) ; num <= max_index; num += prime) {
                setBit(sieve_state,num);
            }  
            if (factor_index == stop_prime_idx) {
                break;
            }
        }

        factor_index++;
    }
}

/*
    Purpose:
    The procedure below runs the sieve in a segmented algorithm.
    
    See details described in README.md
*/
void run_sieve(struct sieve_state *sieve_state) {
    unsigned int prime_word_cpy_idx;
    unsigned int prime_product = 1;
    unsigned int next_prime_product = 0;
    // < 1% of the total justifies the extra effort that is needed 
    // for the repeating word calculation
    unsigned int max_product = (sieve_state->limit >> 1) / 100; 

    if (sieve_state->limit < 100000) {
        // for small sieve sizes, just calculate all in one go
        run_sieve_segment(sieve_state,1,sieve_state->limit,0);
    } else {
        // STEP 1:
        // First calculate all the primes in the first word
        // sizeof(TYPE)<<3U == sizeof(TYPE) * 8
        run_sieve_segment(sieve_state,1,(sizeof(TYPE)<<3U),0);

        // STEP 2:
        // find the optimal product of primes for repeat pattern
        // in the first half word
        for (unsigned int num = 1; num <= (sizeof(TYPE)<<2U); num ++) {
            if (getBit(sieve_state,num) == ON ) {
                next_prime_product = prime_product * ((num << 1U)+1U);
                if (next_prime_product < max_product) {
                    prime_product = next_prime_product;
                    prime_word_cpy_idx = num;
                } else {
                    break;
                }
            }
        }

        // STEP 3
        // crossout from begin to product for up to found prime
        // start with prime3
        // 4*prime_product*8*sizeof(TYPE) == (prime_product*sizeof(TYPE)) << 5U
        run_sieve_segment(sieve_state,1,(prime_product*sizeof(TYPE)) << 5U, prime_word_cpy_idx );

        // STEP 4
        // now we can do a word crossout for the first few primes that
        // are determined by the logic above
        repeat_words_2_end (
            sieve_state,
            prime_product+1, 
            &(sieve_state->bit_array[1]), 
            prime_product
        );

        // STEP 5
        // crossout the remaining
        run_sieve_segment(sieve_state,prime_word_cpy_idx +1,sieve_state->limit,0);
    }
}

void print_primes (struct sieve_state *sieve_state) {
    unsigned int max_index=(sieve_state->limit>>1U) - ((sieve_state->limit & 1U) == 1U ?  0U:1U);
    printf("%i,",2);
    for (unsigned int i = 1; i <= max_index; i++) {
        if (getBit(sieve_state,i) == ON ) {
            printf("%i,",(i<<1U) +1U);    
        }
    }
    printf("\n");
}

unsigned int count_primes (struct sieve_state *sieve_state) {
    unsigned int count = 1;
    unsigned int max_index=(sieve_state->limit>>1U) - ((sieve_state->limit & 1U) == 1U ?  0U:1U);

    for (unsigned int i = 1; i <=max_index; i++) {
        if (getBit(sieve_state,i) == ON ) {
            count++;   
        }
    }
    return count;
}

char* validate_result(unsigned int limit, int count) {
    int valid_primes;

    switch(limit) {
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

    if (valid_primes == -1 ) {
        return "Unknown";
    } else if (valid_primes == count) {
        return "True";
    } 
    return "False";
}

void print_results (    
        struct          sieve_state *sieve_state,
        unsigned int    show_result,
        double          duration,
        int             passes) 
{
    int     count = count_primes(sieve_state);
    char*   valid = validate_result(sieve_state->limit,count);

    if (show_result == 1U) {
        print_primes(sieve_state);
    }

    printf("Passes: %d, Time: %f, Avg: %f, Limit: %d, Count: %d, Valid: %s\n",
            passes,
            duration,
            (duration / passes),
            sieve_state->limit,
            count, 
            valid 
        );

	printf("\n");
	printf("fvbakel_Cwords;%d;%f;1;algorithm=other,faithful=yes,bits=%lu\n", passes, duration,1LU);
}

int main(int argc, char **argv) {
    unsigned int        limit     = 1000000;
    double              maxtime     = 5.;
    unsigned int        show_result = 0;
    
    struct sieve_state *sieve_state;
    struct timespec     start,now;
    double              duration;
    int                 passes      = 0;


    clock_gettime(CLOCK_MONOTONIC,&start);

    while (1) {
        sieve_state=create_sieve(limit);
        run_sieve(sieve_state);
        passes++;
        clock_gettime(CLOCK_MONOTONIC,&now);
        double duration=now.tv_sec+now.tv_nsec*1e-9-start.tv_sec-start.tv_nsec*1e-9;

        if (duration>=maxtime) {
            print_results ( sieve_state, show_result, duration, passes);
            break;
        }

        delete_sieve(sieve_state);
    }

    return 0;
}