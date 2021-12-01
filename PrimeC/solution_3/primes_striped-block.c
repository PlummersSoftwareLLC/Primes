#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <math.h>

#define TYPE uint32_t
#define BITZERO ((TYPE) 0)
#define ON BITZERO

#define KEEP_FREE 1024
//
// The configuration parameter below determines 
unsigned int BLOCK_SIZE;
unsigned int BITS_IN_BLOCK;

// the const below is to reduce the multiplications
#define BITS_IN_BYTE 8
const unsigned int BITS_IN_WORD=BITS_IN_BYTE*sizeof(TYPE);

// the constant below is a cache of all the possible bit masks
const TYPE OFFSET_MASK[] = {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536,131072,262144,524288,1048576,2097152,4194304,8388608,16777216,33554432,67108864,134217728,268435456,536870912,1073741824,2147483648};

struct sieve_state {
    TYPE *bit_storage;
    TYPE **block_index;
    unsigned int limit;
    unsigned int size;
    unsigned int nr_of_words;
    unsigned int nr_of_blocks;
};

static inline struct sieve_state *create_sieve(unsigned int limit) {
    struct sieve_state *sieve_state=malloc(sizeof *sieve_state);
    sieve_state->size = limit >>1;
    unsigned int extra = (sieve_state->size % BITS_IN_BLOCK);
    if (extra > 0) {
        extra = 1;
    }
    sieve_state->nr_of_blocks= (sieve_state->size / BITS_IN_BLOCK) + extra;
    sieve_state->nr_of_words = (sieve_state->nr_of_blocks) * BLOCK_SIZE;
    sieve_state->bit_storage= calloc(sieve_state->nr_of_words,sizeof(TYPE));
    sieve_state->block_index= (TYPE**) malloc(sieve_state->nr_of_blocks * sizeof(TYPE*));
    sieve_state->limit=limit;
    unsigned int block_off_set = 0;
    for (unsigned int i = 0; i<sieve_state->nr_of_blocks;i++) {
        sieve_state->block_index[i] = &(sieve_state->bit_storage[block_off_set]);
        block_off_set += BLOCK_SIZE;
    }
    return sieve_state;
}

static inline void delete_sieve(struct sieve_state *sieve_state) {
  free(sieve_state->bit_storage);
  free(sieve_state->block_index);
  free(sieve_state);
}

static inline TYPE setBit (struct sieve_state *sieve_state,unsigned int index) {
    if (index > sieve_state->size) {
        return (TYPE) 0;
    }

    unsigned int block_idx = index / BITS_IN_BLOCK;
    unsigned int offset = index % BITS_IN_BLOCK;
    unsigned int bit_idx = offset / BLOCK_SIZE;
    unsigned int word_idx = offset % BLOCK_SIZE;
    sieve_state->block_index[block_idx][word_idx] |=  OFFSET_MASK[bit_idx];
}

static inline TYPE getBit (struct sieve_state *sieve_state,unsigned int index) {
    if (index > sieve_state->size) {
        return (TYPE) 0;
    }

    unsigned int block_idx = index / BITS_IN_BLOCK;
    unsigned int offset = index % BITS_IN_BLOCK;
    unsigned int bit_idx = offset / BLOCK_SIZE;
    unsigned int word_idx = offset % BLOCK_SIZE;
    return (TYPE) sieve_state->block_index[block_idx][word_idx] & OFFSET_MASK[bit_idx];
}

static inline void bit_cross_out(
    struct sieve_state *sieve_state,
    unsigned int prime
) {
    unsigned int start_index = ((prime * prime)>>1U);
    unsigned int block_idx_start = start_index / BITS_IN_BLOCK;
    unsigned int offset_idx = start_index % BITS_IN_BLOCK;
    unsigned int bit_idx = offset_idx / BLOCK_SIZE;
    unsigned int word_idx = offset_idx % BLOCK_SIZE;

    unsigned int prime_2 = prime * 2;
    unsigned int prime_3 = prime * 3;
    unsigned int prime_4 = prime * 4;

    for (unsigned int block_idx = block_idx_start; block_idx < sieve_state->nr_of_blocks;block_idx++) {
        while (bit_idx < BITS_IN_WORD) {
            unsigned int stripe_start_position = (block_idx * BITS_IN_BLOCK) + (bit_idx * BLOCK_SIZE);
            unsigned int effective_len = (sieve_state->size - stripe_start_position);

            if (effective_len > BLOCK_SIZE) {
                effective_len = BLOCK_SIZE;
            }

            unsigned int save_len = 0;
            if (effective_len > prime_3) {
                save_len = effective_len - prime_3;
            }
            
            while ( word_idx < save_len ) {
                sieve_state->block_index[block_idx][word_idx] |= OFFSET_MASK[bit_idx];
                sieve_state->block_index[block_idx][word_idx + prime] |= OFFSET_MASK[bit_idx];
                sieve_state->block_index[block_idx][word_idx + prime_2 ] |= OFFSET_MASK[bit_idx];
                sieve_state->block_index[block_idx][word_idx + prime_3 ] |= OFFSET_MASK[bit_idx];
                
                word_idx += prime_4;
            }

            // the rest at the end
            while (word_idx < effective_len) {
                sieve_state->block_index[block_idx][word_idx] |= OFFSET_MASK[bit_idx];
                word_idx += prime;
            }

            if (effective_len != BLOCK_SIZE) {
                return;
            }

            bit_idx++;
            word_idx -= BLOCK_SIZE;
        }

        bit_idx = 0;
    }
}

void run_sieve(struct sieve_state *sieve_state) {
    unsigned int factor_index = 1U;
    unsigned int prime;
    unsigned int q=(unsigned int)sqrt(sieve_state->limit);
    unsigned int q_index=q>>1U;
    
    while (factor_index < q_index) {
        if ( getBit(sieve_state,factor_index) == ON ) {
            prime = (factor_index << 1U)+1U;
            bit_cross_out(sieve_state,prime);  
        }
        factor_index++;
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
	printf("fvbakel_Cstriped-block;%d;%f;1;algorithm=base,faithful=yes,bits=%lu\n", passes, duration,1LU);
}

double run_timed_sieve(  
    unsigned int        limit,
    double              maxtime,
    unsigned int        show_result,
    unsigned int        print_sumary
 ) {
    
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
        duration=now.tv_sec+now.tv_nsec*1e-9-start.tv_sec-start.tv_nsec*1e-9;

        if (duration>maxtime) {
            if (print_sumary) {
                print_results ( sieve_state, show_result, duration, passes);
            }
            break;
        }

        delete_sieve(sieve_state);
    }

    return passes/duration;
}

/*
    Purpose:
    Determine the optimal size for the BLOCK_SIZE 
    parameter on this hardware based on sample runs
    and set that parameter
*/
void set_word_block_size(const unsigned int limit) {
    const double sample_time = 0.2;
    const unsigned int block_size_samples_kb[] = {4,16,32,64,128,(1024*4)};
    const unsigned int nr_of_samples = 6;
    unsigned int block_size_samples[(nr_of_samples)];
    double speed[(nr_of_samples)];
    unsigned int max_speed_index = 0;

    for (unsigned int i = 0; i <nr_of_samples; i ++) {
        block_size_samples[i] = ((block_size_samples_kb[i] * 1024) - KEEP_FREE) / sizeof(TYPE);
        BLOCK_SIZE = block_size_samples[i];
        BITS_IN_BLOCK =  BLOCK_SIZE * BITS_IN_WORD;
        speed[i] = run_timed_sieve(limit,sample_time,0,0);
    }

    for (unsigned int i = 1; i <(nr_of_samples); i ++) {
        if (speed[i]> speed[max_speed_index]) {
            max_speed_index = i;
        }
    }
    BLOCK_SIZE = block_size_samples[max_speed_index];
    BITS_IN_BLOCK =  BLOCK_SIZE * BITS_IN_WORD;
}

int main(int argc, char **argv) {
    unsigned int        limit       = 1000000;
    double              maxtime     = 5.;
    unsigned int        show_result = 0;
    
    double              speed;

    set_word_block_size(limit);
    BITS_IN_BLOCK =  BLOCK_SIZE * BITS_IN_WORD;
    speed = run_timed_sieve(limit,maxtime,show_result,1);

    return 0;
}