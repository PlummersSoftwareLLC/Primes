#include <stdlib.h>
#include <math.h>
#include "c_sieve.h"

struct sieve *construct_sieve(uint64_t size)
{
    struct sieve *sieve = malloc(sizeof *sieve);
    sieve->array = calloc((size + 127) >> 7, sizeof(uint64_t));
    sieve->size = size;
    return sieve;
}

void destruct_sieve(struct sieve *sieve)
{
    free(sieve->array);
    free(sieve);
}

bool get_bit(struct sieve *sieve, uint64_t index)
{
    index >>= 1;
    return (sieve->array[index >> 6] & ((uint64_t)1 << (index % 64))) == 0;
}

void clear_bit(struct sieve *sieve, uint64_t index)
{
    index >>= 1;
    sieve->array[index >> 6] |= ((uint64_t)1 << (index % 64));
}

void run_sieve(struct sieve *sieve)
{
    uint64_t size = sieve->size;
    uint64_t factor = 3;
    uint64_t q = sqrt(size);

    while (factor <= q)
    {
        for (uint64_t num = factor; num < size; num += 2)
        {
            if (get_bit(sieve, num))
            {
                factor = num;
                break;
            }
        }

        for (uint64_t num = factor * factor; num < size; num += factor * 2)
        {
            clear_bit(sieve, num);
        }

        factor += 2;
    }
}

uint64_t count_primes(struct sieve *sieve)
{
    uint64_t size = sieve->size;
    uint64_t count = (size >= 2) ? 1 : 0;

    for (uint64_t i = 3; i < size; i += 2)
    {
        if (get_bit(sieve, i))
        {
            count++;
        }
    }

    return count;
}
