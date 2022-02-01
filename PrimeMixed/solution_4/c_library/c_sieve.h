#include <stdint.h>
#include <stdbool.h>

struct sieve
{
    uint64_t *array;
    uint64_t size;
};

struct sieve *construct_sieve(uint64_t size);
void destruct_sieve(struct sieve *sieve);
bool get_bit(struct sieve *sieve, uint64_t index);
void clear_bit(struct sieve *sieve, uint64_t index);
void run_sieve(struct sieve *sieve);
uint64_t count_primes(struct sieve *sieve);
