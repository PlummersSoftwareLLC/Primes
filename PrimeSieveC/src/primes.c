#include <time.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <inttypes.h>

static bool* bits = NULL;
static size_t sieve_size;

static size_t count_primes(void) {
	size_t count = (sieve_size >= 2);
	for(size_t i = 3; i < sieve_size; i += 2) {
		if(bits[i]) {
			++count;
		}
	}
	return count;
}

static bool validate_results(void) {
	switch(sieve_size) {
	case 10:
		return count_primes() == 4;
	case 100:
		return count_primes() == 25;
	case 1000:
		return count_primes() == 168;
	case 10000:
		return count_primes() == 1229;
	case 100000:
		return count_primes() == 9592;
	case 1000000:
		return count_primes() == 78498;
	case 10000000:
		return count_primes() == 664579;
	case 100000000:
		return count_primes() == 5761455;
	case 1000000000:
		return count_primes() == 50847534;
	case 10000000000:
		return count_primes() == 455052511;
	default:
		return false;
	}
}

static void run_sieve(size_t n) {
	if(bits == NULL) {
		bits = malloc(n);
	}
	memset(bits, true, n);

	sieve_size = n;
	uint64_t factor = 3;
	uint64_t q = sqrt(sieve_size);

	while(factor <= q) {
		for(size_t num = factor; num < sieve_size; num += 2) {
			if(bits[num]) {
				factor = num;
				break;
			}
		}
		for(size_t num = factor * factor; num < sieve_size; num += factor * 2) {
			bits[num] = false;
		}
		factor += 2;
	}
}

static void print_results(bool show_results, double duration, uint64_t passes) {
	if(show_results) {
		printf("2, ");
	}
	size_t count = (sieve_size >= 2);
	for(size_t num = 3; num <= sieve_size; num += 2) {
		if(bits[num]) {
			if(show_results) {
				printf("%zu, ", num);
			}
			++count;
		}
	}

	if(show_results) {
		printf("\n");
	}

	printf("Passes: %" PRIu64 ", Time: %lf, Avg: %lf, Limit: %zu, Count1: %zu, Count2: %zu, Valid: %u\n",
			passes,
			duration,
			duration / passes,
			sieve_size,
			count,
			count_primes(),
			validate_results());
}

static uint64_t get_time(void) {
	struct timespec ts;
	timespec_get(&ts, TIME_UTC);
	return (ts.tv_sec * 1000000000) + ts.tv_nsec;
}

int main(void) {
	uint64_t passes = 0;
	uint64_t start = get_time();

	while(true) {
		run_sieve(1000000);
		++passes;
		if((get_time() - start) / 1000000000 >= 5) {
			print_results(false, (get_time() - start) / 1000000000, passes);
			break;
		}
	}
	return 0;
}
