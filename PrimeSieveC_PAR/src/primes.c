#include <time.h>
#include <math.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <pthread.h>
#include <stdbool.h>
#include <inttypes.h>

struct sieve_thread {
	uint64_t passes;
	uint64_t start;
	bool valid;
	double duration;
};

static const size_t sieve_size = 1000000;

static size_t count_primes(bool* bits) {
	size_t count = (sieve_size >= 2);
	for(size_t i = 3; i < sieve_size; i += 2) {
		if(bits[i]) {
			++count;
		}
	}
	return count;
}

static bool validate_results(bool* bits) {
	switch(sieve_size) {
	case 10:
		return count_primes(bits) == 4;
	case 100:
		return count_primes(bits) == 25;
	case 1000:
		return count_primes(bits) == 168;
	case 10000:
		return count_primes(bits) == 1229;
	case 100000:
		return count_primes(bits) == 9592;
	case 1000000:
		return count_primes(bits) == 78498;
	case 10000000:
		return count_primes(bits) == 664579;
	case 100000000:
		return count_primes(bits) == 5761455;
	case 1000000000:
		return count_primes(bits) == 50847534;
	case 10000000000:
		return count_primes(bits) == 455052511;
	default:
		return false;
	}
}

static void run_sieve(bool* bits) {
	memset(bits, true, sieve_size);

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

static void print_results(double duration, uint64_t passes, bool valid) {
	printf("Passes: %" PRIu64 ", Time: %lf, Avg: %lf, Limit: %zu, Valid: %u\n",
			passes,
			duration,
			duration / passes,
			sieve_size,
			valid);
}

static uint64_t get_time(void) {
	struct timespec ts;
	timespec_get(&ts, TIME_UTC);
	return (ts.tv_sec * 1000000000) + ts.tv_nsec;
}

static void* do_sieve(void* data) {
	struct sieve_thread* this = data;
	uint64_t start = this->start;
	bool* bits = malloc(sieve_size);
	while((get_time() - start) / 1000000000 < 5) {
		run_sieve(bits);
		++this->passes;
	}
	this->duration = (get_time() - start) / 1000000000.l;
	this->valid = validate_results(bits);
	free(bits);
	return this;
}

int main(void) {
	uint64_t passes = 0;
	uint64_t start = get_time();

	uint64_t nproc = sysconf(_SC_NPROCESSORS_ONLN);
	pthread_t threads[nproc];

	for(size_t thread = 0; thread < nproc; ++thread) {
		struct sieve_thread* sieve = calloc(1, sizeof(struct sieve_thread));
		sieve->start = start;
		pthread_create(&threads[thread], NULL, do_sieve, sieve);
	}

	double duration = 0;
	bool valid = true;

	for(size_t thread = 0; thread < nproc; ++thread) {
		struct sieve_thread* sieve;
		pthread_join(threads[thread], (void**) &sieve);
		passes += sieve->passes;
		duration = fmax(duration, sieve->duration);
		if(!sieve->valid) {
			valid = false;
		}
		free(sieve);
	}
	print_results(duration, passes, valid);
	return 0;
}
