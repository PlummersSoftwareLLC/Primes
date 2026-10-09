#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>
#include <stdlib.h>
#include <pthread.h>
#include <sched.h>

#define LIMIT 1000000
#define EXPECTED_COUNT 78498
#define TARGET_DURATION 5.0
#define BUFFER_SIZE (48 * 1024)

extern uint64_t run_sieve_pass(uint8_t* buffer, uint64_t limit);

int main(void) {
    uint8_t* buffer = (uint8_t*)aligned_alloc(64, BUFFER_SIZE);
    if (!buffer) {
        perror("aligned_alloc failed");
        return 1;
    }

    // Verification run
    uint64_t initial_count = run_sieve_pass(buffer, LIMIT);
    if (initial_count != EXPECTED_COUNT) {
        fprintf(stderr, "FAILED: Got %lu primes, expected %d\n", initial_count, EXPECTED_COUNT);
        free(buffer);
        return 1;
    }

    // Benchmark loop
    struct timespec start, now;
    clock_gettime(CLOCK_MONOTONIC, &start);
    uint64_t passes = 0;
    double elapsed = 0.0;

    while (true) {
        run_sieve_pass(buffer, LIMIT);
        passes++;

        clock_gettime(CLOCK_MONOTONIC, &now);
        elapsed = (now.tv_sec - start.tv_sec) + (now.tv_nsec - start.tv_nsec) * 1e-9;
        if (elapsed >= TARGET_DURATION) {
            break;
        }
    }

    // Plummer Drag Race format: <label>;<iterations>;<total_time>;<num_threads>;<tags>
    printf("TACITVS_st;%lu;%.6f;1;algorithm=wheel,faithful=yes,bits=1\n", passes, elapsed);

    free(buffer);
    return 0;
}
