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
#include <errno.h>

#define LIMIT 1000000
#define EXPECTED_COUNT 78498
#define TARGET_DURATION 5.0
// 500,000 bits = 62,500 bytes. Allocate 64 KB (65,536 bytes) aligned to 64 bytes.
#define BUFFER_SIZE (64 * 1024)

// External assembly entry point conforming to faithful sieve lifecycle
// Buffer is cleared, sieved, and counted within run_faithful_sieve
extern uint64_t run_faithful_sieve(uint8_t* buffer, uint64_t limit);

int main(void) {
    // 1. Soft CPU Affinity Pinning (Ignores EPERM/EINVAL in virtualized CI containers)
    cpu_set_t cpuset;
    CPU_ZERO(&cpuset);
    CPU_SET(0, &cpuset);
    int aff_res = pthread_setaffinity_np(pthread_self(), sizeof(cpu_set_t), &cpuset);
    if (aff_res != 0) {
        fprintf(stderr, "Affinity warning: Could not pin to Core 0 (running unrestricted).\n");
    }

    // 2. Dynamic 64-byte aligned allocation
    uint8_t* buffer = (uint8_t*)aligned_alloc(64, BUFFER_SIZE);
    if (!buffer) {
        perror("aligned_alloc failed");
        return 1;
    }

    // 3. Step 1: Verification Run
    uint64_t initial_count = run_faithful_sieve(buffer, LIMIT);
    if (initial_count != EXPECTED_COUNT) {
        fprintf(stderr, "FAILED: Got %lu, Expected %d\n", initial_count, EXPECTED_COUNT);
        free(buffer);
        return 1;
    }
    fprintf(stderr, "VALID: %lu primes verified.\n", initial_count);

    // 4. Step 2: Timed Benchmark Loop
    struct timespec start, now;
    clock_gettime(CLOCK_MONOTONIC, &start);
    uint64_t passes = 0;
    double elapsed = 0.0;

    while (true) {
        run_faithful_sieve(buffer, LIMIT);
        passes++;

        clock_gettime(CLOCK_MONOTONIC, &now);
        elapsed = (now.tv_sec - start.tv_sec) + (now.tv_nsec - start.tv_nsec) * 1e-9;
        if (elapsed >= TARGET_DURATION) {
            break;
        }
    }

    // 5. Official Plummer Drag Race Output Format (Strict Faithful Category)
    printf("TACITVS_faithful_st;%lu;%.6f;1;algorithm=base,faithful=yes,bits=1\n", passes, elapsed);

    free(buffer);
    return 0;
}
