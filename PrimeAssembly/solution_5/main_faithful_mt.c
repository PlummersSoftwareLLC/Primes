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
#include <sys/sysinfo.h>

#define LIMIT 1000000
#define EXPECTED_COUNT 78498
#define TARGET_DURATION 5.0
#define BUFFER_SIZE (64 * 1024)

extern uint64_t run_faithful_sieve(uint8_t* buffer, uint64_t limit);

typedef struct {
    int thread_id;
    uint64_t passes;
    double elapsed;
    volatile bool* stop_flag;
} thread_data_t;

static volatile bool g_stop = false;

void* worker_thread(void* arg) {
    thread_data_t* data = (thread_data_t*)arg;
    uint8_t* buffer = (uint8_t*)aligned_alloc(64, BUFFER_SIZE);
    if (!buffer) return NULL;

    // Verification check on worker buffer
    uint64_t count = run_faithful_sieve(buffer, LIMIT);
    if (count != EXPECTED_COUNT) {
        fprintf(stderr, "Thread %d FAILED verification: got %lu\n", data->thread_id, count);
        free(buffer);
        return NULL;
    }

    uint64_t passes = 0;
    while (!(*data->stop_flag)) {
        run_faithful_sieve(buffer, LIMIT);
        passes++;
    }

    data->passes = passes;
    free(buffer);
    return NULL;
}

int main(int argc, char** argv) {
    int num_threads = get_nprocs();
    if (argc > 1) {
        int custom_threads = atoi(argv[1]);
        if (custom_threads > 0) num_threads = custom_threads;
    }
    if (num_threads < 1) num_threads = 1;

    // Single verification pass before launch
    uint8_t* test_buf = (uint8_t*)aligned_alloc(64, BUFFER_SIZE);
    uint64_t initial_count = run_faithful_sieve(test_buf, LIMIT);
    free(test_buf);

    if (initial_count != EXPECTED_COUNT) {
        fprintf(stderr, "FAILED initial verification: %lu\n", initial_count);
        return 1;
    }
    fprintf(stderr, "VALID: %lu primes verified across %d threads.\n", initial_count, num_threads);

    pthread_t* threads = (pthread_t*)malloc(sizeof(pthread_t) * num_threads);
    thread_data_t* tdata = (thread_data_t*)malloc(sizeof(thread_data_t) * num_threads);

    struct timespec start, now;
    clock_gettime(CLOCK_MONOTONIC, &start);

    for (int i = 0; i < num_threads; i++) {
        tdata[i].thread_id = i;
        tdata[i].passes = 0;
        tdata[i].stop_flag = &g_stop;
        pthread_create(&threads[i], NULL, worker_thread, &tdata[i]);
    }

    // Main thread times the 5.0 second window
    while (true) {
        clock_gettime(CLOCK_MONOTONIC, &now);
        double elapsed = (now.tv_sec - start.tv_sec) + (now.tv_nsec - start.tv_nsec) * 1e-9;
        if (elapsed >= TARGET_DURATION) {
            g_stop = true;
            break;
        }
        // Sleep briefly to avoid burning CPU on timer polling
        struct timespec sleep_ts = {0, 1000000}; // 1ms
        nanosleep(&sleep_ts, NULL);
    }

    uint64_t total_passes = 0;
    for (int i = 0; i < num_threads; i++) {
        pthread_join(threads[i], NULL);
        total_passes += tdata[i].passes;
    }

    clock_gettime(CLOCK_MONOTONIC, &now);
    double total_elapsed = (now.tv_sec - start.tv_sec) + (now.tv_nsec - start.tv_nsec) * 1e-9;

    printf("TACITVS_faithful_mt;%lu;%.6f;%d;algorithm=base,faithful=yes,bits=1\n",
           total_passes, total_elapsed, num_threads);

    free(threads);
    free(tdata);
    return 0;
}
