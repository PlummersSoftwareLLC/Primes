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
#include <unistd.h>
#include <stdatomic.h>

#define LIMIT 1000000
#define EXPECTED_COUNT 78498
#define TARGET_DURATION 5.0
#define BUFFER_SIZE (48 * 1024)

extern uint64_t run_sieve_pass(uint8_t* buffer, uint64_t limit);

static volatile atomic_bool stop_flag = false;

typedef struct {
    int thread_id;
    uint64_t passes;
} worker_data_t;

void* worker(void* arg) {
    worker_data_t* data = (worker_data_t*)arg;
    uint8_t* buffer = (uint8_t*)aligned_alloc(64, BUFFER_SIZE);
    if (!buffer) return NULL;

    // Verify first pass
    uint64_t count = run_sieve_pass(buffer, LIMIT);
    if (count != EXPECTED_COUNT) {
        fprintf(stderr, "Thread %d validation failed: got %lu\n", data->thread_id, count);
        free(buffer);
        return NULL;
    }

    uint64_t passes = 1;
    while (!atomic_load_explicit(&stop_flag, memory_order_relaxed)) {
        run_sieve_pass(buffer, LIMIT);
        passes++;
    }

    data->passes = passes;
    free(buffer);
    return NULL;
}

int main(int argc, char** argv) {
    int num_threads = (int)sysconf(_SC_NPROCESSORS_ONLN);
    if (num_threads < 1) num_threads = 1;
    if (argc > 1) {
        int custom_threads = atoi(argv[1]);
        if (custom_threads > 0) num_threads = custom_threads;
    }

    pthread_t* threads = (pthread_t*)malloc(sizeof(pthread_t) * num_threads);
    worker_data_t* wdata = (worker_data_t*)malloc(sizeof(worker_data_t) * num_threads);

    for (int i = 0; i < num_threads; i++) {
        wdata[i].thread_id = i;
        wdata[i].passes = 0;
    }

    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);

    for (int i = 0; i < num_threads; i++) {
        pthread_create(&threads[i], NULL, worker, &wdata[i]);
    }

    // Benchmark duration
    struct timespec req = { .tv_sec = 5, .tv_nsec = 0 };
    nanosleep(&req, NULL);
    atomic_store_explicit(&stop_flag, true, memory_order_release);

    for (int i = 0; i < num_threads; i++) {
        pthread_join(threads[i], NULL);
    }
    clock_gettime(CLOCK_MONOTONIC, &end);

    double elapsed = (end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) * 1e-9;
    uint64_t total_passes = 0;
    for (int i = 0; i < num_threads; i++) {
        total_passes += wdata[i].passes;
    }

    // Plummer Drag Race format: <label>;<iterations>;<total_time>;<num_threads>;<tags>
    printf("TACITVS_mt;%lu;%.6f;%d;algorithm=wheel,faithful=yes,bits=1\n", total_passes, elapsed, num_threads);

    free(threads);
    free(wdata);
    return 0;
}
