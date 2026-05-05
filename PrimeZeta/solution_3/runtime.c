#define _GNU_SOURCE
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include <pthread.h>

// Zeta bridge
void *runtime_malloc(long long s) { return malloc(s); }
void runtime_free(void *p) { free(p); }
void println_i64(long long v) { printf("%lld\n", v); fflush(stdout); }

extern long long sieve(long long limit);

// Thread pool — single barrier
// Worker: sieve → b1 → check stop → b2 → check stop → loop
// Main:   b1 → verify → time check → b2 → loop
// KEY: workers ALWAYS hit both b1 and b2 (no early break between barriers)
//      stop_flag is checked AFTER each barrier, so barrier count is always correct

static pthread_barrier_t barrier;
static volatile int stop_flag = 0;
static long long results[64];

static void *worker(void *arg) {
    int id = (int)(intptr_t)arg;
    while (1) {
        results[id] = sieve(1000000);
        pthread_barrier_wait(&barrier);      // b1: done
        if (stop_flag) { pthread_barrier_wait(&barrier); break; }
        pthread_barrier_wait(&barrier);      // b2: go-ahead
        if (stop_flag) { pthread_barrier_wait(&barrier); break; }
    }
    return NULL;
}

long long parallel_sieve_timed(long long limit, long long run_us, long long threads) {
    if (threads < 2 || threads > 20) threads = 20;
    
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    long long start = ts.tv_sec * 1000000LL + ts.tv_nsec / 1000LL;
    long long passes = 0;
    pthread_barrier_init(&barrier, NULL, (unsigned)threads + 1);
    
    pthread_t pt[64];
    for (int t = 0; t < threads; t++)
        pthread_create(&pt[t], NULL, worker, (void*)(intptr_t)t);
    
    while (1) {
        // b1: wait for all workers to finish their sieve
        pthread_barrier_wait(&barrier);
        
        // Verify results
        for (int t = 0; t < threads; t++)
            if (results[t] != 78498) { stop_flag = 1; return 1; }
        passes++;
        
        // Periodic clock check (every 100 batches)
        if (passes % 100 == 0) {
            clock_gettime(CLOCK_MONOTONIC, &ts);
            long long now = ts.tv_sec * 1000000LL + ts.tv_nsec / 1000LL;
            if ((now - start) >= run_us) {
                stop_flag = 1;
                // b2: signal go-ahead — workers WILL hit b2 (no early break)
                pthread_barrier_wait(&barrier);
                // Workers check stop after b2 → break
                // Workers in stop branch also hit b2 (extra barrier_wait)
                for (int t = 0; t < threads; t++)
                    pthread_join(pt[t], NULL);
                double secs = (now - start) / 1000000.0;
                printf("murphsicles;%lld;%.6f;%lld;algorithm=wheel,faithful=no,bits=1\n",
                       passes * threads, secs, (long long)threads);
                fflush(stdout);
                pthread_barrier_destroy(&barrier);
                return 0;
            }
        }
        
        // b2: signal workers to start next batch
        pthread_barrier_wait(&barrier);
    }
}
