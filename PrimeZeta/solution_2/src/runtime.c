#include <stdio.h>
#include <time.h>
#include <stdlib.h>

long long get_time_us(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (long long)ts.tv_sec * 1000000LL + (long long)ts.tv_nsec / 1000LL;
}

long long current_us(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (long long)ts.tv_sec * 1000000LL + (long long)ts.tv_nsec / 1000LL;
}

long long time_is_up(long long start_us, long long target_us) {
    long long now = current_us();
    return (now - start_us) >= target_us ? 1 : 0;
}

void print_result(long long passes, long long elapsed_us) {
    double secs = (double)elapsed_us / 1000000.0;
    printf("murphsicles;%lld;%.6f;1;algorithm=wheel,faithful=yes,bits=1\n", passes, secs);
    fflush(stdout);
}

void println_i64(long long value) {
    printf("%lld\n", value);
    fflush(stdout);
}
