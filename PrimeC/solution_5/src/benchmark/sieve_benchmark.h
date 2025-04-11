// prepare the benchmark settings using defaults
static inline benchmark_settings_t initBenchmarkSettings(const counter_t threads) 
{
    benchmark_settings_t benchmark_settings = option.fixed_benchmark_settings;
    if (!option.fixed_benchmark_settings.stripe_faster    ) { benchmark_settings.stripe_faster    = 64;        }
    if (!option.fixed_benchmark_settings.largestep_faster ) { benchmark_settings.largestep_faster = 128;       }
    if (!option.fixed_benchmark_settings.blocksize_bits   ) { benchmark_settings.blocksize_bits   = 32*1024*8; }
    if (!option.fixed_benchmark_settings.vectorsize       ) { benchmark_settings.vectorsize       = 256;       }
    if (!option.fixed_benchmark_settings.algorithm        ) { benchmark_settings.algorithm        = 1;         }
    benchmark_settings.threads = threads;
    return benchmark_settings;
}

// check the settings to make sure they are valid, dont overlap, etc.
static inline benchmark_settings_t checkBenchmarkSettings(benchmark_settings_t benchmark_settings) 
{
    counter_t prime_max = prime_stop(benchmark_settings.factor_max);
    benchmark_settings.stripe_faster     = min(benchmark_settings.stripe_faster, prime_max);
    benchmark_settings.largestep_faster  = max(benchmark_settings.largestep_faster, 64);
    benchmark_settings.largestep_faster  = min(benchmark_settings.largestep_faster, VECTOR_SIZE_BITS);
    benchmark_settings.largestep_faster  = min(benchmark_settings.largestep_faster, prime_max*2+1);
    benchmark_settings.largestep_faster  = max(benchmark_settings.largestep_faster, 2); // allow for conversion from step to prime
    benchmark_settings.blocksize_bits    = min(benchmark_settings.blocksize_bits, benchmark_settings.factor_max/2);
    if (benchmark_settings.blocksize_bits == 0) benchmark_settings.blocksize_bits = benchmark_settings.factor_max/2;
    if (benchmark_settings.algorithm < 1 || benchmark_settings.algorithm >23) benchmark_settings.algorithm = 1; // default to sieve algorithm 1
    if (benchmark_settings.vectorsize != 128 && benchmark_settings.vectorsize != 256 && benchmark_settings.vectorsize != 512) {
        benchmark_settings.vectorsize = 256; // default to 256 bit vectors
    }
    return benchmark_settings;
}

static inline char* setBenchmarkSettingAsString(char* settings_string, benchmark_settings_t benchmark_settings) 
{
    snprintf(settings_string, 50, "s%03ju-l%03ju-b%07ju-v%3ju-a%1ju", (uintmax_t)benchmark_settings.stripe_faster, (uintmax_t)benchmark_settings.largestep_faster, (uintmax_t)benchmark_settings.blocksize_bits, (uintmax_t)benchmark_settings.vectorsize, (uintmax_t)benchmark_settings.algorithm);
    return settings_string;
}

static char      global_settings_string[50] = ""; // settings string to use where it is directly outputted
static inline char *getBenchmarkSettingAsString(benchmark_settings_t benchmark_settings) 
{
    return setBenchmarkSettingAsString(global_settings_string, benchmark_settings);
}

static inline void prepareBenchmarkGlobals(benchmark_settings_t benchmark_settings) 
{
    global_stripeprime_faster   = benchmark_settings.stripe_faster;
    global_largestep_faster     = benchmark_settings.largestep_faster;
    global_blocksize_bits       = benchmark_settings.blocksize_bits;
    global_vectorsize           = benchmark_settings.vectorsize;
    global_algorithm            = benchmark_settings.algorithm;  
    verbose5({ printf("Using settings " COLOR_GREEN "%s" COLOR_RESET "\n", getBenchmarkSettingAsString(benchmark_settings)); })
}

static int checkSieveWithBenchmarkSettings(benchmark_settings_t benchmark_settings) 
{
    benchmark_settings = checkBenchmarkSettings(benchmark_settings);
    prepareBenchmarkGlobals(benchmark_settings);
    const counter_t factor_max = benchmark_settings.factor_max;
    struct sieve_t* sieve_check = shakeSieve(factor_max);
    const int valid = validateSieve(sieve_check, factor_max);
    verbose3( if (!valid) {
        printf("The sieve is " COLOR_RED "NOT" COLOR_RESET " valid for settings " COLOR_GREEN "%s" COLOR_RESET " with factor %ju\n", getBenchmarkSettingAsString(benchmark_settings), (uintmax_t) factor_max);
        deepAnalyzeSieve(sieve_check);
    })
    sieve_delete(sieve_check);
    return valid;
}

static inline void requestPower(void) 
{
    #ifdef __APPLE__
        pthread_set_qos_class_self_np(QOS_CLASS_USER_INTERACTIVE, 0);
    #elif defined(__linux__)
        if (option.fixed_benchmark_settings.threads == 1) {
            cpu_set_t cpuset;
            CPU_ZERO(&cpuset);
            CPU_SET(0, &cpuset);
            sched_setaffinity(0, sizeof(cpu_set_t), &cpuset);
        }   
        // Set real-time scheduling
        struct sched_param param;
        param.sched_priority = sched_get_priority_max(SCHED_FIFO); // Mid-level real-time priority
        if (sched_setscheduler(0, SCHED_FIFO, &param) != 0) {
            // Fallback if we don't have permission
            param.sched_priority = 0;
            sched_setscheduler(0, SCHED_OTHER, &param);
            int n10 = nice(-10); // Try to increase priority within normal scheduling
            int n20 = nice(-20); // Try to increase priority within normal scheduling
        }
    #endif
}

static inline double benchmarkTime() 
{
    struct timespec time;
    #ifdef __APPLE__
        clock_gettime(CLOCK_MONOTONIC_RAW, &time);
    #else
        clock_gettime(CLOCK_MONOTONIC, &time);
    #endif
    return (time.tv_sec + time.tv_nsec * 1e-9);
}

static inline void updateBenchmarkResult(benchmark_result_t *result, const counter_t passes, const double time_elapsed) {
    result->passes       += passes;
    result->elapsed_time += time_elapsed / result->settings.threads;
    result->avg           = result->passes / result->elapsed_time;
}

static benchmark_result_t benchmark(benchmark_settings_t benchmark_settings) 
{
    benchmark_result_t benchmark_result = { .settings = checkBenchmarkSettings(benchmark_settings), .passes = 0, .elapsed_time = 0, .avg = 0 };

    // set global variables used in the sieve functions
    prepareBenchmarkGlobals(benchmark_result.settings); 

    // prepare for the benchmark
    const counter_t sieve_size   = benchmark_result.settings.factor_max;
    const double time_sample     = benchmark_result.settings.sample_duration; // do this before we set the clock
    register double time_elapsed = 0;
    register counter_t passes    = 0;
    
    #ifdef _OPENMP
        omp_set_num_threads(benchmark_result.settings.threads);
        #pragma omp parallel reduction(+:passes) reduction(+:time_elapsed)
        {
            requestPower();
            double thread_elapsed = 0;
            const double time_start = benchmarkTime(), time_target = time_start + time_sample; // use target time to avoid substraction in the while loop
            while (thread_elapsed <= time_target) {
                struct sieve_t *sieve = shakeSieve(sieve_size);
                sieve_delete(sieve);
                thread_elapsed = benchmarkTime();         
                passes++;
            }
            time_elapsed = thread_elapsed - time_start;
        }
    #else
        requestPower();
        const double time_start = benchmarkTime(), time_target = time_start + time_sample; // use target time to avoid substraction in the while loop
        while (time_elapsed <= time_target) {
            struct sieve_t *sieve = shakeSieve(sieve_size);
            sieve_delete(sieve);
            time_elapsed = benchmarkTime();         
            passes++;
        }
        time_elapsed -= time_start;         
    #endif

    // calculate results
    updateBenchmarkResult(&benchmark_result, passes, time_elapsed);

    return benchmark_result;
}
