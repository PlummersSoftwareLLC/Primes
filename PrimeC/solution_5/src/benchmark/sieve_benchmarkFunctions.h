// Define benchmark timing constants
#include "../bitstorage/bitstorage_setBitsTrueFunctionList.h"

#define nonvector 1
#define BENCHMARK_DURATION 0.002  // seconds per test

static inline void clear_cache() {
    const size_t size = 128*1024*1024;
    char* data = (char*)malloc(size);
    for (size_t i = 0; i < size; i++) data[i] = i;
    free(data);
}

static void listSetBitsTrueMethods() {
    for (int m = 0; m < methods; m++) {
        printf("%3d %-50s", m, setBitsTrueMethods[m].name);
        if ((m + 1) % 4 == 0 || m == methods - 1) printf("\n");
    }
}

static void printStripePasses(const counter_t stripe_passes[methods + 1], counter_t max_value) {
    for (int method = 0; method < methods; method++) {
        if (method >= nonvector && stripe_passes[method] == max_value && max_value > 0) {
            printf("\033[32m%6ju" COLOR_RESET " ", (uintmax_t)stripe_passes[method]); // Green for max
        } else if (method >= nonvector && stripe_passes[method] >= max_value * 0.95 && stripe_passes[method] < max_value && max_value > 0) {
            printf("\033[33m%6ju" COLOR_RESET " ", (uintmax_t)stripe_passes[method]); // Yellow for within 5% of max
        } else if (method == 0 && max_value > 0 && stripe_passes[0] >= max_value * 0.95) {
            printf("\033[32;1m%6ju" COLOR_RESET " ", (uintmax_t)stripe_passes[method]); // Green for method when 95% of max
        } else {
            printf("%6ju ", (uintmax_t)stripe_passes[method]);
        }
    }
}
// this function is used for the benchmarking
// it knows all the different ways to setBitsTrue for a given range and step
// it benchmarks the different methods and keeps the resulting times or passed in an array
// it sorts the results from best to worst
// the array contains for each stepsize the best method
static inline void benchmarkSetBitsTrue(void* restrict bitstorage, const counter_t block_start, const counter_t block_stop, const counter_t prime_start, const counter_t prime_max)
{
    counter_t prime = prime_start;

    counter_t stripe_passes[1000][methods+1];
    for(int i=0; i<1000; i++) { for(int j=0; j<methods; j++) { stripe_passes[i][j] = 0; } }

    listSetBitsTrueMethods();

    // Loop through all primes and benchmark the methods
    while (prime < prime_max) {
        register const counter_t step = prime * 2 + 1;
        register counter_t start = compute_start(prime, block_start);

        for(int m=0; m < methods; m++) {
            const SetBitsTrueMethod* method = &setBitsTrueMethods[m];
            
            // Skip disabled methods
            // if (!method->enabled) continue;
            if (step >= method->min_step && step <= method->max_step) {
                clear_cache();
                const double time_start = benchmarkTime();
                const double time_target = time_start + BENCHMARK_DURATION;
                double time_elapsed = 0;
                counter_t passes = 0;
                
                while (time_elapsed <= time_target) {
                        method->func(bitstorage, start, step, block_stop);
                        passes++;
                        time_elapsed = benchmarkTime();
                }
                stripe_passes[step][m] = passes;
            }
        }
        prime = searchBitFalse(bitstorage, prime);
    }

    // Print the results. First row has the method numbers
    printf( COLOR_BLUE "Step      ");  for(int method=0; method < methods; method++) printf("%6ju ", (uintmax_t)method);  printf( COLOR_RESET "\n");

    // Loop through all steps and print the results
    for(int step=1; step<prime_max*2+1; step+=2) {
        counter_t prime = (step-1) >> 1;
        if (checkBitFalse(bitstorage, prime) ) {
            // Find the maximum and second largest value among methods 4-18
            counter_t max_value = 0 ,second_max_value = 0;
            for(int method=0; method<methods; method++) {
                if (stripe_passes[step][method] > max_value) {
                    second_max_value = max_value;
                    max_value = stripe_passes[step][method];
                } 
                else if (stripe_passes[step][method] > second_max_value) {
                    second_max_value = stripe_passes[step][method];
                }
            }
            
            // Print all method values, highlighting the max and second largest among methods 4-18
            printf( COLOR_BLUE "Step %4ju " COLOR_RESET, (uintmax_t)step);
            printStripePasses(stripe_passes[step], max_value);
            printf("\n");
        }
    }
}

static inline void benchmarkSieveSetBitsTrue()
{
    struct sieve_t* sieve = shakeSieve(1000000/2);
    benchmarkSetBitsTrue(sieve->bitstorage, 256*1024, min(1000000/2, 512*1024), 2, 500);
    sieve_delete(sieve);
}

static void playStepplan(struct sieve_t* sieve, const counter_t prime_max, setBitsTrueFunc* local_best_stepfunction) 
{
    counter_t prime = 1, range_start = 0;
    while (prime < prime_max) {
        register const counter_t step  = prime * 2 + 1;
        register counter_t start = compute_start(prime, range_start);
        (*local_best_stepfunction[step])(sieve->bitstorage, start, step, sieve->bits);
        prime = searchBitFalse(sieve->bitstorage, prime);
    }
}

static void createStepplan(benchmark_settings_t settings) {
    counter_t range_start = 0, range_stop = settings.factor_max/2;
    counter_t prime = 1, prime_max = prime_stop(range_stop), step_max = prime_max * 2 + 1;

    struct sieve_t* sieve = sieve_create(range_stop*2);
    void* bitstorage = sieve->bitstorage;

    int stepplan[step_max];
    setBitsTrueFunc best_stepfunction[step_max];
    counter_t stripe_passes[step_max][methods+1];

    for(int i=0; i<step_max; i++) { for(int j=0; j<methods; j++) { stripe_passes[i][j] = 0; } }

    listSetBitsTrueMethods();

    printf( COLOR_BLUE "Step      ");  for(int method=0; method < methods; method++) printf("%6ju ", (uintmax_t)method);  printf( COLOR_RESET "\n");

    while (prime < prime_max) {
        register const counter_t step  = prime * 2 + 1;
        register counter_t start = compute_start(prime, range_start);

        // try all methods for this step and benchmark them
        for(int m=0; m<methods; m++) {
            SetBitsTrueMethod method = setBitsTrueMethods[m];
            if (step >= method.min_step && step <= method.max_step) {
                const double time_start = benchmarkTime(), time_target = time_start + 0.005;
                double time_elapsed = 0;
                counter_t passes = 0;
                
                while (time_elapsed <= time_target) {
                        playStepplan(sieve, prime-1, &best_stepfunction[0]); // prepare the cache in the relevant state by replaying the stepplan
                        method.func(bitstorage, start, step, range_stop);
                        passes++;
                        time_elapsed = benchmarkTime();
                }
                stripe_passes[step][m] = passes;
            }
        }

        // find the best method for this step - skip 0
        counter_t max_value = 0;
        for(int m=1; m<methods; m++) {
            if (stripe_passes[step][m] > max_value) {
                max_value = stripe_passes[step][m];
                stepplan[step] = m;
                best_stepfunction[step] = setBitsTrueMethods[m].func;
            }
        }
        
        printf( COLOR_BLUE "Step %4ju " COLOR_RESET, (uintmax_t)step);
        printStripePasses(stripe_passes[step], max_value);
        printf("Selecting method %2ju %s \n", (uintmax_t) stepplan[step], setBitsTrueMethods[stepplan[step]].name);

        prime = searchBitFalse(bitstorage, prime);
    }
    sieve_delete(sieve);

    // benchmark the final stepplan for 5 seconds
    printf("Benchmarking the final stepplan of the best functions for 5 seconds\n");
    counter_t passes = 0;
    const double time_start = benchmarkTime();
    const double time_target = time_start + 5.0;
    double time_elapsed = 0;

    while (time_elapsed <= time_target) {
            // prepare the cache in the relevant state
            playStepplan(sieve, prime_max, &best_stepfunction[0]);
            passes++;
            time_elapsed = benchmarkTime();
    }
    printf("Final stepplan: %ju passes in 5 seconds\n", (uintmax_t)passes);

}
