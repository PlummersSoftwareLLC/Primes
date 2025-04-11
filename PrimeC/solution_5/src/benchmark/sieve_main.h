#include "sieve_check.h"
#include "sieve_benchmark.h"

#ifdef COMPILE_EXPLAIN
#include "sieve_explain.h"
#endif

#ifdef COMPILE_CHECK_STRIPERS
#include "sieve_checkFunctions.h"
#endif

#ifdef COMPILE_BENCHMARK_STRIPERS
#include "sieve_benchmarkFunctions.h"
#endif

#ifdef COMPILE_TUNE
#include "sieve_tune.h"
#endif

#include "sieve_performBenchmark.h"
#include "sieve_validate.h"
#include "sieve_usage.h"
#include "sieve_parseCommandline.h"

int main(int argc, char *argv[]) 
{
    setbuf(stdout, NULL); // prevent buffering of stdout
    setDefaultOptions();
    parseCommandLine(argc, argv);

    verbose3({ printf("Sieve algorithm by Rogier van Dam - 2025\n"
                       "Find all primes up to " COLOR_YELLOW "%ju" COLOR_RESET " using the Sieve of Eratosthenes (https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes)\n"
                       , (uintmax_t)option.fixed_benchmark_settings.factor_max);})
    verbose2({ printf("\nRunning sieve variant " COLOR_YELLOW "%s" COLOR_RESET "%s" COLOR_BLUE "%s" COLOR_RESET " with max %ju\n", 
                         algorithm_name, (option.dockerfile_type ? " in docker " : ""), (option.dockerfile_type ? option.dockerfile_type : ""), (uintmax_t)option.fixed_benchmark_settings.factor_max); })

    #ifdef COMPILE_EXPLAIN
    if (option.explain) {
        if (option.verbose_level < 6) option.verbose_level = 6;
        option.check = 0;
        explainSieveShake(option.fixed_benchmark_settings);
        return (0);
    }
    #endif

    // command line --check can be used to check the algorithm for all sieve/blocksize combinations
    if (option.check) handleCheckOption(option.check, option.fixed_benchmark_settings);

    #ifdef COMPILE_BENCHMARK_STRIPERS
    if (option.tunelevel) {
        if (option.tunelevel == 3) {
            benchmarkSieveSetBitsTrue();
            return (0);
        }
        if (option.tunelevel == 4) {
            createStepplan(option.fixed_benchmark_settings);
            return (0);
        }
    }
    #endif

    #ifdef COMPILE_TIMERS
    if (option.timers) {
        timer_init();
        verbose2( printf("Timing the different parts of the algorithm\n"); )
    }
    #endif

    int valid = performBenchmarks(option);

    // show results for --show command line option and other developer information
    if (option.show_explain_factor_max > 0) showResult(option.fixed_benchmark_settings);

    #ifdef COMPILE_TIMERS
    if (option.timers) print_timing_table();
    #endif
    
    if (debug_hits) { verbose2( printf("Hits: %ju\n",(uintmax_t)debug_hits); ) }

    return valid;
}
