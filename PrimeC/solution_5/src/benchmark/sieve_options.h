
typedef struct  {
    counter_t factor_max;
    counter_t stripe_faster;
    counter_t largestep_faster;
    counter_t blocksize_bits;
    counter_t vectorsize;
    counter_t algorithm;
    counter_t threads;
    double    sample_duration;
} benchmark_settings_t;

typedef struct  {
    benchmark_settings_t settings;
    counter_t passes;
    double    elapsed_time;
    double    avg;
} benchmark_result_t;

static struct options_t {
    benchmark_settings_t fixed_benchmark_settings;
    counter_t show_explain_factor_max;
    counter_t show_tuning_results_max;
    counter_t show_primes_on_error;
    counter_t verbose_level;
    counter_t explain;
    counter_t timers;
    counter_t check;
    counter_t tunelevel;
    counter_t extended_output;
    double    initial_sample_duration;
    double    next_sample_duration;
    double    warmup_duration;
    double    tune_duration_max;
    counter_t tune_keeppercent_longlist;
    counter_t tune_keeppercent_shortlist;
    char*     program_name;
    char*     dockerfile_type;
    char*     extension;
} option;

static struct options_t __attribute__((cold)) 
setDefaultOptions() 
{
    option.show_explain_factor_max    = 0;
    option.show_tuning_results_max    = 50;
    option.show_primes_on_error       = 100;
    option.verbose_level              = 0;
    option.explain                    = 0;
    option.timers                     = 0;

    option.check                      = 1; // set to 2 to stop after the check algorithm
    option.tunelevel                  = 1;
    option.initial_sample_duration    = 0.001;
    option.next_sample_duration       = 0.003;
    option.warmup_duration            = 1;
    option.tune_duration_max          = 10.0;
    option.tune_keeppercent_longlist  = 10;
    option.tune_keeppercent_shortlist = 60;

    option.fixed_benchmark_settings.factor_max              = 1000000;
    option.fixed_benchmark_settings.threads                 = 1;
    option.fixed_benchmark_settings.stripe_faster           = 0;
    option.fixed_benchmark_settings.largestep_faster        = 0;
    option.fixed_benchmark_settings.blocksize_bits          = 0;
    option.fixed_benchmark_settings.vectorsize              = 0;
    option.fixed_benchmark_settings.algorithm               = 0;
    option.fixed_benchmark_settings.sample_duration         = 5;

    option.dockerfile_type = getenv("DOCKERFILE_TYPE"); 

    // changes though compilation options
    #ifdef _OPENMP
    option.fixed_benchmark_settings.threads                 = omp_get_max_threads();
    #endif

    #ifdef COMPILE_TIMERS
    option.timers = 1;
    #endif

    #ifdef _OPENMP
    option.extension = "_epar";
    #else
    option.extension = "";
    #endif

    #ifdef ALGORITHM_CLASSIC
    option.tunelevel = 0;
    #endif

    return option;
}

#include "sieve_timers.h"