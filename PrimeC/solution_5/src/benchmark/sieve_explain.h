#ifdef COMPILE_EXPLAIN
static void __attribute__((cold)) 
explainSieveShake(benchmark_settings_t benchmark_settings) 
{
    benchmark_settings = checkBenchmarkSettings(benchmark_settings);
    prepareBenchmarkGlobals(benchmark_settings);

    debug_final_plan = 1;
    struct sieve_t* sieve = shakeSieve(benchmark_settings.factor_max);
    debug_final_plan = 0;

    option.verbose_level = 3; // set back to 3 because we don't need explanations anymore
    if (option.show_explain_factor_max) {
        showPrimesinSieve(sieve, option.show_explain_factor_max);
    }

    int valid = validateSieve(sieve, benchmark_settings.factor_max);
    if (!valid) {
        printf("The sieve for factors up to %ju is \033[0;31m\033[5mNOT\033[0;0m valid...\n", (uintmax_t) benchmark_settings.factor_max);
        deepAnalyzeSieve(sieve);
    }
    else {
        printf("The sieve for factors up to %ju is \033[0;32mvalid\033[0;0m\n", (uintmax_t) benchmark_settings.factor_max);
    }
    sieve_delete(sieve);

    printf("Hits: %ju\n",(uintmax_t)debug_hits);
    
    #ifdef COMPILE_TIMERS
    if (option.timers) print_timing_table();
    #endif
}
#endif
