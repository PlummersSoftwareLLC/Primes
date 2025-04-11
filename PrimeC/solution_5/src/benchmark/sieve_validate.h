static void deepAnalyzeWithBenchmarkSettings(benchmark_settings_t benchmark_settings) 
{
    prepareBenchmarkGlobals(benchmark_settings);
    struct sieve_t* sieve = shakeSieve(benchmark_settings.factor_max);
    deepAnalyzeSieve(sieve);
    sieve_delete(sieve);
}

// check with every sievesize, but not with every blocksize
static int __attribute__((cold)) 
checkSieveAlgorithm(benchmark_settings_t benchmark_settings)
{
    verbose2( printf("Validating variant .. "); )
    verbose3( printf("\n"); )

    // validate algorithm - run one time for all sizes
    for (counter_t sieveSize_check = 100; sieveSize_check <= 10000000; sieveSize_check *=10) {
        verbose3( printf("..Checking size %ju ...",(uintmax_t)sieveSize_check); ) verbose4( printf("\n"); )
        benchmark_settings.blocksize_bits = sieveSize_check / 2;
        benchmark_settings.factor_max = sieveSize_check;
        benchmark_settings = checkBenchmarkSettings(benchmark_settings);

        int valid = checkSieveWithBenchmarkSettings(benchmark_settings); 

        if (!valid) {
            verbose1( fprintf(stderr,"Invalid count for %ju Settings used: %s\n",(uintmax_t)sieveSize_check, getBenchmarkSettingAsString(benchmark_settings)); )
            deepAnalyzeWithBenchmarkSettings(benchmark_settings);
            if (option.check == 7) exit(1);
            return valid;
        }
        verbose3( printf(COLOR_GREEN "valid" COLOR_RESET " for %ju Settings used: %s\n", (uintmax_t)sieveSize_check, getBenchmarkSettingAsString(benchmark_settings)); )
    }
    verbose2( printf(COLOR_GREEN "valid" COLOR_RESET " algorithm\n"); )
    
    return 1;
}

// check with every sievesize and blocksize
static int __attribute__((cold)) 
checkSieveAlgorithmAll(benchmark_settings_t benchmark_settings)
{
    verbose2( printf("Validating variant... "); ) verbose3( printf("\n"); ) 

    // validate algorithm - run one time for all sizes
    for (counter_t sieveSize_check = 100; sieveSize_check <= 1000000; sieveSize_check *=10) {
        verbose3( {
            printf("..Checking size %ju ...",(uintmax_t)sieveSize_check); 
            verbose4( printf("\n"); )
        })
        for (counter_t blocksize_bits=1024; blocksize_bits<=32*1024*8; blocksize_bits *= 2) {
            verbose4( printf("....Blocksize %ju:",(uintmax_t)blocksize_bits); )
            benchmark_settings.blocksize_bits = blocksize_bits;
            benchmark_settings.factor_max = sieveSize_check;
            benchmark_settings = checkBenchmarkSettings(benchmark_settings);
            int valid = checkSieveWithBenchmarkSettings(benchmark_settings); 

            if (!valid) {
                verbose1( fprintf(stderr,"Invalid count for %ju Settings used: %s\n",(uintmax_t)sieveSize_check, getBenchmarkSettingAsString(benchmark_settings)); )
                deepAnalyzeWithBenchmarkSettings(benchmark_settings);
                if (option.check == 7) exit(1);
                return valid;
            }
            else {
                verbose4( printf(COLOR_GREEN "valid" COLOR_RESET " for %ju Settings used: %s\n", (uintmax_t)sieveSize_check, getBenchmarkSettingAsString(benchmark_settings)); )
            }
        }
        verbose3( printf(COLOR_GREEN "valid" COLOR_RESET " for %ju Settings used: %s\n", (uintmax_t)sieveSize_check, getBenchmarkSettingAsString(benchmark_settings)); )
    }
    verbose2( printf(COLOR_GREEN "valid" COLOR_RESET " algorithm\n"); )
    
    return 1;
}

static void __attribute__((cold)) 
showResult(benchmark_settings_t benchmark_settings)
{
    verbose2( printf("Show result set:\n"); )
    struct sieve_t* sieve = shakeSieve(benchmark_settings.factor_max);
    showPrimesinSieve(sieve, option.show_explain_factor_max);
    sieve_delete(sieve);
}

static inline void __attribute__((cold)) 
handleCheckOption(int check, benchmark_settings_t benchmark_settings) {
    #ifdef COMPILE_CHECK_STRIPERS
    if (check >= 4) checkSetBitsTrueMethods(setBitsTrueMethods, 0, benchmark_settings.factor_max);
    if (check >= 5) {
        for (counter_t sieveSize_check = 100; sieveSize_check <= 1000000; sieveSize_check *=10) {
            checkSetBitsTrueMethods(setBitsTrueMethods, 0, sieveSize_check);
        }
    }
    if (check >= 6) checkSetBitsTrueMethodsBlocks(setBitsTrueMethods, 0, benchmark_settings.factor_max);
    #endif

    if (check >= 1) checkSieveWithBenchmarkSettings(benchmark_settings);
    if (check >= 2) checkSieveAlgorithm(benchmark_settings);
    if (check >= 3) checkSieveAlgorithmAll(benchmark_settings);

    if (check == 7) exit(0);
}
