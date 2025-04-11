static int performBenchmarks(struct options_t option)
{
    for(counter_t threads=option.fixed_benchmark_settings.threads, runs = 0; threads >= 1 && runs < 4; threads = (threads/2), runs++ ) {

        // prepare settings
        benchmark_settings_t benchmark_settings = initBenchmarkSettings(threads);

        // tuning - try combinations of different settings and apply these
        #ifdef COMPILE_TUNE
        if (option.tunelevel) { 
            benchmark_result_t tuning_result = tuneSieveSettings(option.tunelevel, benchmark_settings);
            setSettingsFromTuning(&benchmark_settings, &(tuning_result.settings));
        }
        #endif

        // one last check to make sure this is a valid algorithm for these settings
        benchmark_settings = checkBenchmarkSettings(benchmark_settings);
        debug_final_plan = 1; // allow to count something in only one run
        if (!checkSieveWithBenchmarkSettings(benchmark_settings)) { 
            verbose1( fprintf(stderr, "The sieve is " COLOR_RED "NOT" COLOR_RESET " valid for settings %s with factor %ju\n", 
                              getBenchmarkSettingAsString(benchmark_settings), (uintmax_t) benchmark_settings.factor_max); )
            return 1; 
        } 
        else { verbose2( printf("Verified that algortihm with settings %s and max %ju is " COLOR_GREEN "valid" COLOR_RESET ".\n", 
                        getBenchmarkSettingAsString(benchmark_settings), (uintmax_t) benchmark_settings.factor_max); 
        )}
        debug_final_plan = 0;
    
        // warm up the cache for a short time
        verbose2( printf("Warming up the cache and processing units in %.1f seconds\n", option.warmup_duration); )	
        benchmark_settings_t final_tuning_settings = benchmark_settings;
        final_tuning_settings.sample_duration = option.warmup_duration;
        benchmark(final_tuning_settings);

        // perform benchmark -> outputs passes, elapsed time and avg in result 
        verbose2( printf("Benchmarking with settings: " COLOR_GREEN "%s" COLOR_RESET " (stripeprime, largestep, blocksize) and " COLOR_GREEN "%ju" COLOR_RESET " threads for " COLOR_GREEN "%.1f" COLOR_RESET " seconds\n"
                         "Results: " COLOR_BLINK "(wait " COLOR_GREEN "%.1lf" COLOR_RESET " seconds)" COLOR_BLINK_OFF "...", 
                         getBenchmarkSettingAsString(benchmark_settings),(uintmax_t)benchmark_settings.threads, benchmark_settings.sample_duration, benchmark_settings.sample_duration );
        )
        debug_final_benchmarking = 1; // allow to count something in the final benchmark runs
        benchmark_result_t benchmark_result = benchmark(benchmark_settings);
        debug_final_benchmarking = 0;

        // report results
        verbose2({
            printf("\nResult: Passes " COLOR_YELLOW "%ju" COLOR_RESET " " COLOR_GREEN "(per %.1f seconds)" COLOR_RESET " - average " COLOR_YELLOW "%.1f" COLOR_RESET " per second using " COLOR_MAGENTA "%ju" COLOR_RESET " threads\n", 
                    (uintmax_t) benchmark_result.passes, benchmark_result.elapsed_time, benchmark_result.avg, (uintmax_t) benchmark_result.settings.threads);

            if (benchmark_result.settings.threads > 1) 
            printf("Used " COLOR_MAGENTA "%ju" COLOR_RESET " threads. Passes per thread: " COLOR_YELLOW "%ju" COLOR_RESET " " COLOR_GREEN "(per %.1f seconds)" COLOR_RESET " - average " COLOR_YELLOW "%.1f" COLOR_RESET " per second per thread.\n", 
                   (uintmax_t)benchmark_result.settings.threads, (uintmax_t) benchmark_result.passes / benchmark_result.settings.threads, benchmark_result.elapsed_time, benchmark_result.avg / benchmark_result.settings.threads);
            
            printf(COLOR_GREEN "Output message:" COLOR_RESET " \n"); 
        })
        
        // output the results in a format that can be parsed by the benchmarking system
        printf("%s%s;%ju;%f;%ju;algorithm=%s,faithful=yes,bits=1",algorithm_name, option.extension, (uintmax_t)benchmark_result.passes, benchmark_result.elapsed_time, (uintmax_t)threads, algorithm_type);

        // add extra information to the output for research purposes
        verbose1({ 
            if (option.dockerfile_type) printf(";docker=" COLOR_BLUE "%s" COLOR_RESET "",option.dockerfile_type);
            printf(";" COLOR_GREEN "%s" COLOR_RESET " total " COLOR_YELLOW "%ju" COLOR_RESET "", getBenchmarkSettingAsString(benchmark_settings), (uintmax_t)benchmark_result.passes); 
        }) 
        printf("\n");

        if (threads > 4 && threads < 8) threads = 8; // force looking at 4 and 2 by setting threads to 8 which will be halved (4) next loop run
    }
    return 0;
}