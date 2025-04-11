static int compareTuningResults(const void *resultA, const void *resultB) 
{
    return (((benchmark_result_t *)resultB)->avg > ((benchmark_result_t *)resultA)->avg ? 1 : -1);
}

static inline int sameTuningResult(benchmark_result_t* resultA, benchmark_result_t* resultB) 
{
    return (resultA->settings.stripe_faster    == resultB->settings.stripe_faster &&
            resultA->settings.largestep_faster == resultB->settings.largestep_faster &&
            resultA->settings.blocksize_bits   == resultB->settings.blocksize_bits &&
            resultA->settings.vectorsize       == resultB->settings.vectorsize &&
            resultA->settings.algorithm        == resultB->settings.algorithm
        );
}

static inline void setSettingsFromTuning(benchmark_settings_t* benchmark_settings, benchmark_settings_t* tuning_settings) 
{
    benchmark_settings->stripe_faster     = tuning_settings->stripe_faster;
    benchmark_settings->largestep_faster  = tuning_settings->largestep_faster;
    benchmark_settings->blocksize_bits    = tuning_settings->blocksize_bits;
    benchmark_settings->vectorsize        = tuning_settings->vectorsize;
    benchmark_settings->algorithm         = tuning_settings->algorithm;
}

static inline void resetBenchmarkResult(benchmark_result_t* benchmark_result, benchmark_settings_t benchmark_settings) 
{
    benchmark_result->settings     = benchmark_settings;
    benchmark_result->passes       = 0;
    benchmark_result->elapsed_time = 0;
    benchmark_result->avg          = 0;
}

static inline void printTuningResult(benchmark_result_t tuning_result) 
{
    verbose2({
        if (tuning_result.settings.stripe_faster == usqrt(tuning_result.settings.factor_max)/2) {
            printf(COLOR_BLUE "average" COLOR_RESET);
        } else {
            printf("average");
        }
        printf( COLOR_BOLD_YELLOW "%13.6f" COLOR_RESET " with options " COLOR_BOLD_GREEN "%s" COLOR_RESET 
                " was achieved with " COLOR_BOLD_YELLOW "%3ju" COLOR_RESET " passes in " COLOR_BOLD_YELLOW "%f" COLOR_RESET " seconds\n", 
            tuning_result.avg, getBenchmarkSettingAsString(tuning_result.settings), (uintmax_t)tuning_result.passes, tuning_result.elapsed_time); 
    })
}

typedef struct {
    counter_t sieve_bits;
    counter_t prime_max;
    counter_t stripe_faster_steps;
    counter_t largestep_faster_steps;
    counter_t blocksize_steps;
    double    time_target;
    double    sample_duration;
    counter_t tuning_results_max;
    counter_t step;
} tuning_parameters_t;

static counter_t buildInitialTuningTable(benchmark_result_t* tuning_result, benchmark_settings_t tuning_settings, tuning_parameters_t tuning_parameters) {
    counter_t tuning_results = 0;
    for (counter_t algorithm=1; algorithm <= 2; algorithm++) {
        for (counter_t vectorsize = 128; vectorsize <= 512; vectorsize *= 2) {
            counter_t stripe_faster = 0;
            do {
                stripe_faster += tuning_parameters.stripe_faster_steps; // do this here to over force processing of prime_max as well
                for (counter_t largestep_faster = 64; largestep_faster <= vectorsize; largestep_faster += tuning_parameters.largestep_faster_steps) { 
                    counter_t blocksize_bits=0;
                    do { // do loop because user can set this beyound sieve_bits
                        blocksize_bits += tuning_parameters.blocksize_steps;

                        // override with user settings if specified
                        if (option.fixed_benchmark_settings.stripe_faster)    { stripe_faster    = option.fixed_benchmark_settings.stripe_faster;    }
                        if (option.fixed_benchmark_settings.largestep_faster) { largestep_faster = option.fixed_benchmark_settings.largestep_faster; }
                        if (option.fixed_benchmark_settings.blocksize_bits)   { blocksize_bits   = option.fixed_benchmark_settings.blocksize_bits;   }
                        if (option.fixed_benchmark_settings.vectorsize)       { vectorsize       = option.fixed_benchmark_settings.vectorsize;       }
                        if (option.fixed_benchmark_settings.algorithm)        { algorithm        = option.fixed_benchmark_settings.algorithm;        }

                        if (blocksize_bits > tuning_parameters.sieve_bits) blocksize_bits = tuning_parameters.sieve_bits;
                        if (stripe_faster >= tuning_parameters.prime_max)  {
                            blocksize_bits = tuning_parameters.sieve_bits;
                            stripe_faster  = tuning_parameters.prime_max;
                        }

                        // set variables
                        tuning_settings.blocksize_bits   = blocksize_bits;
                        tuning_settings.stripe_faster    = stripe_faster;
                        tuning_settings.largestep_faster = largestep_faster;
                        tuning_settings.vectorsize       = vectorsize;
                        tuning_settings.algorithm        = algorithm;
                        tuning_settings.sample_duration  = tuning_parameters.sample_duration;

                        if (tuning_settings.stripe_faster < tuning_parameters.prime_max 
                            && tuning_settings.blocksize_bits == tuning_parameters.sieve_bits
                            && (option.fixed_benchmark_settings.stripe_faster == 0) // only break if user didn't set this
                        ) break; // stripe will do the entire sieve as well

                        resetBenchmarkResult(&tuning_result[tuning_results++], checkBenchmarkSettings(tuning_settings));

                        verbose4( { 
                            printf("\rTuning...adding option " COLOR_BOLD_GREEN "%5ju" COLOR_RESET " for settings " COLOR_GREEN "%s" COLOR_RESET "\n", 
                            (uintmax_t)tuning_results, getBenchmarkSettingAsString(tuning_settings)); 
                        })

                        if (option.fixed_benchmark_settings.blocksize_bits) break;
                        if (stripe_faster == tuning_parameters.prime_max) break;
                    } while ( blocksize_bits < tuning_parameters.sieve_bits );
                    if (option.fixed_benchmark_settings.largestep_faster) break;
                }
                if (option.fixed_benchmark_settings.stripe_faster) break;
           } while (stripe_faster < tuning_parameters.prime_max); 
           if (option.fixed_benchmark_settings.vectorsize) break;
        }
        if (option.fixed_benchmark_settings.algorithm) break;
    }
    return tuning_results;
}

static counter_t addTuningVariations(benchmark_result_t* tuning_result, counter_t tuning_results, counter_t tuning_results_selected, tuning_parameters_t tuning_parameters) 
{
    counter_t new_tuning_results = tuning_results;
    
    for (counter_t i=0; i<tuning_results_selected; i++) {
        benchmark_settings_t tuning_settings = tuning_result[i].settings;

        counter_t largestep_faster_steps_diff = tuning_parameters.largestep_faster_steps >> tuning_parameters.step; 
        if (!option.fixed_benchmark_settings.largestep_faster) {
            if (largestep_faster_steps_diff > 1) {
                if (tuning_settings.largestep_faster < VECTOR_SIZE_BITS - largestep_faster_steps_diff) {
                    resetBenchmarkResult(&tuning_result[new_tuning_results], tuning_settings);
                    tuning_result[new_tuning_results].settings.largestep_faster += largestep_faster_steps_diff;
                    new_tuning_results++;
                }
                if (tuning_settings.largestep_faster > 2) {
                    resetBenchmarkResult(&tuning_result[new_tuning_results], tuning_settings);
                    tuning_result[new_tuning_results].settings.largestep_faster -= 2;
                    new_tuning_results++;
                }
            }
        }

        counter_t stripe_faster_steps_diff = tuning_parameters.stripe_faster_steps >> tuning_parameters.step; 
        if (!option.fixed_benchmark_settings.stripe_faster 
            && tuning_settings.blocksize_bits != tuning_parameters.sieve_bits) { // lower than stripe_faster is same as full sieve
            if (stripe_faster_steps_diff > 1) {
                if (tuning_settings.stripe_faster < tuning_parameters.prime_max - stripe_faster_steps_diff) {
                    resetBenchmarkResult(&tuning_result[new_tuning_results], tuning_settings);
                    tuning_result[new_tuning_results].settings.stripe_faster += stripe_faster_steps_diff;
                    new_tuning_results++;
                }

                if (tuning_settings.stripe_faster > stripe_faster_steps_diff) {
                    resetBenchmarkResult(&tuning_result[new_tuning_results], tuning_settings);
                    tuning_result[new_tuning_results].settings.stripe_faster -= stripe_faster_steps_diff;
                    new_tuning_results++;
                }
            }
        }
    }
    
    return new_tuning_results;
}


// join results with the same settings; set the second one to zero. Sorting will flush them out.
static counter_t joinTuningResults(benchmark_result_t* tuning_result, const counter_t tuning_results) 
{
    counter_t tuning_results_selected = tuning_results;
    
    for (counter_t i=0; i<tuning_results; i++) {
        if (tuning_result[i].avg != 0) {
            for (counter_t j=i+1; j<tuning_results; j++) {
                if (tuning_result[j].avg != 0 && sameTuningResult(&tuning_result[i], &tuning_result[j])) {
                    updateBenchmarkResult(&tuning_result[i], tuning_result[j].passes, tuning_result[j].elapsed_time);
                    resetBenchmarkResult(&tuning_result[j], tuning_result[j].settings);
                    tuning_results_selected--;
                }
            }
        }
    }
    qsort(tuning_result, (size_t)tuning_results, sizeof(benchmark_result_t), compareTuningResults);
    return tuning_results_selected;
}

static benchmark_result_t tuneSieveSettings(int tune_level, benchmark_settings_t start_tuning_settings) 
{
    verbose2( printf("Tuning...building options..."); )

    counter_t prime_max = usqrt(start_tuning_settings.factor_max) / 2; // divide by 2 to compensate for bitwise representation 

    tuning_parameters_t tuning_parameters = {
        .prime_max = prime_max ,
        .stripe_faster_steps = 64,
        .largestep_faster_steps = 32,
        .sample_duration = option.initial_sample_duration,
        .sieve_bits = start_tuning_settings.factor_max >> 1,
        .time_target = 0, // This field wasn't initialized in your original code
        .step = 0,
        .tuning_results_max = 0,
        .blocksize_steps = 8*1024*8
    };
    
    switch (tune_level) {
        case 1:
            tuning_parameters.stripe_faster_steps    = prime_max/4;
            tuning_parameters.largestep_faster_steps = 32;
            tuning_parameters.sample_duration        = option.initial_sample_duration;
            break;
        case 2:
            tuning_parameters.stripe_faster_steps    = prime_max/8;
            tuning_parameters.largestep_faster_steps = 16;
            tuning_parameters.sample_duration        = option.initial_sample_duration*2;
            break;
    }
    
    // prepare a table to store the tuning results
    const size_t max_results = ((prime_max)+1) * ((size_t)(VECTOR_SIZE_BITS/tuning_parameters.largestep_faster_steps)+1) * 32 * 6; // 6 strategies
    benchmark_result_t* tuning_result = malloc(max_results * sizeof(tuning_result));
    benchmark_settings_t tuning_settings = initBenchmarkSettings(start_tuning_settings.threads);

    // start the timer
    const double time_start = benchmarkTime();
    const double time_target = time_start + option.tune_duration_max; // in seconds
    double time_elapsed = time_start;

    // build the initial tuning table
    counter_t tuning_results = buildInitialTuningTable(tuning_result, tuning_settings, tuning_parameters);
    if (tuning_results == 0) {
        verbose1( fprintf(stderr, "No tuning results found\n"); )
        free(tuning_result);
        exit(1);
    }

    verbose_at2( { printf("\rTuning...build %ju options..",(uintmax_t)tuning_results); })
    verbose3(    { printf("Finding the best option by reevaluating the top options with a longer sample duration.\n"); })

    // reduce the tuning results to the best options
    // keep the best of the results and reevaluate them with a longer sample duration
    counter_t tuning_results_max = tuning_results; // keep this value for verbose messages
    
    for (tuning_parameters.step = 1; tuning_results >= 1; tuning_parameters.step++) {
        for (counter_t i=0; i<tuning_results; i++) {
            benchmark_settings_t tuning_settings = tuning_result[i].settings;

            if (tuning_parameters.step < 8 && tuning_results > 16) {
                tuning_settings.sample_duration = tuning_parameters.step * option.initial_sample_duration;
            }
            else {
                tuning_settings.sample_duration = tuning_parameters.step * option.next_sample_duration;
            }
            verbose2( { 
                printf("\rTuning step " COLOR_BOLD_GREEN "%2ju" COLOR_RESET " with " COLOR_BOLD_YELLOW "%5ju" COLOR_RESET " options. Benchmarking option " COLOR_BOLD_GREEN "%5ju" COLOR_RESET ": %s in progress  ",(uintmax_t)tuning_parameters.step,(uintmax_t)tuning_results, (uintmax_t)i, getBenchmarkSettingAsString(tuning_settings)  ); 
            })
            
            // Check if the settings are valid
            #ifdef COMPILE_CHECKALL
            tuning_settings = checkBenchmarkSettings(tuning_settings);
            const int valid = checkSieveWithBenchmarkSettings(tuning_settings);
            if (!valid) {
                verbose1( fprintf(stderr, "The sieve is " COLOR_RED "NOT" COLOR_RESET " valid for settings %s with factor %ju\n", getBenchmarkSettingAsString(tuning_settings), (uintmax_t) tuning_settings.factor_max); )
                exit(1);
            }
            #endif
            
            // Perform the benchmark
            tuning_result[i] = benchmark(tuning_settings);

            time_elapsed = benchmarkTime();
            if (time_elapsed > time_target) { break; } // stop when time expired
        }

        // Sort the results by average time and then exit if we ran out of time. After sorting so the best results are on top, so good time to stop
        qsort(tuning_result, (size_t)tuning_results, sizeof(benchmark_result_t), compareTuningResults);
        time_elapsed = benchmarkTime();
        if (time_elapsed > time_target) { 
            verbose3( { printf("\nTune time expired\n"); } );  
            break; 
        }

        // Keep the best results. Be careful with the last 16 and benchmark them further
        counter_t tuning_results_selected = tuning_results * option.tune_keeppercent_longlist / 100;
        if (tuning_results_selected < 16 && tuning_results > 16) tuning_results_selected = 16;
        if (tuning_results_selected < 16) tuning_results_selected = tuning_results * option.tune_keeppercent_shortlist / 100;
        if (tuning_results_selected <= 1) break;

        verbose3( {
            printf("\n\r" COLOR_DARK_GRAY "(iteration %1ju) - %5ju options left - selecting %5ju" COLOR_RESET " options\n",(uintmax_t)tuning_parameters.step, (uintmax_t)tuning_results,(uintmax_t)tuning_results_selected) ; 
            printf(">> "); printTuningResult(tuning_result[0]); printf("" COLOR_RESET "");
            for (counter_t tuning_result_index = 1; tuning_result_index < min( option.show_tuning_results_max,tuning_results); tuning_result_index++) {
                if (tuning_result_index < tuning_results_selected) printf(">>."); else printf("...");
                printTuningResult(tuning_result[tuning_result_index]);
            }
        })

        tuning_results = tuning_results_selected;
        verbose2( tuning_results_max += tuning_results; )

        // Add variations of the best results
        if (tuning_parameters.step < 8) { // allow for 2^8 = 256 variations
            tuning_results = addTuningVariations(tuning_result, tuning_results, tuning_results_selected, tuning_parameters);
            tuning_results = joinTuningResults(tuning_result, tuning_results);
        }
    }

    // Take best result
    benchmark_result_t best_result = tuning_result[0];
    free(tuning_result);

    time_elapsed = (time_elapsed - time_start);
    verbose2( { printf(COLOR_CLEAR_LINE "Tuning done in %.1f seconds. Evaluated %ju options in %ju steps. Best result: ", (time_elapsed), (uintmax_t) tuning_results_max, (uintmax_t) tuning_parameters.step ); printTuningResult(best_result);} );
    return best_result;
}