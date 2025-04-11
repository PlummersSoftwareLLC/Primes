static inline int __attribute__((cold, const)) 
isdigit_local(int c) {
    return (c >= '0' && c <= '9');
}

static inline char __attribute__((cold, nonnull, returns_nonnull)) 
*strrchr_local(const char *s, int c) {
    const char *p = NULL;
    for (;;) {
        if (*s == (char)c) p = s;
        if (*s++ == '\0')  return (char *)p;
    }
}

static inline int __attribute__((cold)) 
strcmp_local(const char *s1, const char *s2) {
    while (*s1 && (*s1 == *s2)) { s1++; s2++; }
    return (((unsigned char)*s1 - (unsigned char)*s2) == 0);
}

// Helper function to check for next argument
static inline int __attribute__((cold))
ensure_next_arg(int arg, int argc, char *program_name, const char *option_name) {
    if (arg >= argc) {
        fprintf(stderr, "No %s specified\n", option_name);
        usage(program_name, 1);
        return 0; // Never reached due to usage() exit
    }
    return 1;
}

// Custom string to uintmax_t converter
static inline int __attribute__((cold))
str_to_uintmax(const char *str, uintmax_t *value) {
    // skip empty string and non-digit characters
    if (!str || !*str || !isdigit_local(*str)) return 0;
    
    uintmax_t result = 0;
    for (; *str && isdigit_local(*str); str++) {
        // Check for overflow before adding new digit
        if (result > UINTMAX_MAX / 10) return 0;
        result *= 10;
        
        uintmax_t digit = *str - '0';
        if (result > UINTMAX_MAX - digit) return 0;
        result += digit;
    }
    
    // If we stopped on non-whitespace/null/hyphen, it's invalid
    if (*str && *str != ' ' && *str != '\t'  && *str != '-') return 0;
    
    *value = result;
    return 1;
}

// Helper function for integer argument parsing
static inline int __attribute__((cold))
parse_int_arg(char *arg_str, counter_t *value, counter_t max_value, char *program_name, const char *error_msg) {
    uintmax_t temp_value; // Use a local variable instead of a pointer
    if (str_to_uintmax(arg_str, &temp_value) != 1 || temp_value > max_value) {
        verbose1({ fprintf(stderr, "Error: %s: %s\n", error_msg, arg_str); usage(program_name, 1); });
        return 0; // Never reached due to usage() exit
    }
    *value = (counter_t)temp_value; // Assign the value after casting
    return 1;
}

// Custom string to double converter
static inline int __attribute__((cold))
str_to_double(const char *str, double *value) {
    if (!str || !*str) return 0;
    
    // Parse integer part
    double result = 0.0;
    int have_digits = 0;
    for (; *str && isdigit_local(*str); str++) {
        result = result * 10.0 + (*str - '0');
    }
    
    // Parse fractional part
    if (*str == '.') {
        str++;
        double fraction = 0.1;
        for (;*str && isdigit_local(*str); str++) {
            result += (*str - '0') * fraction;
            fraction *= 0.1;
        }
    }
   
    // If we stopped on non-whitespace/null, it's invalid
    if (*str && *str != ' ' && *str != '\t') return 0;
    
    *value = result;
    return 1;
}

// Helper function for double argument parsing
static inline int __attribute__((cold))
parse_double_arg(char *arg_str, double *value,  char *program_name, const char *error_msg) {
    if (str_to_double(arg_str, value) != 1) {
        verbose1({ fprintf(stderr, "Error: %s: %s\n", error_msg, arg_str); usage(program_name, 1); });
        return 0; // Never reached due to usage() exit
    }
    return 1;
}

// Helper function to handle set parameters
static inline void __attribute__((cold))
handle_set_parameter(char param_type, uintmax_t value, struct options_t *option) {
    switch(param_type) {
        case 's': option->fixed_benchmark_settings.stripe_faster = value; break;
        case 'l': option->fixed_benchmark_settings.largestep_faster = value; break;
        case 'b': option->fixed_benchmark_settings.blocksize_bits = value; break; 
        case 'v': option->fixed_benchmark_settings.vectorsize = value; break;
        case 'a': option->fixed_benchmark_settings.algorithm = value; break;
        case 't': option->fixed_benchmark_settings.threads = value; break;
        default:
            fprintf(stderr, "Error: Unknown parameter '%c'\n", param_type);
            usage(NULL, 1); // program_name will be set when function is called
    }
}

static inline void __attribute__((cold))
parse_set_parameter(char *arg, char *program_name, struct options_t *option) {
    char *p = arg;
    while (*p) {
        // Skip any hyphens
        if (*p == '-') {
            p++;
            continue;
        }

        // Get the parameter type
        char param_type = *p++;
        uintmax_t value = 0;

        // Skip to first digit
        while (*p && !isdigit_local(*p)) p++;

        // Parse the number
        if (*p && isdigit_local(*p)) {
            if (str_to_uintmax(p, &value) != 1) {
                fprintf(stderr, "Error: Invalid number after '%c'\n", param_type);
                usage(program_name, 1);
            }

            // Apply the value based on parameter type
            handle_set_parameter(param_type, value, option);

            // Skip the parsed number
            while (*p && isdigit_local(*p)) p++;
        }
    }

}

static void __attribute__((cold)) 
parseCommandLine(int argc, char *argv[])
{
    option.program_name = argv[0];
    option.program_name = max(option.program_name, strrchr_local(option.program_name, '/')+1);
    option.program_name = max(option.program_name, strrchr_local(option.program_name, '\\')+1);
    char *program_name = option.program_name;

    // processing command line changes to options
    for (int arg=1; arg < argc; arg++) {
        if (strcmp_local(argv[arg], "--help")) { 
            usage(program_name, 0); 
        }
        else if (strcmp_local(argv[arg], "--verbose")) { 
            ensure_next_arg(++arg, argc, program_name, "verbose level");
            parse_int_arg(argv[arg], &option.verbose_level, 9, program_name, "Invalid verbose level");
        } 
        #ifdef COMPILE_EXPLAIN
        else if (strcmp_local(argv[arg], "--explain")) { 
            option.explain = 1;  
            verbose2(printf("Explain ON\n"));
        }
        #endif
        #ifdef COMPILE_TIMERS 
        else if (strcmp_local(argv[arg], "--timers")) { 
            option.timers = 2; 
        }
        #endif
        else if (strcmp_local(argv[arg], "--check")) { 
            ensure_next_arg(++arg, argc, program_name, "check level");
            parse_int_arg(argv[arg], &option.check, 7, program_name, "Invalid check level");
            verbose2(printf("Check level set to %d\n", option.check));
        }
        else if (strcmp_local(argv[arg], "--nocheck")) { 
            option.check = 0; 
        }
        else if (strcmp_local(argv[arg], "--tune")) { 
            ensure_next_arg(++arg, argc, program_name, "tune level");
            parse_int_arg(argv[arg], &option.tunelevel, 4, program_name, "Invalid tune level");
            verbose2(printf("Tune level set to %d\n", option.tunelevel));
        }
        else if (strcmp_local(argv[arg], "--time")) {
            ensure_next_arg(++arg, argc, program_name, "time");
            parse_double_arg(argv[arg], &option.fixed_benchmark_settings.sample_duration, program_name, "Invalid max time");
            verbose2(printf("Max time is set to %f seconds\n", option.fixed_benchmark_settings.sample_duration));
        }
        else if (strcmp_local(argv[arg], "--show")) {
            ensure_next_arg(++arg, argc, program_name, "show maximum");
            parse_int_arg(argv[arg], &option.show_explain_factor_max, option.fixed_benchmark_settings.factor_max, program_name, "Invalid show maximum");
            verbose2(printf("Show maximum set to %ju\n", (uintmax_t)option.show_explain_factor_max);)
        }
        else if (strcmp_local(argv[arg], "--max")) {
            ensure_next_arg(++arg, argc, program_name, "sieve maximum");
            parse_int_arg(argv[arg], &option.fixed_benchmark_settings.factor_max, COUNTER_T_MAX_VALUE, program_name, "Invalid sieve maximum");
            verbose2(printf("Maximum set to %ju\n", (uintmax_t)option.fixed_benchmark_settings.factor_max);)
        }
        else if (strcmp_local(argv[arg], "--set")) {
            ensure_next_arg(++arg, argc, program_name, "settings for --set");
            parse_set_parameter(argv[arg], program_name, &option);
            verbose2(printf("Initial settings: " COLOR_BOLD_GREEN "%s" COLOR_RESET "\n", getBenchmarkSettingAsString(option.fixed_benchmark_settings));)
        }
        else if (strcmp_local(argv[arg], "--threads")) { 
            ensure_next_arg(++arg, argc, program_name, "thread maximum");
            
        #ifdef _OPENMP
            counter_t max_threads = (counter_t) omp_get_max_threads();
            if (strcmp_local(argv[arg], "all")) {
                option.fixed_benchmark_settings.threads = max_threads;
            }
            else if (strcmp_local(argv[arg], "half")) {
                option.fixed_benchmark_settings.threads = max_threads>>1;
            }
            else if (sscanf(argv[arg], "%d", (int *)&option.fixed_benchmark_settings.threads) != 1) { 
                fprintf(stderr, "Error: Invalid max threads: %s\n", argv[arg]); 
                usage(program_name, 1); 
            }
            
            // Ensure thread count is within valid range
            if (option.fixed_benchmark_settings.threads < 1) {
                option.fixed_benchmark_settings.threads = 1;
            }
            if (option.fixed_benchmark_settings.threads > max_threads) {
                option.fixed_benchmark_settings.threads = max_threads;
            }
            
            verbose2(printf("Thread maximum set to %ju\n", (uintmax_t)option.fixed_benchmark_settings.threads));
        #else
            verbose2(printf("This is the version without multithreading - ignoring threads\n"));
        #endif
        }
        else if (sscanf(argv[arg], "%ju", (uintmax_t*)&option.fixed_benchmark_settings.factor_max) != 1) {
            verbose1({ fprintf(stderr, "Invalid size %s\n", argv[arg]); usage(program_name, 1); });
            verbose2(printf("Maximum set to %ju\n", (uintmax_t)option.fixed_benchmark_settings.factor_max));
        }
    }
}
