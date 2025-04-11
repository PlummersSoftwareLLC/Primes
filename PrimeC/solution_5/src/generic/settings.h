// Set to 1-9 to enable compiling different verbose levels
// Higher levels are necessary for --explain.
// But they cost time, so are only compiled if specified

#ifndef COMPILE_VERBOSE_LEVEL
    #define COMPILE_VERBOSE_LEVEL 2  
#endif

// these options are set using the command line tool ./sieve
#ifdef COMPILE_FULL
#undef COMPILE_CHECKALL
#undef COMPILE_EXPLAIN                  
#undef COMPILE_BENCHMARK_STRIPERS
#undef COMPILE_CHECK_STRIPERS
#undef COMPILE_TUNE
#define COMPILE_CHECKALL                 
#define COMPILE_EXPLAIN                  
#define COMPILE_BENCHMARK_STRIPERS
#define COMPILE_CHECK_STRIPERS
#define COMPILE_TUNE
#endif

// How to align the caches
#define cache_line_bytes 256

// type for describing the index of a bit in the sieve and general loops
#if defined(USE_64BIT_COUNTER)
    typedef int64_t counter_t;
    #define COUNTER_T_MAX_VALUE INT64_MAX
#else
    typedef int32_t counter_t;
    #define COUNTER_T_MAX_VALUE INT32_MAX
#endif

// type used to shift bits
#ifndef bitshift_t
    #define bitshift_t counter_t 
#endif

#include "helpers.h"
#include "types.h"
#include "verbose.h"
// #include "tools.h" // used for debugging