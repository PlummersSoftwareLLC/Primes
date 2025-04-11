// Structure to hold function information
// This is a helper function for development and checking benchmarking purposes
#ifndef BITSTORAGE_SETBITSTRUE_FUNCTIONLIST_H
#define BITSTORAGE_SETBITSTRUE_FUNCTIONLIST_H

// Define a function pointer type for setBitsTrue functions
typedef void (*setBitsTrueFunc)(void* restrict, const counter_t, const counter_t, const counter_t);
typedef struct {
    const char* name;               // Function name 
    setBitsTrueFunc func;           // Function pointer
    counter_t min_step;             // Minimum applicable step value
    counter_t max_step;             // Maximum applicable step value
    int enabled;                    // Whether this function is enabled in benchmarking
} SetBitsTrueMethod;


// Global array with all setBitsTrue functions
static SetBitsTrueMethod setBitsTrueMethods[] = {
    { "setBitsTrue                                 ", setBitsTrue                                 , 0, INT32_MAX, 1 },
    { "setBitsTrue_range                           ", setBitsTrue_range                           , 0, INT32_MAX, 1},
    { "setBitsTrue_smallstep_rotate_pair_uint64v8  ", setBitsTrue_smallstep_rotate_pair_uint64v8  , 0, 63, 1},
    { "setBitsTrue_smallstep_rotate_pair_uint64v4  ", setBitsTrue_smallstep_rotate_pair_uint64v4  , 0, 63, 1},
    { "setBitsTrue_smallstep_rotate_pair_uint32v16 ", setBitsTrue_smallstep_rotate_pair_uint32v16 , 0, 31, 1},
    { "setBitsTrue_smallstep_rotate_pair_uint16v16 ", setBitsTrue_smallstep_rotate_pair_uint16v16 , 0, 15, 1},
    { "setBitsTrue_largestep_vector_uint64v8       ", setBitsTrue_largestep_vector_uint64v8       , 65, 511, 1},
    { "setBitsTrue_largestep_vector_uint64v4       ", setBitsTrue_largestep_vector_uint64v4       , 65, 255, 1},
    { "setBitsTrue_largestep_vector_uint32v16      ", setBitsTrue_largestep_vector_uint32v16      , 33, 255, 0},
    { "setBitsTrue_largestep_repeat_uint64         ", setBitsTrue_largestep_repeat_uint64         , 0, INT32_MAX, 1},
    { "setBitsTrue_largestep_repeat_uint32         ", setBitsTrue_largestep_repeat_uint32         , 0, INT32_MAX, 1},
    { "setBitsTrue_largestep_repeat_uint8_unroll8  ", setBitsTrue_largestep_repeat_uint8_unroll8  , 0, INT32_MAX, 1},
    { "setBitsTrue_largestep_repeat_uint8          ", setBitsTrue_largestep_repeat_uint8          , 0, INT32_MAX, 1},
    { "setBitsTrue_largestep_norepeat_uint8_unroll8", setBitsTrue_largestep_norepeat_uint8_unroll8, 0, INT32_MAX, 1},
    { "setBitsTrue_largestep_norepeat_uint8        ", setBitsTrue_largestep_norepeat_uint8        , 0, INT32_MAX, 1},
};

#define methods (sizeof(setBitsTrueMethods) / sizeof(SetBitsTrueMethod))

#endif /* BITSTORAGE_SETBITSTRUE_FUNCTIONLIST_H */