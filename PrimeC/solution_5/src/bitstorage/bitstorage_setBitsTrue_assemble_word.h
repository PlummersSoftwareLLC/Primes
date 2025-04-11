// in the first iteration, read this file for each preset
// the guard will prevent an infinite loop of includes
#ifndef BITSTORAGE_ASSEMBLE_WORD_GUARD
    #define BITSTORAGE_ASSEMBLE_WORD_GUARD

    #undef unrolls
    #undef variant

    #define unrolls 4
    #include "../bitstorage/bitstorage_setBitsTrue_setBit.h"

    #define unrolls 4
    #define variant uint8
    #include "bitstorage_setBitsTrue_assemble_word.h" 
    // #define variant uint16
    // #include "bitstorage_setBitsTrue_assemble_word.h" 
    #define variant uint32
    #include "bitstorage_setBitsTrue_assemble_word.h" 
    #define variant uint64
    #include "bitstorage_setBitsTrue_assemble_word.h" 
    #undef unrolls

    #define unrolls 8
    #define variant uint8
    #include "bitstorage_setBitsTrue_assemble_word.h" 
    // #define variant uint16
    // #include "bitstorage_setBitsTrue_assemble_word.h" 
    // #define variant uint32
    // #include "bitstorage_setBitsTrue_assemble_word.h" 
    // #define variant uint64
    // #include "bitstorage_setBitsTrue_assemble_word.h" 

#else
    #define KEEP_VARIANT
    #include "../bitstorage/bitstorage_setBitsTrue_applyMask.h"
    #include "../bitstorage/bitstorage_setBitsTrue_setBit.h"
    #include "../bitstorage/bitstorage_setBitsTrue_largestep_word.h"
    #undef KEEP_VARIANT
    #include "../generic/cleansuffix.h"
#endif