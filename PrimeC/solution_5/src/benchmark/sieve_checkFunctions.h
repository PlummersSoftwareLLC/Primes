#include "../bitstorage/bitstorage_setBitsTrueFunctionList.h"

static uint8_t checkSetBitsTrueMethod_stripe(const SetBitsTrueMethod* method, const counter_t range_start, const counter_t step, const counter_t range_stop)
{
    // create sieve
    struct sieve_t* sieve = sieve_create((range_stop+1024)*2);
    void* bitstorage = sieve->bitstorage;
    sieve_clear(sieve);
    setBitsTrue_range(bitstorage, range_start, step, range_stop);
    counter_t target_count = countBitsTrue(bitstorage, range_start, range_stop+1024);
    sieve_delete(sieve);

    sieve = sieve_create(range_stop*2+1024); // reserve extra to check set bits after range stop
    bitstorage = sieve->bitstorage;
    sieve_clear(sieve);

    method->func(bitstorage, range_start, step, range_stop);
    counter_t actual_count_inrange    = countBitsTrue(bitstorage, range_start, range_stop); // add 1024 to check the bits after the range
    counter_t actual_count_atrange    = checkBitTrue(bitstorage, range_stop) ? 1 : 0;
    counter_t actual_count_afterrange = countBitsTrue(bitstorage, range_stop+1, range_stop+1024);

    uint8_t correct_inrange    = (actual_count_inrange == target_count);
    uint8_t correct_atrange    = (actual_count_atrange == 0);
    uint8_t correct_afterrange = (actual_count_afterrange == 0);

    if (!(actual_count_inrange == target_count )) {
        printf("\nMethod %s for stripe with step %ju in range %ju-%ju failed with %ju bits set, expected %ju \n", method->name, (uintmax_t) step, (uintmax_t) range_start, (uintmax_t) range_stop, (uintmax_t)actual_count_inrange, (uintmax_t)target_count);
        // set the correctly set bits false

        counter_t max_messages = 0;
        for (counter_t index = range_start; index <= range_stop; index += step) {
            if (checkBitTrue(bitstorage, index)) { setBitFalse(bitstorage, index); }
            else {
                if (max_messages==0) printf("Not set        : "); 
                if (max_messages++ < 10) { printf(" %ju ", (uintmax_t)index); } 
            }
        }
        if (max_messages > 0) printf("\n");

        max_messages = 0;
        // what is left as set, is incorrectly set
        for (counter_t index = range_start; index <= range_stop; index++) {
            if (checkBitTrue(bitstorage, index)) { 
                if (max_messages==0) printf("Incorrectly set: "); 
                if (max_messages++ < 10) printf(" %ju ", (uintmax_t)index); 
            } 
        }
        if (max_messages > 0) printf("\n");
    }
    sieve_delete(sieve);

    return correct_inrange | (correct_atrange << 1) | (correct_afterrange << 2);
}  

static inline uint8_t checkSetBitsTrueMethod(const SetBitsTrueMethod* method, const counter_t range_start, const counter_t range_stop) 
{
    // build a base sieve for getting the right primes
    struct sieve_t* sieve_base = sieve_create(range_stop*2);
    void* bitstorage_base = sieve_base->bitstorage;
    counter_t prime = 1, prime_max = prime_stop(range_stop);

    while (prime < prime_max) {
        register const counter_t step  = prime * 2 + 1;
        register counter_t start = compute_start(prime, range_start);
        setBitsTrue_range(bitstorage_base, start, step, range_stop);
        prime = searchBitFalse(bitstorage_base, prime);
    }

    // reset prime to 1; loop through all primes and when min_step < step < max_step, check the method
    uint8_t allvalid = 7;
    prime = 1;
    while (prime < prime_max) {
        register const counter_t step  = prime * 2 + 1;
        if (step >= method->min_step && step <= method->max_step) {
            // printf("Checking method %s for step %ju\n", method->name, (uintmax_t)step);
            allvalid &= checkSetBitsTrueMethod_stripe(method, compute_start(prime, range_start), step, range_stop);
        }
        prime = searchBitFalse(bitstorage_base, prime);
    }
    sieve_delete (sieve_base);
    return allvalid;
}

static inline uint8_t checkSetBitsTrueMethods(const SetBitsTrueMethod* SetBitsTrueMethods, const counter_t range_start, const counter_t range_stop) {
    int allvalid = 1;

    printf( COLOR_BLUE "..Checking methods in %ju bit range (%ju-%ju) ...\n" COLOR_RESET,(uintmax_t)range_stop - range_start, (uintmax_t)range_start, (uintmax_t)range_stop);

    for(int m=0; m<methods; m++) {
        SetBitsTrueMethod setBitsTrueMethod = SetBitsTrueMethods[m];

        printf("%3d %-50s ", m, setBitsTrueMethod.name);
        uint8_t valid = checkSetBitsTrueMethod(&setBitsTrueMethod, range_start, range_stop);
        printf("In range: "   ); if (valid&1) { printf( COLOR_GREEN "✓ valid    " COLOR_RESET " "); } else { printf( COLOR_RED "✗ NOT VALID" COLOR_RESET " "); }
        printf("At range: "   ); if (valid&2) { printf( COLOR_GREEN "✓ valid    " COLOR_RESET " "); } else { printf( COLOR_RED "✗ NOT VALID" COLOR_RESET " "); }
        printf("After range: "); if (valid&4) { printf( COLOR_GREEN "✓ valid    " COLOR_RESET " "); } else { printf( COLOR_RED "✗ NOT VALID" COLOR_RESET " "); }
        printf("\n"); 
        if (valid != 7) { allvalid = 0; }
    }
    printf("\n");
    return allvalid;
}

static inline uint8_t checkSetBitsTrueMethodsBlocks(const SetBitsTrueMethod* SetBitsTrueMethods, const counter_t range_start, const counter_t range_stop) 
{
    uint8_t allvalid = 7;

    printf( COLOR_BLUE "..Checking methods in %ju bit range (%ju-%ju) ...\n" COLOR_RESET,(uintmax_t)range_stop - range_start, (uintmax_t)range_start, (uintmax_t)range_stop);

    for(int m=0; m<methods; m++) {
        SetBitsTrueMethod setBitsTrueMethod = SetBitsTrueMethods[m];
        printf("%3d %-50s ", m, setBitsTrueMethod.name);
        uint8_t methodvalid = 7;
        for (counter_t blocksize_bits=1024; blocksize_bits<=32*1024*8; blocksize_bits *= 2) {
            for (counter_t block_start = blocksize_bits, block_stop = 2*blocksize_bits-1; block_start < range_stop; block_start += blocksize_bits, block_stop += blocksize_bits) {
                uint8_t valid = checkSetBitsTrueMethod(&setBitsTrueMethod, block_start, min(block_stop, range_stop));
                // if (valid != 7) { printf("Block %ju-%ju: \033[31m✗ NOT VALID" COLOR_RESET " ", (uintmax_t)block_start, (uintmax_t)block_stop); } else { printf("Block %ju-%ju: \033[32m✓ valid" COLOR_RESET " ", (uintmax_t)block_start, (uintmax_t)block_stop); }
                methodvalid &= valid;
                allvalid &= valid;
            } 
        }
        uint8_t valid = methodvalid;
        printf("In range: "   ); if (valid&1) { printf( COLOR_GREEN "✓ valid    " COLOR_RESET " "); } else { printf( COLOR_RED "✗ NOT VALID" COLOR_RESET " "); }
        printf("At range: "   ); if (valid&2) { printf( COLOR_GREEN "✓ valid    " COLOR_RESET " "); } else { printf( COLOR_RED "✗ NOT VALID" COLOR_RESET " "); }
        printf("After range: "); if (valid&4) { printf( COLOR_GREEN "✓ valid    " COLOR_RESET " "); } else { printf( COLOR_RED "✗ NOT VALID" COLOR_RESET " "); }
        printf("\n");
    }
    
    return allvalid;
}
