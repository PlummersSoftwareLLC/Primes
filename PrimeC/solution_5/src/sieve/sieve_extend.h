// returns prime that could not be handled:
// start is too large
// range is too big
// block stop should not exceed sieve size for faster handling
static inline counter_t __attribute__((always_inline, nonnull, aligned(cache_line_bytes))) 
extendSieveBlock0(void* restrict bitstorage, const counter_t block_stop) 
{
    startAnalysis5(time_sieve_block_extend, "\nExtending sieve block 0 to range %ju - %ju\n",(uintmax_t)0,(uintmax_t)block_stop)

    ((uint64_t*)bitstorage)[0] = (uint64_t)0ULL; // only the first word has to be cleared; the rest is populated by the extension procedure

    counter_t prime_start            = 1;
    counter_t prime                  = 1;
    counter_t step                   = prime * 2 + 1;
    counter_t start                  = prime * (step + 1);
    counter_t range_stop             = step * 2;  // range is x2 so the second block cointains all multiples of primes
    counter_t patternsize_bits       = 3;

    setBitsTrue_range(bitstorage, start, step, range_stop);

    for (;range_stop < block_stop;) {
        prime = searchBitFalse(bitstorage, prime);

        step = prime * 2 + 1;
        start = prime * (step + 1);
        if unlikely(start > block_stop) break;

        range_stop = patternsize_bits * step * 2;  // range is x2 so the second block cointains all multiples of primes
        if unlikely(range_stop > block_stop) break;

        // continue the found pattern to the entire sieve
        continuePattern(bitstorage, patternsize_bits, patternsize_bits, range_stop);
        patternsize_bits *= step;

        setBitsTrue(bitstorage, start, step, range_stop);
    } 

    // continue the found pattern to the entire sieve
    continuePattern(bitstorage, patternsize_bits, patternsize_bits, block_stop);

    endAnalysis5(time_sieve_block_extend, "Copy bitpattern from %ju - %ju to range %ju - %ju:\n", (uintmax_t)patternsize_bits, (uintmax_t)2*patternsize_bits-1, (uintmax_t)2*patternsize_bits, (uintmax_t)block_stop);
    return prime;
}

struct block {
    counter_t pattern_size; // size of pattern applied 
    counter_t pattern_start; // start of pattern
    counter_t prime_next; // next prime to be striped
};

static inline counter_t 
extendSieveBlock(void* restrict bitstorage, const counter_t block_start, const counter_t block_stop) 
{
    startAnalysis5(time_sieve_block_extend, "\nExtending sieve block to range %ju - %ju with extendSieveBlock\n",(uintmax_t)block_start,(uintmax_t)block_stop)

    register counter_t prime         = 0;
    counter_t patternsize_bits       = 1;
    counter_t pattern_start          = 0;
    counter_t range_stop             = block_start;
    struct block block = { .prime_next = 0, .pattern_start = 0, .pattern_size = 0 };

    // only the first word has to be cleared; the rest is populated by the extension procedure
    ((uint16_t*)bitstorage)[index_type(block_start, uint16_t)  ] = (uint16_t)0ULL; 
    ((uint16_t*)bitstorage)[index_type(block_start, uint16_t)+1] = (uint16_t)0ULL; 
    verbose6( printf("Cleared bitstorage %ju - %ju\n", (uintmax_t)block_start, (uintmax_t)block_stop); )
    
    for (;range_stop < block_stop;) {
        prime = searchBitFalse(bitstorage, prime);
        block.prime_next = prime; // remember here so when we break or return, we don't have to search again

        counter_t start = (prime * prime * 2) + (prime * 2);
        if unlikely(start > block_stop) break;

        const counter_t step = prime * 2 + 1;
        if (block_start > prime) start = (block_start + prime) + prime - ((block_start + prime) % step);

        range_stop = block_start + patternsize_bits * step * 2;  // range is x2 so the second block cointains all multiples of primes
        block.pattern_size = patternsize_bits;
        block.pattern_start = pattern_start;
        if unlikely(range_stop > block_stop) break;

        if likely(patternsize_bits>1) {
            pattern_start = block_start | patternsize_bits;
            continuePattern(bitstorage, pattern_start, patternsize_bits, range_stop);
        }
        patternsize_bits *= step;

        setBitsTrue_range(bitstorage, start, step, range_stop);
    } 

    // continue the found pattern to the entire block
    continuePattern(bitstorage, block_start, block.pattern_size, block_stop);

    endAnalysis5(time_sieve_block_extend, "Copy bitpattern from %ju - %ju to range %ju - %ju:\n", (uintmax_t)block.pattern_size, (uintmax_t)2*block.pattern_size-1, (uintmax_t)2*block.pattern_size, (uintmax_t)block_stop);
    return block.prime_next;
}

static inline counter_t __attribute__((always_inline, nonnull)) 
extendSieveBlockByBlock(void* restrict bitstorage, const counter_t sieve_bits, const counter_t blocksize_bits, const counter_t prime_max)
{
    // size the first block, optimizing for large following blocks and aligning to cache line
    counter_t block0_stop = ((sieve_bits % blocksize_bits) + cache_line_bytes*8) & ~(cache_line_bytes*8-1); 

    if (blocksize_bits >= sieve_bits) {
        block0_stop = sieve_bits;
    }

    // first block requires fewer operations; it might be the whole sieve...
    counter_t prime_start = extendSieveBlock0(bitstorage, min(block0_stop, sieve_bits));
    counter_t prime_next = stripeSieveBlock0(bitstorage, min(block0_stop, sieve_bits), prime_start, prime_max);

    // process the rest of the sieve in blocks of blocksize_bits
    for (counter_t block_start = block0_stop, block_stop = block_start + blocksize_bits; block_start < sieve_bits; block_start += blocksize_bits, block_stop += blocksize_bits) {
        // stripe a block, stopping at the end of the sieve and only for primes that have multiples are in the block
        prime_start = extendSieveBlock(bitstorage, block_start, min(block_stop, sieve_bits));
        stripeSieveBlock(bitstorage, block_start, min(block_stop, sieve_bits), prime_start, min(prime_stop(block_stop),prime_max));
    }
    return prime_next; 
}