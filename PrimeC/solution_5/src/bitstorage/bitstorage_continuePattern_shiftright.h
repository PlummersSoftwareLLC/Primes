
static inline void  __attribute__((always_inline, hot, nonnull)) 
continuePattern_shiftright(void* restrict bitstorage, const counter_t source_start, const counter_t size_bits, const counter_t destination_stop)
{
    startAnalysis7(time_continuePattern_shiftright, "Extending sieve size %ju in %ju bit range (%ju-%ju) using continuePattern_shiftright (%ju copies)", (uintmax_t)size_bits, (uintmax_t)destination_stop-(uintmax_t)source_start,(uintmax_t)source_start,(uintmax_t)destination_stop, (uintmax_t)(((uintmax_t)destination_stop-(uintmax_t)source_start)/(uintmax_t)size_bits));

    bitbucket_t* restrict bitstorage_sized = __builtin_assume_aligned(bitstorage, cache_line_bytes);

    const counter_t destination_stop_word = index_type(destination_stop, bitbucket_t);
    const counter_t copy_start = source_start + size_bits;
    register const bitshift_t shift = bitindex_calc_type(copy_start, bitbucket_t) - bitindex_calc_type(source_start, bitbucket_t);
    register const bitshift_t shift_flipped = bitcount_type(bitbucket_t)-shift;
    register counter_t source_word = index_type(source_start, bitbucket_t);
    register counter_t copy_word = index_type(copy_start, bitbucket_t);

    // copy the pattern in one go if the source and copy are close together
    if unlikely(copy_word >= destination_stop_word) { 
        bitstorage_sized[copy_word] |= ((bitstorage_sized[source_word] << shift)  // or the start in to not lose data
                                | (bitstorage_sized[copy_word] >> shift_flipped))
                                & keepmask_type(copy_start, bitbucket_t) & chopmask_type(destination_stop, bitbucket_t);
                                endAnalysis7(time_continuePattern_shiftright,"\n");
        return; // rapid exit for one word variant
    }

    // handle the first word separately to avoid overwriting the source
    bitstorage_sized[copy_word] |= ((bitstorage_sized[source_word] << shift)  // or the start in to not lose data
                                | (bitstorage_sized[copy_word] >> shift_flipped))
                                & keepmask_type(copy_start, bitbucket_t);
    
    copy_word++;

    verbose7( printf("...startword - %ju - copystartword %ju - endword %ju..",(uintmax_t)source_word, (uintmax_t)copy_word, (uintmax_t)destination_stop_word); )

    // search for the first word that is aligned at bytelevel
    counter_t copy_size_bytes = size_bits; // at bytelevel, the size is the same
    counter_t copy_start_word = index_type(index_next_type(copy_start + (copy_size_bytes << SHIFT_BYTE), bitbucket_t), bitbucket_t); 
    if (copy_start_word > destination_stop_word) copy_start_word = destination_stop_word;

    // copy with shift - needed when not aligned at bytelevel
    verbose7(  printf("\n...speed copy until word %ju..", (uintmax_t)copy_start_word); )

    // copy the pattern until we reach bytelevel alignment
    // #pragma GCC ivdep // This pragma caused problems in the past with <64 bit 
    for (; copy_word <= copy_start_word; copy_word++, source_word++ ) 
    bitstorage_sized[copy_word] = (bitstorage_sized[source_word] >> shift_flipped) | (bitstorage_sized[source_word+1] << shift);

    // end if we reached the destination already
    if (copy_word >= destination_stop_word) {
        timer_laptime(time_continuePattern_shiftright); verbose7( printf("\n"); )
        return;
    }

    uint8_t* source_byte           = (uint8_t*)&bitstorage_sized[copy_start_word];
    uint8_t* copy_byte             = (uint8_t*)&bitstorage_sized[copy_start_word];
    uint8_t* destination_stop_byte = (uint8_t*)&bitstorage_sized[destination_stop_word+1];
    source_byte -= copy_size_bytes;

    size_t memcpy_size = destination_stop_byte - copy_byte;
    local_memcpy(copy_byte, source_byte, memcpy_size);

    endAnalysis7(time_continuePattern_shiftright,"\n");
}