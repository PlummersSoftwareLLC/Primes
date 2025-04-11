// This file contains the continuePattern function that is used to extend (copy) a pattern in a bitstorage.
// The function is optimized for different sizes and offsets of the pattern and uses different algorithms for this.
#undef bitbucket_t
#define bitbucket_t uint32_t
#undef variant_base_type_t
#define variant_base_type_t bitbucket_t

#include "bitstorage_continuePattern_smallsize.h"
#include "bitstorage_continuePattern_aligned.h"
#include "bitstorage_continuePattern_shiftleft.h"
#include "bitstorage_continuePattern_shiftright.h"

// continue a pattern that start at <source_start> with a size of <size>.
// repeat this pattern up to <destination_stop>.
// for small sizes, this is done on a word level
// for larger sizes, look at the offset / start bit and apply the appropriate algorithm.
// note that these algorithms are general for bitstorage and have no specialized assumptions for the sieve application
static inline void __attribute__((always_inline, nonnull)) 
continuePattern(void* restrict bitstorage, const counter_t source_start, const counter_t size, const counter_t destination_stop)
{
    startAnalysis7(time_continuePattern, "Continue pattern size %ju in %ju bit range (%ju-%ju) using continuePattern (%ju copies)\n", (uintmax_t)size, (uintmax_t)destination_stop-(uintmax_t)source_start,(uintmax_t)source_start,(uintmax_t)destination_stop, (uintmax_t)(((uintmax_t)destination_stop-(uintmax_t)source_start)/(uintmax_t)size));

    if (size < bitcount_type(bitbucket_t)) {
        continuePattern_smallSize(bitstorage, source_start, size, destination_stop);
        timer_laptime(time_continuePattern); verbose7( printf("\n"); )
        return;
    }

    const bitshift_t copy_bit   = bitindex_calc_type(source_start + size, bitbucket_t);
    const bitshift_t source_bit = bitindex_calc_type(source_start, bitbucket_t);

    if      (source_bit > copy_bit) continuePattern_shiftleft (bitstorage, source_start, size, destination_stop);
    else if (source_bit < copy_bit) continuePattern_shiftright(bitstorage, source_start, size, destination_stop);
    else                            continuePattern_aligned   (bitstorage, source_start, size, destination_stop);

    // timer_laptime(time_continuePattern); verbose7( printf("\n"); )
    endAnalysis7(time_continuePattern,"\n");
}
