// The setsuffix.h file is used to define a suffix for the bitbucket_t type.
// It also makes a "suffix" that can be used to create function variants
// The cleansuffx.h file is used to unset 

// The bitbucket_t type can be provided by setting "variant" or by providing a "preset"
// The possibilities are listed in varianttypes.h

#include "../generic/varianttypes.h"

#ifdef variant
    #define bitbucket_t NAME(variant, _t)
    #define variantsuffix NAME(_,variant)
#elif !defined bitbucket_t
        #define bitbucket_t uint8_t
#endif

#if defined variantsuffix
    #if defined unrolls && unrolls != 4
        #define suffix NAME(variantsuffix, NAME(_unroll,unrolls))
    #else
        #define suffix variantsuffix
    #endif
#else    
    #define suffix
#endif
