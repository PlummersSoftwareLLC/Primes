#define DEFAULT_SIEVE_SIZE 1'000'000

// Maximum number of parallel threads used on the GPU. The actual number of threads used can be lower, 
//   if there aren't enough "chunks of work" to keep this number of threads busy.
#define MAX_THREADS 256

// Bits per sieve buffer word. Can be 32 or 64.
#define BITS_PER_WORD 32

// Highest prime number for which we'll use a rolling bit mask. This value cannot be higher than the
//   number of bits per word. Set to 0 to disable the use of a rolling bit mask. The relevant code won't
//   be compiled then, thus reducing performance side effects to zero. 
#define ROLLING_LIMIT 29

// If defined, the code will show debug output and run both unmarking methods once.
//#define DEBUG

//=======================================================================================================
//
// No user-modifiable defines below. :)
//

#include <cstdint>
#include <type_traits>

#if ROLLING_LIMIT > BITS_PER_WORD
    #error "ROLLING_LIMIT can't be greater than the number of bits per word."
#endif

#if BITS_PER_WORD == 32
    typedef unsigned int sieve_t;
    #define MAX_WORD_VALUE UINT32_MAX
    #define WORD_SHIFT 5
#elif BITS_PER_WORD == 64
    typedef unsigned long long sieve_t;
    #define MAX_WORD_VALUE UINT64_MAX
    #define WORD_SHIFT 6
#else
    #error "Unsupported value for BITS_PER_WORD defined; 32 and 64 are supported."
#endif

#define BYTES_PER_WORD (BITS_PER_WORD >> 3)
#define MAX_BIT_INDEX (BITS_PER_WORD - 1)
#define WORD_INDEX(index) (index >> WORD_SHIFT)
#define BIT_INDEX(index) (index & MAX_BIT_INDEX)
// This is actually BITS_PER_WORD * 2 - 1, but this is a "cheap" way to get there
#define SIEVE_BITS_MASK (BITS_PER_WORD + MAX_BIT_INDEX)
#define SIEVE_WORD_MASK ~uint64_t(SIEVE_BITS_MASK)

enum class Parallelization : char
{
    threads,
    blocks
};

// We have to define this ourselves, as we're not doing C++23 (yet)
template<class TEnum>
constexpr auto to_underlying(TEnum enumValue)  
{
   return static_cast<typename std::underlying_type<TEnum>::type>(enumValue);
}
