#include <cinttypes>
#include <cstdint>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <cmath>
#include <algorithm>
#include <chrono>
#include <map>
#include <cuda_runtime.h>

using namespace std::chrono;

#define DEFAULT_SIEVE_SIZE 1'000'000

// The actual number of threads used can be lower, if there aren't enough "chunks of work" to keep this
//   number of threads busy.
#define MAX_THREADS 256

// Highest prime number for which we'll use a rolling bit mask. This value cannot be higher than the
//   number of bits per word! Set to 0 to disable the use of a rolling bit mask. The relevant code won't
//   be compiled then, thus reducing performance side effects to zero. 
#define ROLLING_LIMIT 29

// If defined, the code will show debug output and run both unmarking methods once.
//#define DEBUG

// Modify the following 4 lines if you want to switch to another word type for the sieve buffer. Two sets
//   of values are supported:
//   - for 32 bit words: 
//     sieve_t = unsigned int, BITS_PER_WORD = UINT32_WIDTH, MAX_WORD_VALUE = UINT32_MAX, WORD_SHIFT = 5
//   - for 64 bit words: 
//     sieve_t = unsigned long long, BITS_PER_WORD = UINT64_WIDTH, MAX_WORD_VALUE = UINT64_MAX, WORD_SHIFT = 6
typedef unsigned int sieve_t;
#define BITS_PER_WORD UINT32_WIDTH
#define MAX_WORD_VALUE UINT32_MAX
#define WORD_SHIFT 5

// The following defines are derived from the previous 4, so it's not necessary to modify these
#define BYTES_PER_WORD (BITS_PER_WORD >> 3)
#define MAX_BIT_INDEX (BITS_PER_WORD - 1)
#define WORD_INDEX(index) (index >> WORD_SHIFT)
#define BIT_INDEX(index) (index & MAX_BIT_INDEX)
// This is actually BITS_PER_WORD * 2 - 1, but this is a "cheap" way to get there
#define SIEVE_WORD_MASK ~uint64_t(BITS_PER_WORD + MAX_BIT_INDEX)

#if ROLLING_LIMIT > BITS_PER_WORD
  #error "ROLLING_LIMIT can't be greater than the number of bits per word!"
#endif

__global__ void initialize_buffer(uint64_t blockSize, uint64_t wordCount, sieve_t *sieve)
{
    const uint64_t startIndex = uint64_t(blockIdx.x) * blockSize;
    const uint64_t endIndex = ullmin(startIndex + blockSize, wordCount);

    for (uint64_t index = startIndex; index < endIndex; index++)
        sieve[index] = MAX_WORD_VALUE;
}

__global__ void unmark_multiples_threads(uint32_t primeCount, uint32_t *primes, uint64_t halfSize, uint32_t sizeSqrt, sieve_t *sieve)
{
    // We unmark every "MAX_THREADS"th prime's multiples, starting with our thread index
    for (uint32_t primeIndex = threadIdx.x; primeIndex < primeCount; primeIndex += MAX_THREADS) 
    {
        const uint32_t prime = primes[primeIndex];
        const uint64_t primeSquared = uint64_t(prime) * prime;

        // Unmark multiples starting at just beyond the square root of the sieve size or the square of the prime, 
        //   whichever is larger.
        uint64_t firstUnmarked = primeSquared > sizeSqrt ? primeSquared : ((sizeSqrt / prime + 1) * prime);
        // We're marking off odd multiples only, so make sure we start with one of those!
        if (!(firstUnmarked & 1))
            firstUnmarked += prime;

        for (uint64_t index = firstUnmarked >> 1; index <= halfSize; index += prime) 
            // Clear the bit in the word that corresponds to the last part of the index 
            atomicAnd(&sieve[WORD_INDEX(index)], ~(sieve_t(1) << BIT_INDEX(index)));
    }
}

__global__ void unmark_multiples_blocks(uint32_t primeCount, uint32_t *primes, uint64_t halfSize, uint32_t sizeSqrt, uint32_t maxBlockIndex, uint64_t blockSize, sieve_t *sieve)
{
    // Calculate the start and end of the block we need to work on, at buffer word boundaries. 
    //   Note that the first variable is a number in sieve space...
    uint64_t blockStart = uint64_t(blockIdx.x) * blockSize + sizeSqrt;
    //   ...and the second is an index in the sieve buffer (representing odd numbers only)
    const uint64_t lastIndex = (blockIdx.x == maxBlockIndex) ? halfSize : (((blockStart + blockSize) & SIEVE_WORD_MASK) >> 1) - 1;

    // If this is not the first block, we actually start at the beginning of the first block word
    if (blockIdx.x != 0)
        blockStart &= SIEVE_WORD_MASK;

#ifdef DEBUG
    printf("  - block %d: blockStart = %" PRIu64 " (index %" PRIu64 "), lastIndex = %" PRIu64 ".\n", blockIdx.x, blockStart, (blockStart >> 1), lastIndex);
#endif

    for (uint32_t primeIndex = 0; primeIndex < primeCount; primeIndex++)
    {
        const uint32_t prime = primes[primeIndex];
        const uint64_t primeSquared = uint64_t(prime) * prime;

        // Unmark multiples starting at just beyond the start of our block or the square of the prime, 
        //   whichever is larger.
        uint64_t firstUnmarked = primeSquared >= blockStart ? primeSquared : ((blockStart / prime + 1) * prime);
        // We're marking off odd multiples only, so make sure we start with one of those!
        if (!(firstUnmarked & 1))
            firstUnmarked += prime;

    #if ROLLING_LIMIT > 0
        if (prime <= ROLLING_LIMIT)
        {
            uint64_t index = firstUnmarked >> 1;
            if (index > lastIndex)
                continue;

            uint64_t wordIndex = WORD_INDEX(index);
            uint32_t bitIndex = BIT_INDEX(index);                
            sieve_t bitMask = 0;

            do
            {
                if (bitIndex > MAX_BIT_INDEX) 
                {
                    // Clear the bits that are set in the mask
                    sieve[wordIndex++] &= ~bitMask;
                    bitIndex %= BITS_PER_WORD;
                    bitMask = sieve_t(1) << bitIndex;
                }
                else
                    bitMask |= sieve_t(1) << bitIndex;

                index += prime;
                bitIndex += prime;
            }
            while (index <= lastIndex);

            sieve[wordIndex] &= ~bitMask;
        }
        else
        {
    #endif

            for (uint64_t index = firstUnmarked >> 1; index <= lastIndex; index += prime) 
                // Clear the bit in the word that corresponds to the last part of the index 
                sieve[WORD_INDEX(index)] &= ~(sieve_t(1) << BIT_INDEX(index));

    #if ROLLING_LIMIT > 0
        }
    #endif

    }
}

enum class Parallelization : char
{
    threads,
    blocks
};

template<typename E>
constexpr auto to_integral(E e) -> typename std::underlying_type<E>::type 
{
   return static_cast<typename std::underlying_type<E>::type>(e);
}

class Sieve 
{
    const uint64_t sieve_size;
    const uint64_t half_size;
    const uint32_t size_sqrt;
    const uint64_t buffer_word_size;
    const uint64_t buffer_byte_size;
    sieve_t *device_sieve_buffer;
    sieve_t *host_sieve_buffer;

    void unmark_multiples(Parallelization type, uint32_t primeCount, uint32_t *primeList) 
    {
        // Copy the first (square root of sieve size) buffer bytes to the device
        cudaMemcpy(device_sieve_buffer, host_sieve_buffer, (size_sqrt >> 4) + 1, cudaMemcpyHostToDevice);
        // Allocate device buffer for the list of primes and copy the prime list to it
        uint32_t *devicePrimeList;
        cudaMalloc(&devicePrimeList, primeCount * sizeof(uint32_t));
        cudaMemcpy(devicePrimeList, primeList, primeCount << 2, cudaMemcpyHostToDevice);

        // Unmark multiples on the GPU and then release the prime list buffer
        switch(type)
        {
            case Parallelization::threads:
            {
                const uint32_t threadCount = min(MAX_THREADS, primeCount);
            #ifdef DEBUG
                printf("- starting thread multiple unmarking with %u threads.\n", threadCount);
            #endif
                unmark_multiples_threads<<<1, threadCount>>>(primeCount, devicePrimeList, half_size, size_sqrt, device_sieve_buffer);
            }
            break;

            case Parallelization::blocks:
            {
                const uint64_t sieveSpace = sieve_size - size_sqrt;
                uint64_t wordCount = sieveSpace << (WORD_SHIFT + 1);
                if (sieveSpace & SIEVE_WORD_MASK)
                    wordCount++;
                const uint32_t blockCount = (uint32_t)min(uint64_t(MAX_THREADS), wordCount);
                uint64_t blockSize = sieveSpace / blockCount;
                if (sieveSpace % blockCount)
                    blockSize++;

            #ifdef DEBUG
                printf("- starting block multiple unmarking with blockCount %u and blockSize %zu.\n", blockCount, blockSize);
            #endif
                unmark_multiples_blocks<<<blockCount, 1>>>(primeCount, devicePrimeList, half_size, size_sqrt, blockCount - 1, blockSize, device_sieve_buffer);
            }
            break;

            default:
                // This is some variation we don't know, so we warn and do nothing.
                fprintf(stderr, "WARNING: Parallelization type %d unknown, multiple unmarking skipped!\n\n", to_integral(type));
            break;
        }
        
        cudaFree(devicePrimeList);

        // Copy the sieve buffer from the device to the host 
        cudaMemcpy(host_sieve_buffer, device_sieve_buffer, buffer_byte_size, cudaMemcpyDeviceToHost);
    #ifdef DEBUG
        printf("- device to host copy of sieve buffer complete.\n");
    #endif
    }

    public:

    Sieve(unsigned long size) :
        sieve_size(size),
        half_size(size >> 1),
        size_sqrt((uint32_t)sqrt(size) + 1),
        buffer_word_size((half_size >> WORD_SHIFT) + 1),
        buffer_byte_size(buffer_word_size * BYTES_PER_WORD)
    {
    #ifdef DEBUG
        printf("- constructing sieve with buffer_word_size %zu and buffer_byte_size %zu.\n", buffer_word_size, buffer_byte_size);
    #endif

        // Allocate and initialize device sieve buffer
        cudaMalloc(&device_sieve_buffer, buffer_byte_size);

        const uint32_t blockCount = (uint32_t)min(uint64_t(MAX_THREADS), buffer_word_size);
        uint64_t blockSize = buffer_word_size / blockCount;
        if (buffer_word_size % blockCount)
            blockSize++;

    #ifdef DEBUG
        printf("- initializing device buffer with blockCount %u and blockSize %zu.\n", blockCount, blockSize);
    #endif
        initialize_buffer<<<blockCount, 1>>>(blockSize, buffer_word_size, device_sieve_buffer);

        // Allocate host sieve buffer and initialize the bytes up to the square root of the sieve size
        host_sieve_buffer = (sieve_t *)malloc(buffer_byte_size);
        memset(host_sieve_buffer, 255, (size_sqrt >> 4) + 1);
        cudaDeviceSynchronize();
    #ifdef DEBUG
        printf("- post buffer initialization device sync complete.\n");
    #endif
    }

    ~Sieve() 
    {
        cudaFree(device_sieve_buffer);
        free(host_sieve_buffer);
    }

    sieve_t *run(Parallelization type = Parallelization::threads)
    {
        // Calculate the size of the array we need to reserve for the primes we find up to and including the square root of
        //   the sieve size. x / (ln(x) - 1) is a good approximation, but often lower than the actual number, which would
        //   cause out-of-bound indexing. This is why we use x / (ln(x) - 1.2) to "responsibly over-allocate".
        const uint32_t primeListSize = uint32_t(double(size_sqrt) / (log(size_sqrt) - 1.2));

        uint32_t primeList[primeListSize];
        uint32_t primeCount = 0;

        // We clear multiples up to and including size_sqrt
        const uint32_t lastMultipleIndex = size_sqrt >> 1;

        for (uint32_t factor = 3; factor <= size_sqrt; factor += 2)
        {
            uint64_t index = factor >> 1;

            if (host_sieve_buffer[WORD_INDEX(index)] & (sieve_t(1) << BIT_INDEX(index))) 
            {
                primeList[primeCount++] = factor;

                for (index = (factor * factor) >> 1; index <= lastMultipleIndex; index += factor)
                    host_sieve_buffer[WORD_INDEX(index)] &= ~(sieve_t(1) << BIT_INDEX(index));
            }
        }

        unmark_multiples(type, primeCount, primeList);

        // Required to be truly compliant with Primes project rules
        return host_sieve_buffer;
    }

    uint64_t count_primes() 
    {
        uint64_t primeCount = 0;
        const uint64_t lastWord = WORD_INDEX(half_size);
        sieve_t word;

        for (uint64_t index = 0; index < lastWord; index++)
        {
            word = host_sieve_buffer[index];
            while (word) 
            {
                if (word & 1)
                    primeCount++;

                word >>= 1;
            }
        }

        word = host_sieve_buffer[lastWord];
        const uint32_t lastBit = BIT_INDEX(half_size);
        for (uint32_t index = 0; word && index <= lastBit; index++) 
        {
            if (word & 1)
                primeCount++;
            
            word >>= 1;
        }

        return primeCount;
    }
};

const std::map<uint64_t, const int> resultsDictionary =
{
    {             10UL, 4         }, // Historical data for validating our results - the number of primes
    {            100UL, 25        }, //   to be found under some limit, such as 168 primes under 1000
    {          1'000UL, 168       },
    {         10'000UL, 1229      },
    {        100'000UL, 9592      },
    {      1'000'000UL, 78498     },
    {     10'000'000UL, 664579    },
    {    100'000'000UL, 5761455   },
    {  1'000'000'000UL, 50847534  },
    { 10'000'000'000UL, 455052511 },
};

const std::map<Parallelization, const char *> parallelizationDictionary = 
{
    { Parallelization::threads, "threads" },
    { Parallelization::blocks,  "blocks"  }
};

// Assumes any first argument is the desired sieve size. Defaults to DEFAULT_SIEVE_SIZE.
uint64_t determineSieveSize(int argc, char *argv[])
{
    if (argc < 2)
        return DEFAULT_SIEVE_SIZE;

    const uint64_t sieveSize = strtoul(argv[1], nullptr, 0);

    if (sieveSize == 0) 
        return DEFAULT_SIEVE_SIZE;

    if (resultsDictionary.find(sieveSize) == resultsDictionary.end())
        fprintf(stderr, "WARNING: Results cannot be validated for selected sieve size of %zu!\n\n", sieveSize);
    
    return sieveSize;
}

void printResults(Parallelization type, uint64_t sieveSize, uint64_t primeCount, double duration, uint64_t passes)
{
    const auto expectedCount = resultsDictionary.find(sieveSize);
    const auto countValidated = expectedCount != resultsDictionary.end() && expectedCount->second == primeCount;
    const auto parallelizationEntry = parallelizationDictionary.find(type);
    const char *parallelizationLabel = parallelizationEntry != parallelizationDictionary.end() ? parallelizationEntry->second : "unknown";

    fprintf(stderr, "Passes: %zu, Time: %lf, Avg: %lf, Word size: %d, Max GPU threads: %d, Type: %s, Limit: %zu, Count: %zu, Validated: %d\n", 
            passes,
            duration,
            duration / passes,
            BITS_PER_WORD,
            MAX_THREADS,
            parallelizationLabel,
            sieveSize,
            primeCount,
            countValidated);

    printf("rbergen_faithful_cuda_%s;%zu;%f;1;algorithm=base,faithful=yes,bits=1\n\n", parallelizationLabel, passes, duration);
}

int main(int argc, char *argv[])
{
    const uint64_t sieveSize = determineSieveSize(argc, argv);

    Parallelization types[] = { Parallelization::blocks, Parallelization::threads };

    for (auto &type : types)
    {
        uint64_t passes = 0;

        Sieve *sieve = nullptr;

        const auto startTime = steady_clock::now();
        duration<double, std::micro> runTime;

    #ifndef DEBUG
        do
        {
    #endif

            delete sieve;

            sieve = new Sieve(sieveSize);
            sieve->run(type);

            passes++;

            runTime = steady_clock::now() - startTime;

    #ifndef DEBUG
        }
        while (duration_cast<seconds>(runTime).count() < 5);
    #else
        printf("\n");
    #endif

        const size_t primeCount = sieve->count_primes();
        
        delete sieve;

        printResults(type, sieveSize, primeCount, duration_cast<microseconds>(runTime).count() / 1000000.0, passes); 
    }
}
