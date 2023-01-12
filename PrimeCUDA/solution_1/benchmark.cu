#include <stdint.h>
#include <map>
#include <chrono>
#include <cstdlib>

#include "CUDASieve/cudasieve.hpp"

using namespace std::chrono;

const uint64_t DEFAULT_SIEVE_SIZE = 1000000;
const int RESET_CYCLE_COUNT = 1000;

const std::map<uint64_t, const int> resultsDictionary =
{
    {          10UL, 4         },               // Historical data for validating our results - the number of primes
    {         100UL, 25        },               // to be found under some limit, such as 168 primes under 1000
    {        1000UL, 168       },
    {       10000UL, 1229      },
    {      100000UL, 9592      },
    {     1000000UL, 78498     },
    {    10000000UL, 664579    },
    {   100000000UL, 5761455   },
    {  1000000000UL, 50847534  },
    { 10000000000UL, 455052511 },
};

// Assumes any first argument is the desired sieve size. Defaults to DEFAULT_SIEVE_SIZE.
uint64_t determineSieveSize(int argc, char *argv[])
{
    if (argc < 2)
        return DEFAULT_SIEVE_SIZE;

    uint64_t sieveSize = strtoul(argv[1], nullptr, 0);

    if (sieveSize == 0) 
        return DEFAULT_SIEVE_SIZE;

    if (resultsDictionary.find(sieveSize) == resultsDictionary.end())
        fprintf(stderr, "WARNING: Results cannot be validated for selected sieve size of %zu!\n\n", sieveSize);
    
    return sieveSize;
}

void printResults(uint64_t sieveSize, size_t primeCount, double duration, uint64_t passes)
{
    auto expectedCount = resultsDictionary.find(sieveSize);
    auto countValidated = expectedCount != resultsDictionary.end() && expectedCount->second == primeCount;

    fprintf(stderr, "Passes: %zu, Time: %lf, Avg: %lf, Limit: %zu, Count: %zu, Validated: %d\n\n", 
            passes,
            duration,
            duration / passes,
            sieveSize,
            primeCount,
            countValidated);

    printf("rbergen_cuda;%zu;%f;1;algorithm=other,faithful=yes,bits=1\n", passes, duration);
}

int main(int argc, char *argv[])
{
    uint64_t sieveSize = determineSieveSize(argc, argv);
    uint64_t passes = 0;
    int cycleCount = 0;
    auto tStart = steady_clock::now();
    size_t primeCount;

    while (true)
    {
        // Implementation is faithful because CudaSieve::getDevicePrimes creates and destroys a sieve class instance
        uint64_t *primes = CudaSieve::getDevicePrimes(0, sieveSize, primeCount);
        passes++;
        cudaFree(primes);
        if (duration_cast<seconds>(steady_clock::now() - tStart).count() >= 5)
        {
            printResults(sieveSize, primeCount, duration_cast<microseconds>(steady_clock::now() - tStart).count() / 1000000.0, passes);
            break;
        }
        cycleCount++;
        // Reset the device every RESET_CYCLE_COUNT passes, as recommended in the CUDASieve README
        if (cycleCount == RESET_CYCLE_COUNT)
        {
            cudaDeviceReset();
            cycleCount = 0;
        }
    } 
}

