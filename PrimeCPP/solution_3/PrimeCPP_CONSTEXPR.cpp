// ---------------------------------------------------------------------------
// PrimeCPP_CONSTEXPR.cpp : Taking advantage of compiler optimizing constants
// ---------------------------------------------------------------------------

#include <chrono>
#include <cstdio>
#include <cstring>
#include <ctime>
#include <thread>
#include <vector>

#include "Sieve.h"

using namespace std;
using namespace std::chrono;

struct RESULTS
{
    unsigned long long upTo;
    int count;
};

constexpr RESULTS resultsDictionary[] = {
    {10LL, 4},   // Historical data for validating our results - the number of primes
    {100LL, 25}, // to be found under some limit, such as 168 primes under 1000
    {1000LL, 168},
    {10000LL, 1229},
    {100000LL, 9592},
    {1000000LL, 78498},
    {10000000LL, 664579},
};

constexpr int find(const unsigned long long val)
{
    for (const auto d : resultsDictionary)
    {
        if (d.upTo == val)
            return d.count;
    }
    return -1;
}

constexpr int countPrimes(const Sieve &sieve)
{
    return sieve.count();
}

constexpr bool validateResults(const Sieve &sieve)
{
    const auto sieveSize = sieve.size();
    const auto result = find(sieveSize);
    if (-1 == result)
        return false;
    return result == countPrimes(sieve);
}

void printResults(const Sieve &sieve, bool showResults, double duration, int passes, int threads)
{
    const auto sieveSize = sieve.size();

    if (showResults)
        printf("2, ");

    int count = (sieveSize >= 2); // Starting count (2 is prime)
    for (int num = 3; num <= sieveSize; num += 2)
    {
        if (sieve.contains(num))
        {
            if (showResults)
                printf("%d, ", num);
            count++;
        }
    }

    if (showResults)
        printf("\n");

    printf("Passes: %d, Time: %lf, Avg: %lf, Limit: %lu, Count1: %d, Count2: %d, Valid: %d\n",
           passes,
           duration,
           duration / passes,
           sieveSize,
           count,
           countPrimes(sieve),
           validateResults(sieve));

    printf("\n");
    printf("flo80_pol_constexpr;%d;%f;%d;algorithm=base,faithful=no,bits=1\n", passes, duration, threads);
}

// Consteval in order to force clang to always optimize. Gcc consistently does
// so with constexpr and inside the body.
consteval auto execute() {
    Sieve result;
    result.runSieve();
    return result;
}

void run(const Sieve &checkSieve, int numberOfThreads = 1, int runtimeSeconds = 5)
{

    unsigned int passes = 0;

    printf("Computing primes to %lu on %d thread%s for %d second%s.\n", checkSieve.size(),
           numberOfThreads,
           numberOfThreads == 1 ? "" : "s",
           runtimeSeconds,
           runtimeSeconds == 1 ? "" : "s");

    const auto tStart = steady_clock::now();
    if (numberOfThreads == 1) {
        for (;duration_cast<seconds>(steady_clock::now() - tStart).count() < runtimeSeconds; ++passes)
        {
            auto res = execute();
        }
    } else {
        std::thread threads[numberOfThreads];
        uint64_t l_passes[numberOfThreads];
        for (unsigned int i = 0; i < numberOfThreads; i++)
            threads[i] = std::thread([i, &l_passes, &tStart]()
            {
                for (l_passes[i] = 0; duration_cast<seconds>(steady_clock::now() - tStart).count() < 5; ++l_passes[i])
                {
                    auto res = execute();
                }
            });
        for (auto i = 0; i < numberOfThreads; i++) {
            threads[i].join();
            passes += l_passes[i];
        }
    }

    const auto tEnd = steady_clock::now();
    printResults(checkSieve, false, duration_cast<microseconds>(tEnd - tStart).count() / 1000000.0, passes, numberOfThreads);
}

int main(int argc, char **argv)
{
    Sieve checkSieve = execute();
    const auto result = validateResults(checkSieve) ? checkSieve.count() : 0;

    const auto numberOfThreads = thread::hardware_concurrency();
    thread::hardware_concurrency();
    run(checkSieve, numberOfThreads, runtimeSeconds);
    run(checkSieve, 1, runtimeSeconds);

    return result;
}
