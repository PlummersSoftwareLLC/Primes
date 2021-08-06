// ---------------------------------------------------------------------------
// PrimeCPP_CONSTEXPR.cpp : Taking advantage of compiler optimizing constants
// ---------------------------------------------------------------------------

#include <chrono>
#include <cstdio>
#include <cstring>
#include <ctime>
#include <thread>
#include <vector>
#include <random>

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

void run(int numberOfThreads = 1, int runtimeSeconds = 5)
{
    Sieve checkSieve;
    unsigned int passes = 0;

    printf("Computing primes to %lu on %d thread%s for %d second%s.\n", checkSieve.size(),
           numberOfThreads,
           numberOfThreads == 1 ? "" : "s",
           runtimeSeconds,
           runtimeSeconds == 1 ? "" : "s");

    double duration;
    if (numberOfThreads == 1) {
        const auto tStart = steady_clock::now();
        while (true) {
            auto res = execute();
            ++passes;
            if (duration_cast<seconds>(steady_clock::now() - tStart).count() >= runtimeSeconds) {
                checkSieve = res;
                break;
            }
        }
        duration = duration_cast<microseconds>(steady_clock::now() - tStart).count() / 1000000.0;
    } else {
        std::thread threads[numberOfThreads];
        uint64_t l_passes[numberOfThreads];
        auto randomThreadCheck = [numberOfThreads, rd = std::random_device{}]() mutable
            {return std::uniform_int_distribution{1, numberOfThreads}(rd);}();

        const auto tStart = steady_clock::now();
        for (unsigned int i = 0; i < numberOfThreads; i++)
            threads[i] = std::thread([&checkSieve, randomThreadCheck, runtimeSeconds, i, &l_passes, &tStart]()
            {
                l_passes[i] = 0;
                while (true) {
                    auto res = execute();
                    ++l_passes[i];
                    if (duration_cast<seconds>(steady_clock::now() - tStart).count() >= runtimeSeconds) {
                        if (randomThreadCheck == i)
                            checkSieve = res;
                        break;
                    }
                }
            });
        for (auto i = 0; i < numberOfThreads; i++) {
            threads[i].join();
            passes += l_passes[i];
        }
        duration = duration_cast<microseconds>(steady_clock::now() - tStart).count() / 1000000.0;
    }

    printResults(checkSieve, false, duration, passes, numberOfThreads);
}

int main(int argc, char **argv)
{
    run(thread::hardware_concurrency(), runtimeSeconds);
    run(1, runtimeSeconds);
}
