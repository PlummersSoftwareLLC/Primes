// ---------------------------------------------------------------------------
// PrimeCPP.cpp : Dave's Garage Prime Sieve in C++
// ---------------------------------------------------------------------------

#include <chrono>
#include <ctime>
#include <iostream>
#include <bitset>
#include <map>
#include <cstring>
#include <cmath>
#include <array>

const std::map<const int, const int> myDict =
    {
        {10, 1},   // Historical data for validating our results - the number of primes
        {100, 25}, // to be found under some limit, such as 168 primes under 1000
        {1000, 168},
        {10000, 1229},
        {100000, 9592},
        {1000000, 78498},
        {10000000, 664579},
        {100000000, 5761455}};

template <size_t N>
class prime_sieve
{
private:

    static bool validateResults(const std::array<unsigned char, N / 8 + 1>& a)
    {
        if (myDict.end() == myDict.find(N))
            return false;
        return myDict.find(N)->second == countPrimes(a);
    }

    __attribute__((always_inline)) static constexpr bool GetBit(const std::array<unsigned char, N / 8 + 1>& a, unsigned int index)
    {
        auto val = index % 2;
        index = index / 2;
        return (a[index / 8] & (val << (index % 8))) != 0;
    }

    __attribute__((always_inline)) static consteval void ClearBit(std::array<unsigned char, N / 8 + 1>& a, unsigned int index)
    {

        auto val = index % 2;
        index = index / 2;
        a[index / 8] &= ~(val << (index % 8));
    }

public:

    static consteval auto runSieve()
    {
        std::array<unsigned char, N / 8 + 1> rawbits2{};
        for(auto& e : rawbits2) e = 0xff;
        int factor = 3;
        int q = sqrt(N);

        while (factor < q)
        {
            for (int num = factor; num < N; num++)
            {
                if (GetBit(rawbits2, num))
                {
                    factor = num;
                    break;
                }
            }
            for (int num = factor * 3; num < N; num += factor * 2)
                ClearBit(rawbits2, num);

            factor += 2;
        }
        return rawbits2;
    }

    static void printResults(const std::array<unsigned char, N / 8 + 1>& a, bool showResults, double duration, int passes)
    {
        if (showResults)
            printf("2, ");

        int count = 1;
        for (int num = 3; num <= N; num++)
        {
            if (GetBit(a, num))
            {
                if (showResults)
                    printf("%d, ", num);
                count++;
            }
        }

        if (showResults)
            printf("\n");

        printf("Passes: %d, Time: %lf, Avg: %lf, Limit: %d, Count: %d, Valid: %d\n",
               passes,
               duration,
               duration / passes,
               static_cast<int>(N),
               count,
               validateResults(a));
    }

    static int countPrimes(const std::array<unsigned char, N / 8 + 1>& a)
    {
        int count = 0;
        for (int i = 0; i < N; i++)
            if (GetBit(a, i))
                count++;
        return count;
    }
};

int main()
{
    auto passes = 0;
    std::array<unsigned char, 1000000 / 8 + 1> data;

    auto tStart = std::chrono::steady_clock::now();
    while(std::chrono::duration_cast<std::chrono::seconds>(std::chrono::steady_clock::now() - tStart).count() < 10)
    {
        data = prime_sieve<1000000>::runSieve();
        passes++;
    }
    auto tD = std::chrono::steady_clock::now() - tStart;

    prime_sieve<1000000>::printResults(data, false, std::chrono::duration_cast<std::chrono::microseconds>(tD).count() / 1000000.0, passes);
}
