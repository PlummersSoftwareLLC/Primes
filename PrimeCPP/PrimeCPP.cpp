// ---------------------------------------------------------------------------
// PrimeCPP.cpp : Dave's Garage Prime Sieve in C++ - No warranty for anything!
// ---------------------------------------------------------------------------

#include <chrono>
#include <ctime>
#include <iostream>
#include <bitset>
#include <map>
#include <cstring>
#include <cmath>
#include <vector>
#include <cstdio>

using namespace std;
using namespace std::chrono;

class prime_sieve
{
  private:

      uint32_t sieveSize = 0;
      vector<bool> Bits;
      const std::map<const uint64_t, const uint32_t> resultsDictionary =
      {
            {          10LL, 4         },               // Historical data for validating our results - the number of primes
            {         100LL, 25        },               // to be found under some limit, such as 168 primes under 1000
            {        1000LL, 168       },
            {       10000LL, 1229      },
            {      100000LL, 9592      },
            {     1000000LL, 78498     },
            {    10000000LL, 664579    },
            {   100000000LL, 5761455   },
            {  1000000000LL, 50847534  },
            { 10000000000LL, 455052511 },

      };


      bool validateResults()
      {
          auto result = resultsDictionary.find(sieveSize);
          if (resultsDictionary.end() == result)
              return false;
          return result->second == countPrimes();
      }

   public:

      prime_sieve(uint32_t n)
        : sieveSize(n),Bits(n, true)
      {
      }

      ~prime_sieve()
      {
      }

      void runSieve()
      {
          uint32_t factor = 3;
          uint32_t q = (uint32_t) sqrt(sieveSize);
          uint32_t num;

          while (factor <= q)
          {
              num = factor;

              if (Bits[num])
              {
                 factor = num;
                 for (uint32_t p = factor * factor; p < sieveSize; p += factor * 2){Bits[p] = false;}
              }

              factor += 2;
          }
      }

      void printResults(bool showResults, double duration, int passes)
      {
          if (showResults)
              printf("2, ");

          uint32_t count = (sieveSize >= 2);                             // Starting count (2 is prime)
          for (uint32_t num = 3; num <= sieveSize; num+=2)
          {
              if (Bits[num])
              {
                  if (showResults)
                      printf("%d", num);
                  count++;
              }
          }

          if (showResults)
              printf("\n");

          printf("Passes: %d, Time: %lf, Avg: %lf, Limit: %d, Count1: %d, Count2: %d, Valid: %d\n",
                 passes,
                 duration,
                 duration / passes,
                 sieveSize,
                 count,
                 countPrimes(),
                 validateResults());
      }

      uint32_t countPrimes()
      {
          uint32_t count =  (sieveSize >= 2);;
          for (uint32_t i = 3; i < sieveSize; i+=2)
              if (Bits[i])
                  count++;
          return count;
      }
};

int main()
{
    auto passes = 0;
    auto tStart = steady_clock::now();

    while (true)
    {
        prime_sieve sieve(1000000L);

        sieve.runSieve();
        passes++;
        if (duration_cast<seconds>(steady_clock::now() - tStart).count() >= 5)
        {
            sieve.printResults(false, duration_cast<microseconds>(steady_clock::now() - tStart).count() / 1000000, passes);
            break;
        }
    }
}
