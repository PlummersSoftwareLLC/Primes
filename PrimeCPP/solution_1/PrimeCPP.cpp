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

using namespace std;
using namespace std::chrono;

class prime_sieve
{
  private:

      long sieveSize = 0, count = -1;
      vector<bool> Bits;
      const std::map<const long long, const int> resultsDictionary = 
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
          countPrimes();
          return result->second == count;
      }

   public:

      prime_sieve(long n) 
        : Bits(n, true), sieveSize(n)
      {
      }

      ~prime_sieve()
      {
      }

      void runSieve()
      {
          int factor = 3;
          int q = (int) sqrt(sieveSize);

          while (factor <= q)
          {
              if (Bits[factor])
              {
                  for (int num = factor * factor; num < sieveSize; num += factor * 2)
                      Bits[num] = false;
              }

              if (factor % 10 == 3)
              {
                  factor += 2;
              }
              factor += 2;
          }
      }

      void printResults(bool showResults, double duration, int passes)
      {
          if (showResults)
          {
              printf("2, 3, 5, ");

              for (int num = 7; num <= sieveSize; num += 2)
              {
                  if (Bits[num])
                  {
                      printf("%d, ", num);
                  }
                  if (num % 10 == 3)
                  {
                      num += 2;
                  }
              }

              printf("\n");
          }

          countPrimes();
          printf("Passes: %d, Time: %lf, Avg: %lf, Limit: %ld, Count: %ld, Valid: %d\n", 
                 passes,
                 duration,
                 duration / passes,
                 sieveSize,
                 count,
                 validateResults());

          // Following 2 lines added by rbergen to conform to drag race output format
          printf("\n");       
          printf("davepl;%d;%f;1;algorithm=base,faithful=yes,bits=1\n", passes, duration);
      }

      void countPrimes()
      {
          count = (sieveSize >= 2);
          for (int i = 3; i < sieveSize; i+=2)
              if (Bits[i])
                  count++;
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
            sieve.printResults(false, duration_cast<microseconds>(steady_clock::now() - tStart).count() / 1000000.0, passes);
            break;
        }
    } 
}
