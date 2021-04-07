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

      long sieveSize = 0;
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
          return result->second == countPrimes();
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
              for (int num = factor; num < sieveSize; num += 2)
              {
                  if (Bits[num])
                  {
                      factor = num;
                      break;
                  }
              }
              for (int num = factor * factor; num < sieveSize; num += factor * 2)
                  Bits[num] = false;

              factor += 2;
          }
      }

      void printResults(bool showResults, double duration, int passes)
      {
          if (showResults)
              printf("2, ");

          int count = (sieveSize >= 2);                             // Starting count (2 is prime)
          for (int num = 3; num <= sieveSize; num+=2)
          {
              if (Bits[num])
              {
                  if (showResults)
                      printf("%d, ", num);
                  count++;
              }
          }

          if (showResults)
              printf("\n");

          printf("Passes: %d, Time: %lf, Avg: %lf, Limit: %ld, Count1: %d, Count2: %d, Valid: %d\n", 
                 passes,
                 duration,
                 duration / passes,
                 sieveSize,
                 count,
                 countPrimes(),
                 validateResults());
      }

      int countPrimes()
      {
          int count =  (sieveSize >= 2);;
          for (int i = 3; i < sieveSize; i+=2)
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
