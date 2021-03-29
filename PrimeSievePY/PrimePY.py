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
#include <vector>

using namespace std;
using namespace std::chrono;

class prime_sieve
{
  private:

      long sieveSize = 0;
      vector<bool> Bits;
      const std::map<const long, const int> myDict = 
      {
            {          10L, 4         },                // Historical data for validating our results - the number of primes
            {         100L, 25        },               // to be found under some limit, such as 168 primes under 1000
            {        1000L, 168       },
            {       10000L, 1229      },
            {      100000L, 9592      },
            {     1000000L, 78498     },
            {    10000000L, 664579    },
            {   100000000L, 5761455   },
            {  1000000000L, 50847534  },
            { 10000000000L, 455052511 },

      };

      bool validateResults()
      {
          if (myDict.end() == myDict.find(sieveSize))
              return false;
          return myDict.find(sieveSize)->second == countPrimes();
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
          int q = sqrt(sieveSize);

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

          int count = 1;
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
          int count = 1;
          for (int i = 3; i < sieveSize; i+=2)
              if (Bits[i])
                  count++;
          return count;
      }
};

int main()
{
    auto passes = 0;
    prime_sieve* sieve = nullptr;

    auto tStart = steady_clock::now();
    while (duration_cast<seconds>(steady_clock::now() - tStart).count() < 5)
    {
        delete sieve;
        sieve = new prime_sieve(1000000L);
        sieve->runSieve();
        passes++;
    }
    auto tD = steady_clock::now() - tStart;
    
    if (sieve)
    {
        sieve->printResults(false, duration_cast<microseconds>(tD).count() / 1000000, passes);
        delete sieve;
    }
}
