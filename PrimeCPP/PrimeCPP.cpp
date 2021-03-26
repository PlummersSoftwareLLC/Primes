// ---------------------------------------------------------------------------
// PrimeCPP.cpp : Dave's Garage Prime Sieve in C++
// ---------------------------------------------------------------------------

#include <chrono>
#include <vector>
#include <ctime>
#include <cmath>
#include <memory>
#include <bitset>
#include <map>

class prime_sieve
{
  private:

      int sieveSize = 0;
      std::vector<unsigned char> rawbits;
      const std::map<const int, const int> myDict =
      {
            { 10 , 1 },                 // Historical data for validating our results - the number of primes
            { 100 , 25 },               // to be found under some limit, such as 168 primes under 1000
            { 1000 , 168 },
            { 10000 , 1229 },
            { 100000 , 9592 },
            { 1000000 , 78498 },
            { 10000000 , 664579 },
            { 100000000 , 5761455 }
      };

      bool validateResults()
      {
          if (myDict.end() == myDict.find(sieveSize))
              return false;
          return myDict.find(sieveSize)->second == countPrimes();
      }

      bool GetBit(unsigned int index)
      {
          if (index % 2 == 0)
              return false;
          index = index / 2;
          return ((rawbits[index / 8]) & (1 << (index % 8))) != 0;
      }

      void ClearBit(unsigned int index)
      {
          if (index % 2 == 0)
          {
              printf("You're setting even bits, which is sub-optimal.\n");
              return;
          }
          index = index / 2;
          rawbits[index / 8] &= ~(1 << (index % 8));
      }

  public:

      prime_sieve(int n)
      {
          sieveSize = n;
          rawbits = std::vector<unsigned char>(n / 8 + 1, 0xff);
      }


      void runSieve()
      {
          int factor = 3;
          int q = sqrt(sieveSize);

          while (factor < q)
          {
              for (int num = factor; num < sieveSize; num++)
              {
                  if (GetBit(num))
                  {
                      factor = num;
                      break;
                  }
              }
              for (int num = factor * 3; num < sieveSize; num += factor * 2)
                  ClearBit(num);
               
              factor += 2;
          }
      }

      void printResults(bool showResults, double duration, int passes)
      {
          if (showResults)
              printf("2, ");

          int count = 1;
          for (int num = 3; num <= sieveSize; num++)
          {
              if (GetBit(num))
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
                 sieveSize, 
                 count, 
                 validateResults());
      }

      int countPrimes()
      {
          int count = 0;
          for (int i = 0; i < sieveSize; i++)
              if (GetBit(i))
                  count++;
          return count;
      }
};

int main()
{
    auto passes = 0;
    std::unique_ptr<prime_sieve> sieve;

    auto tStart = std::chrono::steady_clock::now();
    while (std::chrono::duration_cast<std::chrono::seconds>(std::chrono::steady_clock::now() - tStart).count() < 10)
    {
        sieve = std::make_unique<prime_sieve>(1000000);
        sieve->runSieve();
        passes++;
    }
    auto tD = std::chrono::steady_clock::now() - tStart;
    
    if (sieve)
    {
        sieve->printResults(false, std::chrono::duration_cast<std::chrono::microseconds>(tD).count() / 1000000, passes);
    }
}
