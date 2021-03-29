// ---------------------------------------------------------------------------
// PrimeCPP.cpp : Dave's Garage Prime Sieve in C++
// ---------------------------------------------------------------------------

#include <chrono>
#include <cstdlib>
#include <map>
#include <cstring>
#include <cmath>

class prime_sieve
{
  private:

      int sieveSize = 0;
      unsigned char * rawbits = nullptr;
      const std::map<const int, const int> myDict = 
      {
            { 10 , 4 },                 // Historical data for validating our results - the number of primes
            { 100 , 25 },               // to be found under some limit, such as 168 primes under 1000
            { 1000 , 168 },
            { 10000 , 1229 },
            { 100000 , 9592 },
            { 1000000 , 78498 },
            { 10000000 , 664579 },
            { 100000000 , 5761455 }
      };

      // Number of bits on for each byte
      const unsigned char lut[256] =
      {
            0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,
            1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
            1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
            2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
            1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
            2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
            2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
            3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
            1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
            2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
            2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
            3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
            2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
            3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
            3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
            4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8
        };

      bool validateResults()
      {
          if (myDict.end() == myDict.find(sieveSize))
              return false;
          return myDict.find(sieveSize)->second == countPrimes();
      }

      bool GetBit(unsigned int index)
      {
          return rawbits[index / 8] & 1 << index % 8;
      }

      void ClearBit(unsigned int index)
      {
          rawbits[index / 8] &= ~(1 << index % 8);
      }

  public:

      prime_sieve(int n)
      {
          sieveSize = n;
          rawbits = (unsigned char *) malloc(n / 16 + 1);
      }

      ~prime_sieve()
      {
          free(rawbits);
      }

      void reset()
      {
          if (rawbits)
              memset(rawbits, 0xff, sieveSize / 16 + 1);
      }

      void runSieve()
      {
          reset();

          int halfsize = sieveSize / 2;
          int factor = 1;
          int step;
          int q = (int)sqrt(sieveSize) / 2;

          while (factor <= q)
          {
              for (int num = factor; num <= halfsize; ++num)
              {
                  if (GetBit(num))
                  {
                      factor = num;
                      step = factor * 2 + 1;
                      break;
                  }
              }
              for (int num = factor + step; num <= halfsize; num += step)
                  ClearBit(num);
               
              ++factor;
          }
      }

      void printResults(bool showResults, double duration, int passes)
      {
          if (showResults)
              printf("2, ");

          int count = 1;
          for (int num = 1; num < (sieveSize + 1) / 2; ++num)
          {
              if (GetBit(num))
              {
                  if (showResults)
                      printf("%d, ", num * 2 + 1);
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
          int halfsize = (sieveSize + 1) / 2;
          int limit = halfsize / 8;
          for (unsigned char* i = rawbits; i < rawbits + limit; ++i)
              count += lut[*i];

          count += lut[rawbits[limit] & 0xFF >> 8 - halfsize % 8];

          return count;
      }
};

int main()
{
    auto passes = 0;
    prime_sieve* sieve = new prime_sieve(1000000);

    auto tStart = std::chrono::steady_clock::now();
    while (std::chrono::duration_cast<std::chrono::seconds>(std::chrono::steady_clock::now() - tStart).count() < 10)
    {
        sieve->runSieve();
        passes++;
    }
    auto tD = std::chrono::steady_clock::now() - tStart;
    
    if (sieve)
    {
        sieve->printResults(false, std::chrono::duration_cast<std::chrono::microseconds>(tD).count() / 1000000, passes);
        delete sieve;
    }
}
