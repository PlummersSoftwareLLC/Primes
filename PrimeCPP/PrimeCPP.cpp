// ---------------------------------------------------------------------------
// PrimeCPP.cpp : Dave's Garage Prime Sieve in C++
// ---------------------------------------------------------------------------

#include <cstdint>

#include <chrono>
#include <cstdlib>
#include <map>
#include <cstring>
#include <cmath>
#include <vector>


#if defined(_MSC_VER) && !defined(__clang_major__)
#define USE_RAWBITS
#endif

using namespace std;
using namespace std::chrono;

typedef uint64_t SizeType; //Use this to switch between 32 and 64 bits (uint32_t or uint64_t).

class prime_sieve
{
  private:

      SizeType sieveSize = 0;
#ifdef USE_RAWBITS
      unsigned char* rawbits = nullptr;
#else
      vector<bool> Bits;
#endif
      const std::map<const SizeType, const int> myDict = 
      {
            {          10L, 4         },               // Historical data for validating our results - the number of primes
            {         100L, 25        },               // to be found under some limit, such as 168 primes under 1000
            {        1000L, 168       },
            {       10000L, 1229      },
            {      100000L, 9592      },
            {     1000000L, 78498     },
            {    10000000L, 664579    },
            {   100000000L, 5761455   },
            {  1000000000L, 50847534  },
            { 10000000000L, 455052511 },               // This entry is only valid with SizeType of at least 64 bits.

      };

#ifdef USE_RAWBITS
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
#endif

      bool validateResults()
      {
          if (myDict.end() == myDict.find(sieveSize))
              return false;
          return myDict.find(sieveSize)->second == countPrimes();
      }

      bool GetBit(SizeType index)
      {
#ifdef USE_RAWBITS
          return rawbits[index / 8] & 1 << index % 8;
#else
          return Bits[index];
#endif
      }

      void ClearBit(SizeType index)
      {
#ifdef USE_RAWBITS
          rawbits[index / 8] &= ~(1 << index % 8);
#else
          Bits[index] = false;
#endif
      }

  public:

      prime_sieve(SizeType n) 
        : sieveSize(n)
      {
#ifdef USE_RAWBITS
          rawbits = (unsigned char*)malloc(n / 16 + 1);
#else
          Bits.resize(n + 1);
#endif
      }

      ~prime_sieve()
      {
#ifdef USE_RAWBITS
          free(rawbits);
#endif
      }

      void reset()
      {
#ifdef USE_RAWBITS
          if (rawbits)
              memset(rawbits, 0xff, sieveSize / 16 + 1);
#else
          std::fill(Bits.begin(), Bits.end(), true);
#endif
      }

      void runSieve()
      {
          reset();

          SizeType halfsize = sieveSize / 2;
          uint32_t factor = 1;

          uint32_t step;

          uint32_t q = (uint32_t)sqrt(sieveSize) / 2;

          while (factor <= q)
          {
              for (SizeType num = factor; num <= halfsize; ++num)
              {
                  if (GetBit(num))
                  {
                      factor = num;
                      step = factor * 2 + 1;
                      break;
                  }
              }
              for (SizeType num = factor + step; num <= halfsize; num += step)
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
          
          printf("Passes: %d, Time: %lf, Avg: %lf, Limit: %lld, Count1: %d, Count2: %d, Valid: %d\n",
                 passes, 
                 duration, 
                 duration / passes, 
                 sieveSize, 
                 count,
                 countPrimes(), 
                 validateResults());

        int count2 = countPrimes();

        int spass = 0;
        auto tStart = steady_clock::now();
        while (duration_cast<seconds>(steady_clock::now() - tStart).count() < 5)
        {
            count2 &= countPrimes();
            spass++;
        }
        auto tD = steady_clock::now() - tStart;

        double d = duration_cast<microseconds>(tD).count() / 1000000;


        printf("SPasses: %d, STime: %lf, SAvg: %lf, SCount: %d\n",

            spass,
            d,
            d / spass,
            count2);


      }

      int countPrimes()
      {
#ifdef USE_RAWBITS
          int count = 0;
          uint32_t halfsize = (sieveSize + 1) / 2;
          uint32_t limit = halfsize / 8;
          for (unsigned char* i = rawbits; i < rawbits + limit; ++i)
              count += lut[*i];

          count += lut[rawbits[limit] & 0xFF >> 8 - halfsize % 8];
          return count;
#else
          int count = 1;
          for (int i = 3; i < sieveSize; i+=2)
              if (Bits[i])
                  count++;
          return count;
#endif
      }
};

int main()
{
    auto passes = 0;
    prime_sieve* sieve = new prime_sieve(1000000U);

    auto tStart = steady_clock::now();
    while (duration_cast<seconds>(steady_clock::now() - tStart).count() < 5)
    {
        sieve->runSieve();
        passes++;
    }
    auto tD = steady_clock::now() - tStart;
    
    if (sieve)
    {
        sieve->printResults(false, duration_cast<microseconds>(tD).count() / 1000000, passes);
        delete sieve;
    }

    return 0;
}
