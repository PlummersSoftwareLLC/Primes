// ---------------------------------------------------------------------------
// PrimeCPP.cpp : Pol Marcet's Modified version of Dave's Garage Prime Sieve
// Some great ideas taken from Rust's implementation from Michael Barber
// @mike-barber https://www.github.com/mike-barber (bit-storage-rotate)
// ---------------------------------------------------------------------------

#include <chrono>
#include <ctime>
#include <iostream>
#include <bitset>
#include <map>
#include <cstring>
#include <cmath>

using namespace std;
using namespace std::chrono;

class BitArray {
    uint32_t *array;
    size_t arrSize;

    inline static size_t arraySize(size_t size) {
        return (size >> 5) + ((size & 31) > 0);
    }

    inline static size_t index(size_t n) {
        return (n >> 5);
    }

    inline static uint32_t getSubindex(size_t n, uint32_t d) {
        return d & uint32_t(uint32_t(0x01) << (n % 32));
    }

    inline void setFalseSubindex(size_t n, uint32_t &d) {
        d &= ~uint32_t(uint32_t(0x01) << (n % (8*sizeof(uint32_t))));
    }
public:
    explicit BitArray(size_t size) : arrSize(size) {
        array = new uint32_t[arraySize(size)];
        std::memset(array, 0xFF, (size >> 3) + ((size & 7) > 0));
    }

    ~BitArray() {delete [] array;}

    bool get(size_t n) const {
        return getSubindex(n, array[index(n)]);
    }

    static constexpr uint32_t rol(uint32_t x, uint32_t n) {
        return (x<<n) | (x>>(32-n));
    }

    void setFlagsFalse(size_t n, size_t skip) {
        auto rolling_mask = ~uint32_t(1 << n % 32);
        auto roll_bits = skip % 32;
        while (n < arrSize) {
            array[index(n)] &= rolling_mask;
            n += skip;
            rolling_mask = rol(rolling_mask, roll_bits);
        }
    }
};

class prime_sieve
{
  private:

      long sieveSize = 0;
      BitArray Bits;
      static const std::map<const long long, const int> resultsDictionary;

      bool validateResults()
      {
          auto result = resultsDictionary.find(sieveSize);
          if (resultsDictionary.end() == result)
              return false;
          return result->second == countPrimes();
      }

   public:

      prime_sieve(long n) 
        : Bits(n), sieveSize(n)
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
                  if (Bits.get(num))
                  {
                      factor = num;
                      break;
                  }
              }
              Bits.setFlagsFalse(factor * factor, factor + factor);

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
              if (Bits.get(num))
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

          // Following 2 lines added by rbergen to conform to drag race output format
          printf("\n");       
          printf("davepl_pol;%d;%f;1;algorithm=base,faithful=yes,bits=1\n", passes, duration);
      }

      int countPrimes()
      {
          int count =  (sieveSize >= 2);;
          for (int i = 3; i < sieveSize; i+=2)
              if (Bits.get(i))
                  count++;
          return count;
      }
};

const std::map<const long long, const int> prime_sieve::resultsDictionary =
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
