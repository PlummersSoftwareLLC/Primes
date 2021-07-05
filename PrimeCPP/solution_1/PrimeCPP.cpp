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
#include <limits>
#include <cmath>

using namespace std;
using namespace std::chrono;

static const std::map<const long long, const int> resultsDictionary = 
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

static constexpr size_t _log2(size_t n) { return ( (n<2) ? 0 : 1+_log2(n/2)); }
static constexpr bool is_powerof2(size_t v) { return v && ((v & (v - 1)) == 0); }	

class dynamic_bitset {
	static constexpr size_t bucket_size = 32;
	static_assert(is_powerof2(bucket_size));
	std::vector<std::bitset<bucket_size>> data;
		
	static constexpr size_t mod = bucket_size - 1;
	static constexpr size_t div = _log2(bucket_size);	
 public:
	dynamic_bitset(unsigned long n, bool): data((n >> div)+1, 0) {}
	~dynamic_bitset() = default; 
	
	void clear(size_t position) noexcept {
		//size_t position = pos >> 1;
        size_t block_id = position >> div;
        size_t offset = position & mod;
		
		data[block_id].set(offset, true);
	}
	
	bool operator[](size_t position) const noexcept {
		//size_t position = pos >> 1;
        size_t block_id = position >> div;
        size_t offset = position & mod;		
		
		return (data[block_id][offset] == 0);
	}
};

class prime_sieve
{
  private:
      size_t sieveSize;
	  dynamic_bitset Bits;
	 
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
          size_t factor = 3;
          size_t q = (size_t) sqrt(sieveSize);

          while (factor <= q)
          {
              for (size_t num = factor; num < sieveSize; num += 2)
              {
                  if (Bits[num])
                  {
                      factor = num;
                      break;
                  }
              }
              for (size_t num = factor * factor; num < sieveSize; num += factor * 2)
                  Bits.clear(num); // = false;

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

          // Following 2 lines added by rbergen to conform to drag race output format
          printf("\n");       
          printf("davepl;%d;%f;1;algorithm=base,faithful=yes,bits=1\n", passes, duration);
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
            sieve.printResults(false, duration_cast<microseconds>(steady_clock::now() - tStart).count() / 1000000.0, passes);
            break;
        }
    } 
}
