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

      vector<bool> Bits;

      void set_false(int i)
      {
          Bits[i]=false;
      }
    
   public:

      prime_sieve(long n) 
          : Bits(n, true)
      { }

      auto size() const
      {
          return Bits.size();
      }
    
      void clear() 
      {
          std::fill(Bits.begin(), Bits.end(), true); // reset all bits to true
      }

      bool operator[](int i) const // read only access
      {
          return Bits[i];
      }

      friend void runSieve(prime_sieve& sieve); // may access private members
};

void runSieve(prime_sieve& sieve)
{
    int factor = 3;
    int q = sqrt(sieve.size());

    while (factor <= q)
    {
        for (int num = factor/2; num < sieve.size()/2; num += 2/2)
        {
            if (sieve[num])
            {
                factor = num*2+1;
                break;
            }
        }
        for (int num = factor * factor/2; num < sieve.size()/2; num += factor * 2/2)
            sieve.set_false(num);

        factor += 2;            
    }
}

int countPrimes(const prime_sieve& sieve)
{
    int count = 1;
    for (int i = 3/2; i < sieve.size()/2; i+=2/2)
        if (sieve[i])
            count++;
    return count;
}

bool validateResults(const prime_sieve& sieve)
{
    const std::map<const long, const int> myDict = 
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
            { 10000000000L, 455052511 },

        };
    if (myDict.end() == myDict.find(sieve.size()))
        return false;
    return myDict.find(sieve.size())->second == countPrimes(sieve);
}

void printResults(const prime_sieve& sieve, double duration, int passes)
{
    int count = countPrimes(sieve); 

    printf("Passes: %d, Time: %lf, Avg: %lf, Limit: %ld, Count1: %d, Count2: %d, Valid: %d\n", 
           passes, 
           duration, 
           duration / passes, 
           sieve.size(), 
           count,
           countPrimes(sieve), 
           validateResults(sieve));
}

int main()
{
    auto passes = 0;
    prime_sieve sieve(1000000L);
    
    auto tStart = steady_clock::now();
    while (duration_cast<seconds>(steady_clock::now() - tStart).count() < 5)
    {
        sieve.clear();
        runSieve(sieve);
        passes++;
    }
    auto tD = steady_clock::now() - tStart;

    printResults(sieve, duration_cast<microseconds>(tD).count() / 1000000, passes);
}
