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

/*
vector<char> uses sizeof(bool)=1 byte (8 bits) for each bool. This is
a way to use 1 bit per bool. The hope was that with less memory the
caching would be faster but the complex indexing makes this much slower.
 */
template <typename T>
class Bits
{
    vector<T> bits;
public:
    Bits(long n)
        : bits(n)
    {
        clear();
    }

    void clear() 
    {
        T zero=0;
        std::fill(bits.begin(), bits.end(), ~zero); // reset all bits to true
    }

    auto size() const
    {
        return bits.size();
    }

    bool get(int i) const 
    {
        T bit_mask=1;
        return bits[i/sizeof(T)] & (bit_mask<< (i%sizeof(T)) );
    }

    void set_false(int i)
    {
        T bit_mask=1;
        bits[i/sizeof(T)] &= ~(bit_mask<< (i%sizeof(T)) );
    }
};

class prime_sieve
{
  private:

    Bits<unsigned long> bits;
      
   public:

      prime_sieve(long n) 
          : bits(n)
      {
      }

      auto size() const
      {
          return bits.size();
      }
    
      void clear() 
      {
          bits.clear();
      }

      bool operator[](int i) const // read only access
      {
          return bits.get(i);
      }
    
      void runSieve()
      {
          int factor = 3;
          int q = sqrt(size());

          while (factor <= q)
          {
              for (int num = factor; num < size(); num += 2)
              {
                  if (bits.get(num))
                  {
                      factor = num;
                      break;
                  }
              }
              for (int num = factor * factor; num < size(); num += factor * 2)
                  bits.set_false(num);

              factor += 2;            
          }
      }
      
};

int countPrimes(const prime_sieve& sieve)
{
    int count = 1;
    for (int i = 3; i < sieve.size(); i+=2)
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

void printResults(const prime_sieve& sieve,bool showResults, double duration, int passes)
{
    if (showResults)
        printf("2, ");

    int count = 1;
    for (int num = 3; num <= sieve.size(); num+=2)
    {
        if (sieve[num])
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
        sieve.runSieve();
        passes++;
    }
    auto tD = steady_clock::now() - tStart;

    printResults(sieve,true, duration_cast<microseconds>(tD).count() / 1000000, passes);
}
