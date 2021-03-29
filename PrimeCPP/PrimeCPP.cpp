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
#include <algorithm>

using namespace std;
using namespace std::chrono;

class prime_sieve
{
private:

    vector<bool> Bits;

    void set_false(int i)
    { Bits[i]=false; }
    
public:

    prime_sieve(long n) 
        : Bits(n/2, true) // We only need half the memory for only the odd numbers
    { }

    auto size() const // size of only odd numbers
    { return Bits.size(); }

    auto logical_size() const // size of odd and even numbers
    { return Bits.size()*2; }

    bool operator[](int i) const // read only access
    { return Bits[i]; }

    auto begin() const // begin iterator of odd number bits
    { return Bits.begin(); }

    auto end() const // end iterator of odd number bits
    { return Bits.end(); }

    void clear() // reset all bits to true
    { std::fill(Bits.begin(), Bits.end(), true); }

    void runSieve()
    {
        int factor = 3;
        int q = sqrt(logical_size());

        while (factor <= q)
        {
            auto iter = std::find(begin()+factor/2, end(), true);
            if (iter!=end())
            {
                int num=iter-begin();
                factor=num*2+1;
            }
            for (int num = factor*factor/2; num < size(); num += factor)
                set_false(num);

            factor += 2;            
        }
    }

};

int countPrimes(const prime_sieve& sieve)
{
    return std::count(sieve.begin()++, sieve.end(), true);
}

void printPrimes(const prime_sieve& sieve)
{
    cout<<"2 ";
    for (int num = 1; num < sieve.size(); num ++)
        if (sieve[num])
            cout<< num*2+1 <<' ';
    cout<<'\n';
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
    auto iter=myDict.find(sieve.logical_size());
    if (iter == myDict.end())
        return false;
    return iter->second == countPrimes(sieve);
}

void printResults(const prime_sieve& sieve, double duration, int passes)
{
    int count = countPrimes(sieve); 

    printf("Passes: %d, Time: %lf, Avg: %lf, Limit: %ld, Count1: %d, Count2: %d, Valid: %d\n", 
           passes, 
           duration, 
           duration / passes, 
           sieve.logical_size(), 
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

    printResults(sieve, duration_cast<microseconds>(tD).count() / 1000000, passes);
    // printPrimes(sieve);
}
