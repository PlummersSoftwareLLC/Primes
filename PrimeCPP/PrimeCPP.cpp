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
#include <array>
#include <algorithm>

using namespace std;
using namespace std::chrono;

template <long Size>
class prime_sieve
{
private:

    array<bool,Size/2> Bits; // We only need half the memory for only the odd numbers

    void set_false(int i)
    { Bits[i]=false; }
    
public:

    prime_sieve()
    { clear(); }

    void clear() // reset all bits to true
    { std::fill(Bits.begin(), Bits.end(), true); }
    
    long odd_size() const // size of only odd numbers
    { return Bits.size(); }

    bool operator[](int i) const // read only access
    { return Bits[i]; }

    auto begin() const // begin iterator of odd number bits
    { return Bits.begin(); }

    auto end() const // end iterator of odd number bits
    { return Bits.end(); }

    void runSieve()
    {
        for (int f=1; f<=sqrt(Size)/2; f++) // knowing sqrt(Size) at compile time doesn't help performance, strange 
        {
            // --------------------------
            for (int num = f; num < odd_size(); num++)
                if (Bits[num])
                {
                    f=num;
                    break;
                }
            //-------------------------- or use:
            /* auto iter = std::find(begin()+f, end(), true);
            if (iter!=end())
                f=iter-begin();*/
            //--------------------------
            
            for (int num = f*2*(1+f); num < odd_size(); num += f*2+1)
                set_false(num);
        }
    }

};

template <long Size>
int countPrimes(const prime_sieve<Size>& sieve)
{
    auto iter=sieve.begin();
    return std::count(iter++, sieve.end(), true);
}

template <long Size>
void printPrimes(const prime_sieve<Size>& sieve)
{
    cout<<"2 ";
    for (int num = 1; num < sieve.odd_size(); num ++)
        if (sieve[num])
            cout<< num*2+1 <<' ';
    cout<<'\n';
}

template <long Size>
bool validateResults(const prime_sieve<Size>& sieve)
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
    auto iter=myDict.find(Size);
    if (iter == myDict.end())
        return false;
    return iter->second == countPrimes(sieve);
}

template <long Size>
void printResults(const prime_sieve<Size>& sieve, double duration, int passes)
{
    int count = countPrimes(sieve); 

    printf("Passes: %d, Time: %lf, Avg: %lf, Limit: %ld, Count1: %d, Count2: %d, Valid: %d\n", 
           passes, 
           duration, 
           duration / passes, 
           Size, 
           count,
           countPrimes(sieve), 
           validateResults(sieve));
}

int main()
{
    auto passes = 0;
    prime_sieve<1000000L> sieve;
    
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
