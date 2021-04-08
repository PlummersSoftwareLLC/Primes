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
#include <thread>
#include <memory>

using namespace std;
using namespace std::chrono;

#define UPPER_LIMIT 1000000LL

// prime_sieve
//
// Represents the data comprising the sieve (an array of N bits, where N is the upper limit prime being tested)
// as well as the code needed to eliminate non-primes from its array, which you perform by calling runSieve.

class prime_sieve
{
  private:

      vector<bool> Bits;                                        // Sieve data, where 1==prime, 0==not

   public:

      prime_sieve(unsigned long long n) : Bits(n, true)                  // Initialize all to true (potential primes)
      {
      }

      ~prime_sieve()
      {
      }

      // runSieve
      //
      // Scan the array for the next factor (>2) that hasn't yet been eliminated from the array, and then
      // walk through the array crossing off every multiple of that factor.

      void runSieve()
      {
          long long factor = 3;
          long long q = (int) sqrt(Bits.size());

          while (factor <= q)
          {
              for (unsigned long long num = factor; num < Bits.size(); num += 2)
              {
                  if (Bits[num])
                  {
                      factor = num;
                      break;
                  }
              }
              for (unsigned long long num = factor * factor; num < Bits.size(); num += factor * 2)
                  Bits[num] = false;

              factor += 2;            
          }
      }

      // countPrimes
      //
      // Can be called after runSieve to determine how many primes were found in total

      int countPrimes() const
      {
          int count = (Bits.size() >= 2);                   // Count 2 as prime if within range
          for (int i = 3; i < Bits.size(); i+=2)
              if (Bits[i])
                  count++;
          return count;
      }

      // isPrime 
      // 
      // Can be called after runSieve to determine whether a given number is prime. 

      bool isPrime(long long n) const
      {
          if (n & 1)
              return Bits[n];
          else
              return false;
      }

      // validateResults
      //
      // Checks to see if the number of primes found matches what we should expect.  This data isn't used in the
      // sieve processing at all, only to sanity check that the results are right when done.

      bool validateResults() const
      {
          const std::map<const long long, const int> resultsDictionary =
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
          if (resultsDictionary.end() == resultsDictionary.find(Bits.size()))
              return false;
          return resultsDictionary.find(Bits.size())->second == countPrimes();
      }

      // printResults
      //
      // Displays stats about what was found as well as (optionally) the primes themselves

      void printResults(bool showResults, double duration, int passes, int threads) const
      {
          if (showResults)
              printf("2, ");

          int count = (Bits.size() >= 2);                   // Count 2 as prime if in range
          for (int num = 3; num <= Bits.size(); num+=2)
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
          
          printf("Passes: %d, Threads: %d, Time: %lf, Avg: %lf, Limit:  %lu, Count1: %d, Count2: %d, Valid: %d\n",
                 passes, 
                 threads,
                 duration, 
                 duration / passes, 
                 Bits.size(),
                 count,
                 countPrimes(), 
                 validateResults());
      }
};

int main(int argc, char **argv)
{
    vector<string> args(argv + 1, argv + argc);
    unsigned long long ullLimitRequested = 0;
    auto cThreadsRequested = 0;
    auto cSecondsRequested = 0;
    auto bPrintPrimes      = false;

    // Process command-line args

    cout << "Primes Benchmark (c) 2021 Dave's Garage - http://github.com/davepl/primes" << endl;
    cout << "-------------------------------------------------------------------------" << endl;

    for (auto i = args.begin(); i != args.end(); ++i) 
    {
        if (*i == "-h" || *i == "--help") {
              cout << "Syntax: " << argv[0] << " [-t,--threads threads] [-s,--seconds seconds] [-l,--limit limit] [-h] " << endl;
            return 0;
        }
        else if (*i == "-t" || *i == "--threads") {
            i++;
            cThreadsRequested = (i == args.end()) ? 0 : atoi(i->c_str());
        }
        else if (*i == "-s" || *i == "--seconds") {
            i++;
            cSecondsRequested = (i == args.end()) ? 0 : atoi(i->c_str());
        }
        else if (*i == "-l" || *i == "--limit") {
            i++;
            ullLimitRequested = (i == args.end()) ? 0LL : atoll(i->c_str());
        }
        else if (*i == "-p" || *i == "--print") {
             bPrintPrimes ;
        }
        else {
            fprintf(stderr, "Unknown argument: %s", i->c_str());
            return 0;
        }

    }

    auto cPasses      = 0;
    auto tStart       = steady_clock::now();
    auto cSeconds     = (cSecondsRequested ? cSecondsRequested : 5);
    auto cThreads     = (cThreadsRequested ? cThreadsRequested : thread::hardware_concurrency());
    auto llUpperLimit = (ullLimitRequested  ? ullLimitRequested  : 1000000);

    printf("Computing primes to %llu on %d thread%s for %d second%s.\n", 
           llUpperLimit,
           cThreads,
           cThreads == 1 ? "" : "s",
           cSeconds,
           cSeconds == 1 ? "" : "s"
    );

    while (duration_cast<seconds>(steady_clock::now() - tStart).count() < cSeconds)
    {
        vector<thread> threadPool;
        
        // We create N threads and give them each the job of runing the 'runSieve' method on a sieve
        // that we create on the heap, rather than the stack, due to their possible enormity.  By using
        // a unique_ptr it will automatically free resources as soon as its torn down.

        for (unsigned int i = 0; i < cThreads; i++)
            threadPool.push_back(thread([llUpperLimit] { make_unique<prime_sieve>(llUpperLimit)->runSieve(); }));

        // Now we wait for all of the threads to finish before we repeat

        for (auto &th : threadPool) 
            th.join();

        // Credit us with one pass for each of the threads we did work on
        cPasses += cThreads;
    }
    auto tEnd = steady_clock::now() - tStart;
    
    prime_sieve checkSieve(llUpperLimit);
    checkSieve.runSieve();
    checkSieve.printResults(false, duration_cast<microseconds>(tEnd).count() / 1000000.0, cPasses, cThreads);

    // On success return the count of primes found; on failure, return 0

    return checkSieve.validateResults() ? checkSieve.countPrimes() : 0;
}
