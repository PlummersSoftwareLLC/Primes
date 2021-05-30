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

const uint64_t DEFAULT_UPPER_LIMIT = 10'000'000LLU;

// prime_sieve
//
// Represents the data comprising the sieve (an array of N bits, where N is the upper limit prime being tested)
// as well as the code needed to eliminate non-primes from its array, which you perform by calling runSieve.

class prime_sieve
{
  private:

      vector<bool> Bits;                                        // Sieve data, where 1==prime, 0==not

   public:

      prime_sieve(uint64_t n) : Bits(n, true)                  // Initialize all to true (potential primes)
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
          uint64_t factor = 3;
          uint64_t q = (int) sqrt(Bits.size());

          while (factor <= q)
          {
              for (uint64_t num = factor; num < Bits.size(); num += 2)
              {
                  if (Bits[num])
                  {
                      factor = num;
                      break;
                  }
              }
              for (uint64_t num = factor * factor; num < Bits.size(); num += factor * 2)
                  Bits[num] = false;

              factor += 2;            
          }
      }

      // countPrimes
      //
      // Can be called after runSieve to determine how many primes were found in total

      size_t countPrimes() const
      {
          size_t count = (Bits.size() >= 2);                   // Count 2 as prime if within range
          for (int i = 3; i < Bits.size(); i+=2)
              if (Bits[i])
                  count++;
          return count;
      }

      // isPrime 
      // 
      // Can be called after runSieve to determine whether a given number is prime. 

      bool isPrime(uint64_t n) const
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
          const std::map<const uint64_t, const int> resultsDictionary =
          {
                {             10LLU, 4         },               // Historical data for validating our results - the number of primes
                {            100LLU, 25        },               // to be found under some limit, such as 168 primes under 1000
                {          1'000LLU, 168       },
                {         10'000LLU, 1229      },
                {        100'000LLU, 9592      },
                {      1'000'000LLU, 78498     },
                {     10'000'000LLU, 664579    },
                {    100'000'000LLU, 5761455   },
                {  1'000'000'000LLU, 50847534  },
                { 10'000'000'000LLU, 455052511 },
          };
          if (resultsDictionary.end() == resultsDictionary.find(Bits.size()))
              return false;
          return resultsDictionary.find(Bits.size())->second == countPrimes();
      }

      // printResults
      //
      // Displays stats about what was found as well as (optionally) the primes themselves

      void printResults(bool showResults, double duration, size_t passes, size_t threads) const
      {
          if (showResults)
              cout << "2, ";

          size_t count = (Bits.size() >= 2);                   // Count 2 as prime if in range
          for (uint64_t num = 3; num <= Bits.size(); num+=2)
          {
              if (Bits[num])
              {
                  if (showResults)
                      cout << num << ", ";
                  count++;
              }
          }

          if (showResults)
              cout << "\n";
          
          cout << "Passes: "  << passes << ", "
               << "Threads: " << threads << ", "
               << "Time: "    << duration << ", " 
               << "Average: " << duration/passes << ", "
               << "Limit: "   << Bits.size() << ", "
               << "Counts: "  << count << "/" << countPrimes() << ", "
               << "Valid : "  << (validateResults() ? "Pass" : "FAIL!") 
               << "\n";

          // Following 2 lines added by rbergen to conform to drag race output format
          cout << "\n";
          cout << "davepl_par;" << passes << ";" << duration << ";" << threads << ";algorithm=base,faithful=yes,bits=1\n";
      }               
  
};

int main(int argc, char **argv)
{
    vector<string> args(argv + 1, argv + argc);         // From first to last argument in the argv array
    uint64_t ullLimitRequested = 0;
    auto cThreadsRequested = 0;
    auto cSecondsRequested = 0;
    auto bPrintPrimes      = false;
    auto bOneshot          = false;
    auto bQuiet            = false;

    // Process command-line args

    for (auto i = args.begin(); i != args.end(); ++i) 
    {
        if (*i == "-h" || *i == "--help") {
              cout << "Syntax: " << argv[0] << " [-t,--threads threads] [-s,--seconds seconds] [-l,--limit limit] [-1,--oneshot] [-q,--quiet] [-h] " << endl;
            return 0;
        }
        else if (*i == "-t" || *i == "--threads") 
        {
            i++;
            cThreadsRequested = (i == args.end()) ? 0 : max(1, atoi(i->c_str()));
        }
        else if (*i == "-s" || *i == "--seconds") 
        {
            i++;
            cSecondsRequested = (i == args.end()) ? 0 : max(1, atoi(i->c_str()));
        }
        else if (*i == "-l" || *i == "--limit") 
        {
            i++;
            ullLimitRequested = (i == args.end()) ? 0LL : max((long long)1, atoll(i->c_str()));
        }
        else if (*i == "-1" || *i == "--oneshot") 
        {
            bOneshot = true;
            cThreadsRequested = 1;
        }
        else if (*i == "-p" || *i == "--print") 
        {
             bPrintPrimes = true;
        }
        else if (*i == "-q" || *i == "--quiet") 
        {
             bQuiet = true;
        }        
        else 
        {
            fprintf(stderr, "Unknown argument: %s", i->c_str());
            return 0;
        }
    }

    if (!bQuiet)
    {
        cout << "Primes Benchmark (c) 2021 Dave's Garage - http://github.com/davepl/primes" << endl;
        cout << "-------------------------------------------------------------------------" << endl;
    }

    if (bOneshot)
        cout << "Oneshot is on.  A single pass will be used to simulate a 5 second run." << endl;

    if (bOneshot && (cSecondsRequested > 0 || cThreadsRequested > 1))   
    {
        cout << "Oneshot option cannot be mixed with second count or thread count." << endl;
        return 0;
    }

    auto cPasses      = 0;
    auto cSeconds     = (cSecondsRequested ? cSecondsRequested : 5);
    auto cThreads     = (cThreadsRequested ? cThreadsRequested : thread::hardware_concurrency());
    auto llUpperLimit = (ullLimitRequested ? ullLimitRequested : DEFAULT_UPPER_LIMIT);

    if (!bQuiet)
    {
        printf("Computing primes to %llu on %d thread%s for %d second%s.\n", 
            llUpperLimit,
            cThreads,
            cThreads == 1 ? "" : "s",
            cSeconds,
            cSeconds == 1 ? "" : "s"
        );
    }
    auto tStart       = steady_clock::now();

    if (bOneshot)
    {
        std::unique_ptr<prime_sieve>(new prime_sieve(llUpperLimit))->runSieve();
    }
    else
    {
        while (duration_cast<seconds>(steady_clock::now() - tStart).count() < cSeconds)
        {
            vector<thread> threadPool;
            
            // We create N threads and give them each the job of runing the 'runSieve' method on a sieve
            // that we create on the heap, rather than the stack, due to their possible enormity.  By using
            // a unique_ptr it will automatically free resources as soon as its torn down.

            for (unsigned int i = 0; i < cThreads; i++)
                threadPool.push_back(thread([llUpperLimit] 
                { 
                    std::unique_ptr<prime_sieve>(new prime_sieve(llUpperLimit))->runSieve(); 
                }));

            // Now we wait for all of the threads to finish before we repeat

            for (auto &th : threadPool) 
                th.join();

            // Credit us with one pass for each of the threads we did work on
            cPasses += cThreads;
        }
    }

    auto tEnd = steady_clock::now() - tStart;
    auto duration = duration_cast<microseconds>(tEnd).count()/1000000.0;

    if (bOneshot)
    {
        cPasses = 1.0 / duration * 5;
        duration = 5.0;
    }

    prime_sieve checkSieve(llUpperLimit);
    checkSieve.runSieve();
    auto result = checkSieve.validateResults() ? checkSieve.countPrimes() : 0;
  
    if (!bQuiet)
        checkSieve.printResults(bPrintPrimes, duration , cPasses, cThreads);
    else
        cout << cPasses << ", " << duration / cPasses << endl;

    // On success return the count of primes found; on failure, return 0

    return (int) result;
}
