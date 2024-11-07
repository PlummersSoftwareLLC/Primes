// ---------------------------------------------------------------------------
// PrimeCPP.cpp : Pol Marcet's Modified version of Dave's Garage Prime Sieve
// Some great ideas taken from Rust's implementation from Michael Barber
// @mike-barber https://www.github.com/mike-barber (bit-storage-rotate)
// ---------------------------------------------------------------------------

#include <chrono>
#include <ctime>
#include <iostream>
#include <sstream>
#include <bitset>
#include <map>
#include <unordered_map>
#include <stdexcept>
#include <cstring>
#include <cmath>
#include <vector>
#include <thread>
#include <memory>

using namespace std;
using namespace std::chrono;

const uint64_t DEFAULT_UPPER_LIMIT = 10'000'000LLU;

class BitArray {
    uint8_t *array;
    size_t logicalSize;

    static constexpr size_t arraySize(size_t size) 
    {
        return (size >> 3) + ((size & 7) > 0);
    }

    static constexpr size_t index(size_t n) 
    {
        return (n >> 3);
    }

public:
    explicit BitArray(size_t size) : logicalSize(size)
    {
        auto arrSize = (size + 1) / 2; // Only store bits for odd numbers
        array = new uint8_t[arraySize(arrSize)];
        std::memset(array, 0x00, arraySize(arrSize));
    }

    ~BitArray() { delete[] array; }

    constexpr bool get(size_t n) const 
    {
        if (n % 2 == 0)
            return false; // Even numbers > 2 are not prime
        n = n / 2; // Map the actual number to the index in the array
        return !(array[index(n)] & (uint8_t(1) << (n % 8)));
    }

    void set(size_t n)
    {
        n = n / 2; // Map the actual number to the index in the array
        array[index(n)] |= (uint8_t(1) << (n % 8));
    }

    constexpr size_t size() const 
    {
        return logicalSize;
    }
};


// prime_sieve
//
// Represents the data comprising the sieve (an array of bits representing odd numbers starting from 3)
// and includes the code needed to eliminate non-primes from its array by calling runSieve.

class prime_sieve
{
  private:

      BitArray Bits; // Sieve data, where 0==prime, 1==not

   public:

      prime_sieve(uint64_t n) : Bits(n) // Initialize bits to zero default
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
              // Find the next prime number
              for (; factor <= q; factor += 2)
              {
                  if (Bits.get(factor))
                  {
                      break;
                  }
              }

              // Mark multiples of the prime number as not prime
              uint64_t start = factor * factor;
              for (uint64_t num = start; num <= Bits.size(); num += factor * 2)
              {
                  Bits.set(num);
              }

              factor += 2;            
          }
      }

      // countPrimes
      //
      // Can be called after runSieve to determine how many primes were found in total

      size_t countPrimes() const
      {
          size_t count = (Bits.size() >= 2); // Count 2 as prime if within range
          for (uint64_t num = 3; num <= Bits.size(); num += 2)
              if (Bits.get(num))
                  count++;
          return count;
      }

      // isPrime 
      // 
      // Can be called after runSieve to determine whether a given number is prime. 

      bool isPrime(uint64_t n) const
      {
          if (n == 2)
              return true;
          if (n < 2 || n % 2 == 0)
              return false;
          return Bits.get(n);
      }

      // validateResults
      //
      // Checks to see if the number of primes found matches what we should expect. This data isn't used in the
      // sieve processing at all, only to sanity check that the results are right when done.

      bool validateResults() const
      {
          const std::map<const uint64_t, const int> resultsDictionary =
          {
                {             10LLU, 4         }, // Historical data for validating our results - the number of primes
                {            100LLU, 25        }, // to be found under some limit, such as 168 primes under 1000
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

          size_t count = (Bits.size() >= 2); // Count 2 as prime if in range
          for (uint64_t num = 3; num <= Bits.size(); num += 2)
          {
              if (Bits.get(num))
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
               << "Valid: "   << (validateResults() ? "Pass" : "FAIL!") 
               << "\n";

          // Following 2 lines added by rbergen to conform to drag race output format
          cout << "\n";
          cout << "davepl_array;" << passes << ";" << duration << ";" << threads << ";algorithm=base,faithful=yes,bits=1\n";
      }               
  
};

// custom_atoll
//
// Like atoll(), but accepts K, M, G, and T as magnitude suffixes.

long long custom_atoll(const std::string& value_str) {
    static const std::unordered_map<char, long long> suffixes = {
        {'K', 1000LL},
        {'M', 1000000LL},
        {'G', 1000000000LL},
        {'T', 1000000000000LL}
    };

    std::string input_str = value_str;
    for (char& c : input_str) {
        c = std::toupper(c);
    }

    char last_char = input_str.back();
    if (suffixes.find(last_char) != suffixes.end()) {
        long long multiplier = suffixes.at(last_char);
        std::string numeric_part = input_str.substr(0, input_str.size() - 1);
        std::istringstream iss(numeric_part);
        double numeric_value;
        if (!(iss >> numeric_value)) {
            throw std::invalid_argument("Invalid numeric part: " + numeric_part);
        }
        return static_cast<long long>(numeric_value * multiplier);
    }

    std::istringstream iss(input_str);
    long long result;
    if (!(iss >> result)) {
        throw std::invalid_argument("Invalid input format");
    }
    return result;
}

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
            ullLimitRequested = (i == args.end()) ? 0LL : max((long long)1, custom_atoll(i->c_str()));
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
        cout << "Oneshot is on. A single pass will be used to simulate a 5 second run." << endl;

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
            (unsigned long long)llUpperLimit,
            cThreads,
            cThreads == 1 ? "" : "s",
            cSeconds,
            cSeconds == 1 ? "" : "s"
        );
    }
    double duration;

    if (bOneshot)
    {
        auto tStart       = steady_clock::now();
        prime_sieve(llUpperLimit).runSieve();
        auto tEnd = steady_clock::now() - tStart;
        duration = duration_cast<microseconds>(tEnd).count()/1000000.0;
    }
    else
    {
        auto tStart       = steady_clock::now();
        std::vector<std::thread> threads(cThreads);
        std::vector<uint64_t> l_passes(cThreads);
        for (unsigned int i = 0; i < cThreads; i++)
            threads[i] = std::thread([i, &l_passes, &tStart](size_t llUpperLimit)
            {
                l_passes[i] = 0;
                while (duration_cast<seconds>(steady_clock::now() - tStart).count() < 5) {
                    prime_sieve(llUpperLimit).runSieve();
                    ++l_passes[i];
                }
            }, llUpperLimit);
        for (auto i = 0; i < cThreads; i++) {
            threads[i].join();
            cPasses += l_passes[i];
        }
        auto tEnd = steady_clock::now() - tStart;
        duration = duration_cast<microseconds>(tEnd).count()/1000000.0;
    }


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
