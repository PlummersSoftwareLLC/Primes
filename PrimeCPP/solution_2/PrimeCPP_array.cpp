// ---------------------------------------------------------------------------
// Optimized PrimeCPP.cpp
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

class BitArray 
{
    uint8_t* _byteArray;
    size_t _numberOfBits;
    size_t _numberOfIndices;  // Number of indices (odd numbers up to _numberOfBits)

    static constexpr size_t arraySizeInBytes(size_t numberOfBits) 
    {
        return (numberOfBits >> 3) + ((numberOfBits & 7) > 0);
    }

    static constexpr size_t byteIndexOfBit(size_t n) 
    {
        return (n >> 3);
    }

public:
    explicit BitArray(size_t size) 
        : _numberOfBits(size), _numberOfIndices((size + 1) / 2)
    {
        size_t arrSizeInBytes = arraySizeInBytes(_numberOfIndices);
        _byteArray = new uint8_t[arrSizeInBytes];
        std::memset(_byteArray, 0x00, arrSizeInBytes);
    }

    ~BitArray() 
    { 
        delete[] _byteArray; 
    }

    // Methods that take index directly
    inline bool get_index(size_t index) const 
    {
        return !(_byteArray[byteIndexOfBit(index)] & (uint8_t(1) << (index % 8)));
    }

    inline void set_index(size_t index)
    {
        _byteArray[byteIndexOfBit(index)] |= (uint8_t(1) << (index % 8));
    }

    size_t numberOfBits() const 
    {
        return _numberOfBits;
    }

    size_t numberOfIndices() const
    {
        return _numberOfIndices;
    }
};

class prime_sieve
{
private:
    BitArray Bits; // Sieve data, where 0 == prime, 1 == not

public:
    prime_sieve(uint64_t n) : Bits(n) {}

    void runSieve()
    {
        size_t q = (size_t)std::sqrt(Bits.numberOfBits());
        size_t q_index = q / 2;

        size_t factor_index = 1; // Index for number 3
        size_t factor = factor_index * 2 + 1;

        while (factor_index <= q_index)
        {
            // If the number at factor_index is prime
            if (Bits.get_index(factor_index))
            {
                factor = factor_index * 2 + 1;
                size_t start_index = (factor * factor) / 2;

                for (size_t num_index = start_index; num_index < Bits.numberOfIndices(); num_index += factor)
                {
                    Bits.set_index(num_index);
                }
            }
            ++factor_index;
        }
    }

    size_t countPrimes() const
    {
        size_t count = (Bits.numberOfBits() >= 2) ? 1 : 0; // Count 2 as prime if within range
        for (size_t index = 1; index < Bits.numberOfIndices(); ++index)
        {
            if (Bits.get_index(index))
                ++count;
        }
        return count;
    }

    bool isPrime(uint64_t n) const
    {
        if (n == 2)
            return true;
        if (n < 2 || n % 2 == 0)
            return false;
        size_t index = n / 2;
        if (index < Bits.numberOfIndices())
            return Bits.get_index(index);
        else
            return false;
    }

    bool validateResults() const
    {
        const std::map<const uint64_t, const int> resultsDictionary =
        {
            {             10LLU, 4         },
            {            100LLU, 25        },
            {          1'000LLU, 168       },
            {         10'000LLU, 1229      },
            {        100'000LLU, 9592      },
            {      1'000'000LLU, 78498     },
            {     10'000'000LLU, 664579    },
            {    100'000'000LLU, 5761455   },
            {  1'000'000'000LLU, 50847534  },
            { 10'000'000'000LLU, 455052511 },
        };
        auto it = resultsDictionary.find(Bits.numberOfBits());
        if (it != resultsDictionary.end())
            return it->second == countPrimes();
        else
            return false;
    }

    void printResults(bool showResults, double duration, size_t passes, size_t threads) const
    {
        if (showResults && Bits.numberOfBits() >= 2)
            std::cout << "2, ";

        for (size_t index = 1; index < Bits.numberOfIndices(); ++index)
        {
            if (Bits.get_index(index))
            {
                if (showResults)
                    std::cout << (index * 2 + 1) << ", ";
            }
        }

        if (showResults)
            std::cout << "\n";

        size_t count = countPrimes();

        std::cout << "Passes: "  << passes << ", "
                  << "Threads: " << threads << ", "
                  << "Time: "    << duration << ", " 
                  << "Average: " << duration / passes << ", "
                  << "Limit: "   << Bits.numberOfBits() << ", "
                  << "Counts: "  << count << "/" << count << ", "
                  << "Valid: "   << (validateResults() ? "Pass" : "FAIL!") 
                  << "\n";

        // Output format conforming to drag race output format
        std::cout << "\n";
        std::cout << "davepl_array;" << passes << ";" << duration << ";" << threads << ";algorithm=base,faithful=yes,bits=1\n";
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
    if (suffixes.find(last_char) != suffixes.end()) 
    {
        long long multiplier = suffixes.at(last_char);
        std::string numeric_part = input_str.substr(0, input_str.size() - 1);
        std::istringstream iss(numeric_part);
        double numeric_value;
        if (!(iss >> numeric_value)) 
            throw std::invalid_argument("Invalid numeric part: " + numeric_part);
        
        return static_cast<long long>(numeric_value * multiplier);
    }

    std::istringstream iss(input_str);
    long long result;
    if (!(iss >> result)) 
        throw std::invalid_argument("Invalid input format");
    
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
            ullLimitRequested = (i == args.end()) ? 0LL : max((long long)1, custom_atoll(*i));
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
            threads[i] = std::thread([i, &l_passes, &tStart, cSeconds](size_t llUpperLimit)
            {
                l_passes[i] = 0;
                while (duration_cast<seconds>(steady_clock::now() - tStart).count() < cSeconds) 
                {
                    prime_sieve sieve(llUpperLimit);
                    sieve.runSieve();
                    ++l_passes[i];
                }
            }, llUpperLimit);
        for (auto i = 0; i < cThreads; i++) 
        {
            threads[i].join();
            cPasses += l_passes[i];
        }
        auto tEnd = steady_clock::now() - tStart;
        duration = duration_cast<microseconds>(tEnd).count()/1000000.0;
    }

    if (bOneshot)
    {
        cPasses = static_cast<size_t>(1.0 / duration * 5);
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
