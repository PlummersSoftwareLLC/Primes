#include <cstdlib>
#include <chrono>
#include <sstream>
#include <iostream>
#include <map>

using namespace std;
using namespace std::chrono;

class prime_sieve {
private:
    long sieve_size = 0;
    int count = 0;
    string exec_str;

    const std::map<const long long, const int> resultsDictionary =
            {
                    {10LL,          4},
                    {100LL,         25},
                    {1000LL,        168},
                    {10000LL,       1229},
                    {100000LL,      9592},
                    {1000000LL,     78498},
                    {10000000LL,    664579},
                    {100000000LL,   5761455},
                    {1000000000LL,  50847534},
                    {10000000000LL, 455052511},
            };

    bool validateResults() {
        auto result = resultsDictionary.find(sieve_size);
        if (resultsDictionary.end() == result)
            return false;
        return result->second == count;
    }

public:
    explicit prime_sieve(long n, string &bf_source_file) {
        sieve_size = n;

        stringstream exec_stream;
        exec_stream << "brainfuck " << bf_source_file << " " << n + 128;
        getline(exec_stream, exec_str);
    }

    ~prime_sieve() = default;

    void runSieve() {
        int exit_status = system(exec_str.c_str());
        count = WEXITSTATUS(exit_status);
    }

    void printResults(double duration, int passes) {
        printf("Passes: %d, Time: %lf, Avg: %lf, Limit: %ld, Count1: %d, Valid: %d\n",
               passes,
               duration,
               duration / passes,
               sieve_size,
               count,
               validateResults());

        printf("\n");
        printf("aquarel;%d;%f;1;algorithm=base,faithful=no,bits=32\n", passes, duration);
    }
};

int main(int argc, char *argv[]) {
    long upper_limit = 1'000'000L;
    string bf_source_file = "PrimeBrainFuck.b";

    if (argc > 1) upper_limit = max((long) strtol(argv[1], nullptr, 10), (long) 1);
    if (argc > 2) bf_source_file = argv[2];

    prime_sieve sieve(upper_limit, bf_source_file);

    int passes = 0;
    auto start_time = steady_clock::now();

    sieve.runSieve();
    sieve.printResults(1.0, 1);

//    while (true) {
//        sieve.runSieve();
//        passes++;
//
//        auto duration = duration_cast<microseconds>(steady_clock::now() - start_time).count();
//        if (duration >= 5000000) {
//            sieve.printResults((double) duration / 1000000.0, passes);
//            break;
//        }
//    }

    return 0;
}
