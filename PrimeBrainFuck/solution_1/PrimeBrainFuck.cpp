// ---------------------------------------------------------------------------
// PrimeBrainFuck.cpp : An invoker for the BrainFuck implementation
// Mostly copied from the original davepl C++ version
// Modified by ThatAquarel
// ---------------------------------------------------------------------------

#include <cstdlib>
#include <chrono>
#include <sstream>
#include <iostream>
#include <map>
#include <sys/stat.h>

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
        exec_stream << "brainfuck " << bf_source_file << " " << n * 8 + 1024;
        getline(exec_stream, exec_str);
    }

    void runSieve() {
        FILE *fp;
        char path[256];

        fp = popen(exec_str.c_str(), "r");
        if (fp == nullptr) {
            cerr << "Failed to execute command: " << exec_str << endl;
            exit(-1);
        }

        while (fgets(path, sizeof(path), fp) != nullptr) {
            count = stoi(path);
        }

        pclose(fp);
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
    string bf_source_file = "PrimeBrainFuck.b";
    if (argc > 1) bf_source_file = argv[1];

    struct stat buffer{};
    if (stat(bf_source_file.c_str(), &buffer) != 0) {
        cerr << "File \"" << bf_source_file << "\" does not exist." << endl;
        exit(-1);
    }

    int passes = 0;
    auto start_time = steady_clock::now();

    while (true) {
        prime_sieve sieve(1000, bf_source_file);
        sieve.runSieve();
        passes++;

        auto duration = duration_cast<microseconds>(steady_clock::now() - start_time).count();
        if (duration >= 5000000) {
            sieve.printResults((double) duration / 1000000.0, passes);
            break;
        }
    }

    return 0;
}
