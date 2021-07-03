#include <chrono>
#include <map>
#include <vector>

#include <cstddef>
#include <cstdio>

class PrimeSieve {
  public:
    PrimeSieve(const std::size_t sieveSize) : m_sieveSize(sieveSize), m_bits(sieveSize / 2 + 1, true) {}

    ~PrimeSieve() {}

    void runSieve()
    {
        for(auto i = std::size_t{1}; i * i <= m_sieveSize; ++i) {
            for(auto num = i; num < m_sieveSize / 2; ++num) {
                if(m_bits[num]) {
                    i = num;
                    break;
                }
            }
            const auto factor = i * 2 + 1;
            for(auto num = factor * factor / 2; num < m_sieveSize / 2; num += factor)
                m_bits[num] = false;
        }
    }

    void printResults(double duration, int passes)
    {
        if(!validateResults())
            std::printf("Error: Results not valid!\n");

        std::printf("BlackMark;%d;%f;1;algorithm=base,faithful=yes,bits=1\n", passes, duration);
    }

    std::size_t countPrimes()
    {
        auto count = std::size_t{1};
        for(auto i = std::size_t{1}; i < m_sieveSize / 2; ++i)
            if(m_bits[i])
                ++count;
        return count;
    }

  private:
    const std::size_t m_sieveSize = 0;
    std::vector<bool> m_bits;

    // Historical data for validating our results - the number of primes to be found under some limit, such as 168 primes under 1000
    static const std::map<const std::size_t, const std::size_t> m_resultsDictionary;

    bool validateResults()
    {
        auto result = m_resultsDictionary.find(m_sieveSize);
        if(m_resultsDictionary.end() == result)
            return false;
        return result->second == countPrimes();
    }
};

// clang-format off
const std::map<const std::size_t, const std::size_t> PrimeSieve::m_resultsDictionary = {
    {            10,           4},
    {           100,          25},
    {         1'000,         168},
    {        10'000,       1'229},
    {       100'000,       9'592},
    {     1'000'000,      78'498},
    {    10'000'000,     664'579},
    {   100'000'000,   5'761'455},
    { 1'000'000'000,  50'847'534},
    {10'000'000'000, 455'052'511},
};
// clang-format on

int main()
{
    auto passes = 0;
    const auto start = std::chrono::high_resolution_clock::now();

    while(true) {
        PrimeSieve sieve(1'000'000);
        sieve.runSieve();
        ++passes;
        if(const auto end = std::chrono::high_resolution_clock::now(); std::chrono::duration_cast<std::chrono::seconds>(end - start).count() >= 5) {
            sieve.printResults(std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() / 1'000'000.0, passes);
            break;
        }
    }
}
