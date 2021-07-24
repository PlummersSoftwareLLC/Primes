#pragma once

#include <algorithm>
#include <future>
#include <iostream>
#include <tuple>
#include <utility>

#include <cstdlib>

#include "compile_time.hpp"
#include "utils.hpp"

namespace detail {

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Trivial isPrime function to test correctness of sieves.

static inline auto isPrime(const std::size_t& num)
{
    if(num <= 1) {
        return false;
    }
    for(auto i = std::size_t{2}; i <= num / 2; ++i) {
        if(num % i == 0) {
            return false;
        }
    }
    return true;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Calculates reference primes to compare sieve results against.

static inline auto calcReferencePrimes(const std::size_t sieveSize)
{
    auto primes = std::vector<std::size_t>{};
    for(auto i = std::size_t{0}; i <= sieveSize; ++i) {
        if(isPrime(i)) {
            primes.push_back(i);
        }
    }
    std::cout << "Computed reference primes" << std::endl;
    return primes;
}

static inline auto& getReferencePrimes(const std::size_t sieveSize)
{
    static const auto s_referencePrimes = detail::calcReferencePrimes(sieveSize);
    return s_referencePrimes;
}

} // namespace detail

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Compares results and prints what and where errors occurred.

static inline auto compareResults(const auto& name, const auto& computed, const auto& reference)
{
    auto error = false;
    if(computed.size() != reference.size()) {
        std::cout << name << ": Error: Size mismatch: Computed " << computed.size() << " primes, but should be " << reference.size() << std::endl;
        error = true;
    }

    for(auto i = std::size_t{0}; i < std::min(computed.size(), reference.size()); ++i) {
        if(computed[i] != reference[i]) {
            std::cout << name << ": Error: prime #" << i << " mismatch: " << computed[i] << " != " << reference[i] << std::endl;
            error = true;
        }
    }
    return !error;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Runner that allows sieves to be tested in parallel, using the same interface the benchmark uses.

template<typename Sieve, std::size_t SieveSize, typename Time>
struct TestRunner {
    inline auto operator()(const Time&, const std::size_t)
    {
        constexpr auto limitPrimes = [](const auto& primes, const auto& limit) {
            auto limitedPrimes = std::vector<std::size_t>{};
            std::copy_if(primes.begin(), primes.end(), std::back_inserter(limitedPrimes), [&](const auto& val) { return val <= limit; });
            return limitedPrimes;
        };

        return std::async(std::launch::async, [&limitPrimes]() {
            auto sieveName = std::string{};
            const auto& referencePrimes = detail::getReferencePrimes(SieveSize);
            auto error = false;
            for(auto i = std::size_t{0}; i <= SieveSize; ++i) {
                Sieve sieve(i);
                sieve.runSieve();
                const auto sievedPrimes = sieve.getPrimes();
                const auto refPrimes = limitPrimes(referencePrimes, i);
                if(!compareResults(sieve.getConfig().name, sievedPrimes, refPrimes)) {
                    error = true;
                }
                sieveName = sieve.getConfig().name;
            }
            if(!error) {
                std::cout << sieveName << ": Success" << std::endl;
            }
            return !error;
        });
    }
};
