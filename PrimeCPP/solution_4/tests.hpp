#pragma once

#include <algorithm>
#include <future>
#include <iostream>
#include <tuple>
#include <utility>

#include <cstdlib>

#include "compile_time.hpp"
#include "utils.hpp"

static inline auto compareResults(const auto& name, const auto& computed, const auto& preComputed)
{
    auto error = false;
    if(computed.size() != preComputed.size()) {
        std::cout << name << ": Error: Size mismatch: Computed " << computed.size() << " primes, but should be " << preComputed.size() << std::endl;
        error = true;
    }

    for(auto i = std::size_t{0}; i < std::min(computed.size(), preComputed.size()); ++i) {
        if(computed[i] != preComputed[i]) {
            std::cout << name << ": Error: prime #" << i << " mismatch: " << computed[i] << " != " << preComputed[i] << std::endl;
            error = true;
        }
    }
    return !error;
}

template<typename Runners, std::size_t SieveSize>
static inline auto runTests()
{
    constexpr auto limitPrimes = [](const auto& primes, const auto& limit) {
        auto limitedPrimes = std::vector<std::size_t>{};
        std::copy_if(primes.begin(), primes.end(), std::back_inserter(limitedPrimes), [&](const auto& val) { return val <= limit; });
        return limitedPrimes;
    };
    constexpr auto primes = genPrimes<SieveSize>();

    auto runFutures = utils::for_constexpr(
        [&](const auto& idx) {
            return std::async(std::launch::async, [idx, &limitPrimes, &primes]() {
                using runner_t = std::tuple_element_t<idx.value, Runners>;

                auto error = false;
                for(auto i = std::size_t{0}; i < SieveSize; ++i) {
                    runner_t sieve(i);
                    sieve.runSieve();
                    const auto sievedPrimes = sieve.getPrimes();
                    const auto preComputedPrimes = limitPrimes(primes, i);
                    if(!compareResults(sieve.getConfig().name, sievedPrimes, preComputedPrimes)) {
                        error = true;
                    }
                }
                if(!error) {
                    std::cout << runner_t{0}.getConfig().name << ": Success" << std::endl;
                }
                return !error;
            });
        },
        std::make_index_sequence<std::tuple_size_v<Runners>>{});

    return utils::for_constexpr([](auto& runFuture) { return runFuture.get(); }, runFutures) ? EXIT_SUCCESS : EXIT_FAILURE;
}
