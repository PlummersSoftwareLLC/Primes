#include <chrono>
#include <iostream>
#include <map>
#include <numeric>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstring>

#include "algorithms.hpp"
#include "compile_time.hpp"
#include "storages.hpp"
#include "tests.hpp"
#include "utils.hpp"
#include "validator.hpp"

int main()
{
    constexpr auto RUN_TIME = std::chrono::seconds(5);

#ifdef RUN_TESTS
    constexpr auto SIEVE_SIZE = 50'000;
#else
    constexpr auto SIEVE_SIZE = 1'000'000;
#endif

    // clang-format off
    using runners_t = std::tuple<NaiveBase<VectorStorage<bool, true>>,
                                 // NaiveBase<VectorStorage<bool, false>>,
                                 NaiveBase<VectorStorage<std::uint8_t, true>>,
                                 // NaiveBase<VectorStorage<std::uint8_t, false>>,
                                 // NaiveBase<VectorStorage<std::uint16_t, true>>,
                                 // NaiveBase<VectorStorage<std::uint16_t, false>>,
                                 // NaiveBase<VectorStorage<std::uint32_t, true>>,
                                 // NaiveBase<VectorStorage<std::uint32_t, false>>,
                                 // NaiveBase<VectorStorage<std::uint64_t, true>>,
                                 // NaiveBase<VectorStorage<std::uint64_t, false>>,
                                 NaiveBase<BitStorage<std::uint8_t, true>>,
                                 // NaiveBase<BitStorage<std::uint8_t, false>>,
                                 // NaiveBase<BitStorage<std::uint16_t, true>>,
                                 // NaiveBase<BitStorage<std::uint16_t, false>>,
                                 // NaiveBase<BitStorage<std::uint32_t, true>>,
                                 // NaiveBase<BitStorage<std::uint32_t, false>>,
                                 // NaiveBase<BitStorage<std::uint64_t, true>>,
                                 // NaiveBase<BitStorage<std::uint64_t, false>>,
                                 Base<VectorStorage<bool, true>>,
                                 // Base<VectorStorage<bool, false>>,
                                 Base<VectorStorage<std::uint8_t, true>>,
                                 // Base<VectorStorage<std::uint8_t, false>>,
                                 // Base<VectorStorage<std::uint16_t, true>>,
                                 // Base<VectorStorage<std::uint16_t, false>>,
                                 // Base<VectorStorage<std::uint32_t, true>>,
                                 // Base<VectorStorage<std::uint32_t, false>>,
                                 // Base<VectorStorage<std::uint64_t, true>>,
                                 // Base<VectorStorage<std::uint64_t, false>>,
                                 Base<BitStorage<std::uint8_t, true>>,
                                 // Base<BitStorage<std::uint8_t, false>>,
                                 // Base<BitStorage<std::uint16_t, true>>,
                                 // Base<BitStorage<std::uint16_t, false>>,
                                 // Base<BitStorage<std::uint32_t, true>>,
                                 // Base<BitStorage<std::uint32_t, false>>,
                                 // Base<BitStorage<std::uint64_t, true>>,
                                 // Base<BitStorage<std::uint64_t, false>>,
                                 Wheel<VectorStorage<bool>, 0, true, false>,
                                 Wheel<BitStorage<std::uint8_t>, 0, true, false>,
                                 Wheel<VectorStorage<bool>, 1, true, false>,
                                 Wheel<BitStorage<std::uint8_t>, 1, true, false>,
                                 Wheel<VectorStorage<bool>, 2, true, false>,
                                 Wheel<BitStorage<std::uint8_t>, 2, true, false>,
                                 Wheel<VectorStorage<bool>, 3, true, false>,
                                 Wheel<BitStorage<std::uint8_t>, 3, true, false>,
                                 Wheel<VectorStorage<bool>, 4, true, false>,
                                 Wheel<BitStorage<std::uint8_t>, 4, true, false>,
                                 Wheel<VectorStorage<bool>, 5, true, false>,
                                 Wheel<BitStorage<std::uint8_t>, 5, true, false>,
                                 Wheel<VectorStorage<bool>, 6, true, false>,
                                 Wheel<BitStorage<std::uint8_t>, 6, true, false>,
                                 Wheel<VectorStorage<bool>, 7, true, false>,
                                 Wheel<BitStorage<std::uint8_t>, 7, true, false>,
                                 Wheel<VectorStorage<bool>, 0, true, false>,
                                 Wheel<BitStorage<std::uint8_t>, 0, false, false>,
                                 Wheel<VectorStorage<bool>, 1, false, false>,
                                 Wheel<BitStorage<std::uint8_t>, 1, false, false>,
                                 Wheel<VectorStorage<bool>, 2, false, false>,
                                 Wheel<BitStorage<std::uint8_t>, 2, false, false>,
                                 Wheel<VectorStorage<bool>, 3, false, false>,
                                 Wheel<BitStorage<std::uint8_t>, 3, false, false>,
                                 Wheel<VectorStorage<bool>, 4, false, false>,
                                 Wheel<BitStorage<std::uint8_t>, 4, false, false>,
                                 Wheel<VectorStorage<bool>, 5, false, false>,
                                 Wheel<BitStorage<std::uint8_t>, 5, false, false>,
                                 Wheel<VectorStorage<bool>, 6, false, false>,
                                 Wheel<BitStorage<std::uint8_t>, 6, false, false>,
                                 Wheel<VectorStorage<bool>, 7, false, false>,
                                 Wheel<BitStorage<std::uint8_t>, 7, false, false>,
                                 Wheel<VectorStorage<bool>, 1, true, true>,
                                 Wheel<BitStorage<std::uint8_t>, 1, true, true>,
                                 Wheel<VectorStorage<bool>, 2, true, true>,
                                 Wheel<BitStorage<std::uint8_t>, 2, true, true>,
                                 Wheel<VectorStorage<bool>, 3, true, true>,
                                 Wheel<BitStorage<std::uint8_t>, 3, true, true>,
                                 Wheel<VectorStorage<bool>, 4, true, true>,
                                 Wheel<BitStorage<std::uint8_t>, 4, true, true>,
                                 Wheel<VectorStorage<bool>, 5, true, true>,
                                 Wheel<BitStorage<std::uint8_t>, 5, true, true>,
                                 Wheel<VectorStorage<bool>, 6, true, true>,
                                 Wheel<BitStorage<std::uint8_t>, 6, true, true>,
                                 Wheel<VectorStorage<bool>, 7, true, true>,
                                 Wheel<BitStorage<std::uint8_t>, 7, true, true>,
                                 Wheel<VectorStorage<bool>, 1, false, true>,
                                 Wheel<BitStorage<std::uint8_t>, 1, false, true>,
                                 Wheel<VectorStorage<bool>, 2, false, true>,
                                 Wheel<BitStorage<std::uint8_t>, 2, false, true>,
                                 Wheel<VectorStorage<bool>, 3, false, true>,
                                 Wheel<BitStorage<std::uint8_t>, 3, false, true>,
                                 Wheel<VectorStorage<bool>, 4, false, true>,
                                 Wheel<BitStorage<std::uint8_t>, 4, false, true>,
                                 Wheel<VectorStorage<bool>, 5, false, true>,
                                 Wheel<BitStorage<std::uint8_t>, 5, false, true>,
                                 Wheel<VectorStorage<bool>, 6, false, true>,
                                 Wheel<BitStorage<std::uint8_t>, 6, false, true>,
                                 Wheel<VectorStorage<bool>, 7, false, true>,
                                 Wheel<BitStorage<std::uint8_t>, 7, false, true>,
                                 PreGenerated<SIEVE_SIZE>>;
    // clang-format on

#ifdef RUN_TESTS
    return runTests<runners_t, SIEVE_SIZE>();
#else
    utils::for_constexpr(
        [&](const auto& idx) {
            using runner_t = std::tuple_element_t<idx.value, runners_t>;

            auto passes = std::size_t{0};
            const auto start = std::chrono::high_resolution_clock::now();
            while(true) {
                runner_t sieve(SIEVE_SIZE);
                sieve.runSieve();
                ++passes;
                if(const auto end = std::chrono::high_resolution_clock::now(); end - start >= RUN_TIME) {
                    if(!validate(SIEVE_SIZE, sieve.countPrimes())) {
                        std::printf("Error: Results not valid!\n");
                    }
                    else {
                        const auto duration = end - start;
                        const auto durationS = std::chrono::duration_cast<std::chrono::microseconds>(duration).count() / 1'000'000.0;
                        const auto config = sieve.getConfig();
                        std::printf("%s;%lu;%f;%lu;algorithm=%s,faithful=%s,bits=%lu\n", config.name.c_str(), passes, durationS, config.threads,
                                    config.algorithm.c_str(), config.faithful ? "yes" : "no", config.bits);
                    }
                    break;
                }
            }
        },
        std::make_index_sequence<std::tuple_size_v<runners_t>>{});

    return 0;
#endif
}
