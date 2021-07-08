#include <algorithm>
#include <chrono>
#include <future>
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

template<typename Sieve, std::size_t SieveSize, typename Time>
struct Runner {
    inline auto operator()(const Time& runTime)
    {
        auto run = std::async([&] {
            auto passes = std::size_t{0};
            const auto start = std::chrono::high_resolution_clock::now();
            while(true) {
                Sieve sieve(SieveSize);
                sieve.runSieve();
                ++passes;
                if(const auto end = std::chrono::high_resolution_clock::now(); end - start >= runTime) {
                    if(!validate(SieveSize, sieve.countPrimes())) {
                        std::printf("Error: Results not valid!\n");
                    }
                    else {
                        const auto duration = end - start;
                        const auto durationS = std::chrono::duration_cast<std::chrono::microseconds>(duration).count() / 1'000'000.0;
                        const auto config = sieve.getConfig();
                        std::printf("%s;%lu;%f;%lu;algorithm=%s,faithful=%s,bits=%lu\n", config.name.c_str(), passes, durationS, config.threads,
                                    config.algorithm.c_str(), config.faithful ? "yes" : "no", config.bits);
                        std::fflush(stdout);
                    }
                    break;
                }
            }
            return passes;
        });
        run.wait();
        return run;
    }
};

template<std::size_t SieveSize, template<typename, auto, typename> typename RunnerT, typename Time>
static inline auto run(const Time& runTime)
{
    constexpr auto wheels = std::tuple{0, 1, 2, 3, 4, 5, 6, 7};
    constexpr auto strides = std::tuple{true, false};
    constexpr auto storages = std::tuple{true, false};
    constexpr auto inverted = std::tuple{true, false};
    using types_t = std::tuple<bool, std::uint8_t, std::uint16_t, std::uint32_t, std::uint64_t>;

    auto runThreads = std::vector<std::future<std::size_t>>{};

    utils::for_constexpr(
        [&](const auto wheelIdx) {
            constexpr auto wheelSize = std::get<wheelIdx.value>(wheels);
            utils::for_constexpr(
                [&](const auto strideIdx) {
                    constexpr auto stride = std::get<strideIdx.value>(strides);
                    utils::for_constexpr(
                        [&](const auto storageIdx) {
                            constexpr auto storage = std::get<storageIdx.value>(storages);
                            utils::for_constexpr(
                                [&](const auto invertedIdx) {
                                    constexpr auto inv = std::get<invertedIdx.value>(inverted);
                                    utils::for_constexpr(
                                        [&](const auto typeIdx) {
                                            if constexpr(!(storage && wheelSize == 0)) {
                                                using type_t = std::tuple_element_t<typeIdx.value, types_t>;
                                                using vector_runner_t = GenericSieve<VectorStorage<type_t, inv>, wheelSize, stride, storage>;
                                                using bit_runner_t = GenericSieve<BitStorage<type_t, inv>, wheelSize, stride, storage>;

                                                runThreads.push_back(RunnerT<vector_runner_t, SieveSize, Time>{}(runTime));

                                                if constexpr(!std::is_same_v<type_t, bool>) {
                                                    runThreads.push_back(RunnerT<bit_runner_t, SieveSize, Time>{}(runTime));
                                                }
                                            }
                                        },
                                        std::make_index_sequence<std::tuple_size_v<types_t>>{});
                                },
                                std::make_index_sequence<std::tuple_size_v<decltype(inverted)>>{});
                        },
                        std::make_index_sequence<std::tuple_size_v<decltype(storages)>>{});
                },
                std::make_index_sequence<std::tuple_size_v<decltype(strides)>>{});
        },
        std::make_index_sequence<std::tuple_size_v<decltype(wheels)>>{});

    return std::all_of(runThreads.begin(), runThreads.end(), [](auto& runFuture) { return runFuture.get(); }) ? EXIT_SUCCESS : EXIT_FAILURE;
}

int main()
{
    constexpr auto RUN_TIME = std::chrono::seconds(5);

#ifdef RUN_TESTS
    return run<50'000, TestRunner>(RUN_TIME);
#else
    constexpr auto SIEVE_SIZE = 1'000'000;
    #ifdef RUN_PREGEN
    return Runner<PreGenerated<SIEVE_SIZE>, SIEVE_SIZE, decltype(RUN_TIME)>{}(RUN_TIME).get() ? EXIT_SUCCESS : EXIT_FAILURE;
    #else
    return run<SIEVE_SIZE, Runner>(RUN_TIME);
    #endif
#endif
}
