#include <algorithm>
#include <chrono>
#include <future>
#include <iostream>
#include <map>
#include <numeric>
#include <string>
#include <thread>
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

namespace detail {
[[maybe_unused]] static inline auto getCompilerName()
{
#if defined(__GNUG__) && !defined(__clang__)
    return "g++";
#elif defined(__clang__)
    return "clang++";
#else
    return "unknown";
#endif
}
} // namespace detail

template<typename Sieve, std::size_t SieveSize, typename Time>
struct Runner {
    inline auto operator()(const Time& runTime, const std::size_t numThreads = 1)
    {
        const auto start = std::chrono::high_resolution_clock::now();

        const auto runThread = [&] {
            auto error = false;
            auto passes = std::size_t{0};
            auto end = std::chrono::high_resolution_clock::now();
            while(true) {
                Sieve sieve(SieveSize);
                sieve.runSieve();
                ++passes;
                if(end = std::chrono::high_resolution_clock::now(); end - start >= runTime) {
                    if(!validate(SieveSize, sieve.countPrimes())) {
                        std::printf("Error: Results not valid!\n");
                        error = true;
                    }
                    break;
                }
            }
            return std::tuple{error, passes, end, Sieve{}.getConfig()};
        };

        auto runs = std::vector<std::future<std::invoke_result_t<decltype(runThread)>>>{};
        while(runs.size() < numThreads) {
            runs.push_back(std::async(std::launch::async, runThread));
        }

        auto res = std::async([&] {
            auto totalPasses = std::size_t{0};
            auto latestEnd = std::chrono::high_resolution_clock::now();

            const auto [config, error] = [&] {
                auto error = false;
                for(auto i = std::size_t{0}; i < runs.size(); ++i) {
                    const auto [runError, passes, end, cfg] = runs[i].get();
                    error |= runError;
                    totalPasses += passes;
                    latestEnd = std::max(latestEnd, end);
                    if(i + 1 >= runs.size()) {
                        return std::pair{cfg, error};
                    }
                }
                return std::pair{Config{}, true};
            }();

            const auto duration = latestEnd - start;
            const auto durationS = std::chrono::duration_cast<std::chrono::microseconds>(duration).count() / 1'000'000.0;

            const auto name = config.name + "-" + detail::getCompilerName();
            std::printf("%s;%lu;%f;%lu;algorithm=%s,faithful=%s,bits=%lu\n", name.c_str(), totalPasses, durationS, numThreads, config.algorithm.c_str(),
                        config.faithful ? "yes" : "no", config.bits);
            std::fflush(stdout);
            return !error;
        });
        res.wait();
        return res;
    }
};

template<typename RunnerT, typename Time>
static inline auto parallelRunner(const Time& runTime, const bool parallelize = true)
{
    auto runnerResults = std::vector<std::invoke_result_t<RunnerT, Time, std::size_t>>{};
    const auto threads = parallelize ? std::thread::hardware_concurrency() : 1;

    for(auto numThreads = threads; numThreads >= 1; numThreads /= 2) {
        runnerResults.push_back(RunnerT{}(runTime, numThreads));
    }
    return runnerResults;
}

static inline void moveAppend(auto& dst, auto&& src)
{
    dst.insert(dst.end(), std::make_move_iterator(src.begin()), std::make_move_iterator(src.end()));
}

template<std::size_t SieveSize, template<typename, auto, typename> typename RunnerT, typename Time>
static inline auto run(const Time& runTime, const bool parallelize = true)
{
    constexpr auto wheels = std::tuple{0, 1, 2, 3, 4, 5, 6, 7};
    constexpr auto strides = std::tuple{true, false};
    constexpr auto storages = std::tuple{true, false};
    constexpr auto inverted = std::tuple{true, false};
    using types_t = std::tuple<bool, std::uint8_t, std::uint16_t, std::uint32_t, std::uint64_t>;

    auto runnerResults = std::vector<std::future<bool>>{};

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

                                                moveAppend(runnerResults, parallelRunner<RunnerT<vector_runner_t, SieveSize, Time>>(runTime, parallelize));

                                                if constexpr(!std::is_same_v<type_t, bool>) {
                                                    moveAppend(runnerResults, parallelRunner<RunnerT<bit_runner_t, SieveSize, Time>>(runTime, parallelize));
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

    return runnerResults;
}

[[maybe_unused]] static inline auto runTests(const auto& runTime)
{
    using run_time_t = std::remove_cvref_t<decltype(runTime)>;
    constexpr auto SIEVE_SIZE = 50'000;

    auto res = run<SIEVE_SIZE, TestRunner>(runTime, false);
    moveAppend(res, parallelRunner<TestRunner<PreGenerated<SIEVE_SIZE>, SIEVE_SIZE, run_time_t>>(runTime, false));
    return res;
}

template<std::size_t SieveSize>
[[maybe_unused]] static inline auto runPregen(const auto& runTime)
{
    using run_time_t = std::remove_cvref_t<decltype(runTime)>;
    return parallelRunner<Runner<PreGenerated<SieveSize>, SieveSize, run_time_t>>(runTime);
}

template<std::size_t SieveSize>
[[maybe_unused]] static inline auto runBenchmark(const auto& runTime)
{
    using run_time_t = std::remove_cvref_t<decltype(runTime)>;

    auto res = std::vector<std::future<bool>>{};
    // clang-format off
    using runners_t = std::tuple<
                                 GenericSieve<VectorStorage<bool>, 1, true, true>,
                                 GenericSieve<VectorStorage<bool>, 1, true, false>,
                                 GenericSieve<VectorStorage<std::uint8_t>, 1, true, true>,
                                 GenericSieve<VectorStorage<std::uint8_t>, 1, true, false>,
                                 GenericSieve<VectorStorage<std::uint16_t>, 1, true, true>,
                                 GenericSieve<VectorStorage<std::uint16_t>, 1, true, false>,
                                 GenericSieve<VectorStorage<std::uint32_t>, 1, true, true>,
                                 GenericSieve<VectorStorage<std::uint32_t>, 1, true, false>,
                                 GenericSieve<VectorStorage<std::uint64_t>, 1, true, true>,
                                 GenericSieve<VectorStorage<std::uint64_t>, 1, true, false>,
                                 GenericSieve<BitStorage<std::uint8_t>, 1, true, true>,
                                 GenericSieve<BitStorage<std::uint8_t>, 1, true, false>,
                                 GenericSieve<BitStorage<std::uint16_t>, 1, true, true>,
                                 GenericSieve<BitStorage<std::uint16_t>, 1, true, false>,
                                 GenericSieve<BitStorage<std::uint32_t>, 1, true, true>,
                                 GenericSieve<BitStorage<std::uint32_t>, 1, true, false>,
                                 GenericSieve<BitStorage<std::uint64_t>, 1, true, true>,
                                 GenericSieve<BitStorage<std::uint64_t>, 1, true, false>,
                                 GenericSieve<VectorStorage<bool>, 1, false, true>,
                                 GenericSieve<VectorStorage<bool>, 1, false, false>,
                                 GenericSieve<VectorStorage<std::uint8_t>, 1, false, true>,
                                 GenericSieve<VectorStorage<std::uint8_t>, 1, false, false>,
                                 GenericSieve<VectorStorage<std::uint16_t>, 1, false, true>,
                                 GenericSieve<VectorStorage<std::uint16_t>, 1, false, false>,
                                 GenericSieve<VectorStorage<std::uint32_t>, 1, false, true>,
                                 GenericSieve<VectorStorage<std::uint32_t>, 1, false, false>,
                                 GenericSieve<VectorStorage<std::uint64_t>, 1, false, true>,
                                 GenericSieve<VectorStorage<std::uint64_t>, 1, false, false>,
                                 GenericSieve<BitStorage<std::uint8_t>, 1, false, true>,
                                 GenericSieve<BitStorage<std::uint8_t>, 1, false, false>,
                                 GenericSieve<BitStorage<std::uint16_t>, 1, false, true>,
                                 GenericSieve<BitStorage<std::uint16_t>, 1, false, false>,
                                 GenericSieve<BitStorage<std::uint32_t>, 1, false, true>,
                                 GenericSieve<BitStorage<std::uint32_t>, 1, false, false>,
                                 GenericSieve<BitStorage<std::uint64_t>, 1, false, true>,
                                 GenericSieve<BitStorage<std::uint64_t>, 1, false, false>
                                >;
    // clang-format on

    utils::for_constexpr(
        [&](const auto idx) {
            using runner_t = std::tuple_element_t<idx.value, runners_t>;
            moveAppend(res, parallelRunner<Runner<runner_t, SieveSize, run_time_t>>(runTime));
        },
        std::make_index_sequence<std::tuple_size_v<runners_t>>{});

    return res;
}

template<std::size_t SieveSize>
[[maybe_unused]] static inline auto runSuite(const auto& runTime)
{
    return run<SieveSize, Runner>(runTime, false);
}

int main()
{
    constexpr auto RUN_TIME = std::chrono::seconds(5);

#ifdef RUN_TESTS
    auto res = runTests(RUN_TIME);
#else
    constexpr auto SIEVE_SIZE = 1'000'000;
    #ifdef RUN_PREGEN
    auto res = runPregen<SIEVE_SIZE>(RUN_TIME);
    #elif RUN_SUITE
    auto res = runSuite<SIEVE_SIZE>(RUN_TIME);
    #else
    auto res = runBenchmark<SIEVE_SIZE>(RUN_TIME);
    #endif
#endif

    return std::all_of(res.begin(), res.end(), [](auto& run) { return run.get(); }) ? EXIT_SUCCESS : EXIT_FAILURE;
}
