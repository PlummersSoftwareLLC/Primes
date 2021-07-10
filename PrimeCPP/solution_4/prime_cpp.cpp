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
#include <cstring>

#include "algorithms.hpp"
#include "compile_time.hpp"
#include "storages.hpp"
#include "tests.hpp"
#include "utils.hpp"
#include "validator.hpp"

#if defined(__GNUG__) && !defined(__clang__)
    #define COMPILER_GCC
#elif defined(__clang__)
    #define COMPILER_CLANG
#endif

namespace detail {
[[maybe_unused]] static inline auto getCompilerName()
{
#if defined(COMPILER_GCC)
    return "gcc";
#elif defined(COMPILER_CLANG)
    return "clang";
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
                        std::cout << "Error: Results not valid!" << std::endl;
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
            std::cout << name << ";" << totalPasses << ";" << durationS << ";" << numThreads << ";algorithm=" << config.algorithm
                      << ";faithful=" << (config.faithful ? "yes" : "no") << ";bits=" << config.bits << std::endl;
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
static inline auto runAll(const Time& runTime, const bool parallelize = true)
{
    constexpr auto wheels = std::tuple{0, 1, 2, 3, 4, 5, 6, 7};
    constexpr auto strides = std::tuple{DynStride::NONE, DynStride::OUTER, DynStride::BOTH};
    constexpr auto sizes = std::tuple{true, false};
    constexpr auto inverts = std::tuple{true, false};
    using types_t = std::tuple<bool, std::uint8_t, std::uint16_t, std::uint32_t, std::uint64_t>;

    auto runnerResults = std::vector<std::future<bool>>{};

    utils::for_constexpr(
        [&](const auto wheelIdx) {
            constexpr auto wheelSize = std::get<wheelIdx.value>(wheels);
            utils::for_constexpr(
                [&](const auto strideIdx) {
                    constexpr auto stride = std::get<strideIdx.value>(strides);
                    utils::for_constexpr(
                        [&](const auto sizeIdx) {
                            constexpr auto size = std::get<sizeIdx.value>(sizes);
                            utils::for_constexpr(
                                [&](const auto invertIdx) {
                                    constexpr auto inverted = std::get<invertIdx.value>(inverts);
                                    utils::for_constexpr(
                                        [&](const auto typeIdx) {
                                            if constexpr(!(size && wheelSize == 0)) {
                                                using type_t = std::tuple_element_t<typeIdx.value, types_t>;
                                                using vector_runner_t = GenericSieve<VectorStorage<type_t, inverted>, wheelSize, stride, size>;
                                                using bit_runner_t = GenericSieve<BitStorage<type_t, inverted>, wheelSize, stride, size>;

                                                moveAppend(runnerResults, parallelRunner<RunnerT<vector_runner_t, SieveSize, Time>>(runTime, parallelize));

                                                if constexpr(!std::is_same_v<type_t, bool>) {
                                                    moveAppend(runnerResults, parallelRunner<RunnerT<bit_runner_t, SieveSize, Time>>(runTime, parallelize));
                                                }
                                            }
                                        },
                                        std::make_index_sequence<std::tuple_size_v<types_t>>{});
                                },
                                std::make_index_sequence<std::tuple_size_v<decltype(inverts)>>{});
                        },
                        std::make_index_sequence<std::tuple_size_v<decltype(sizes)>>{});
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

    auto res = runAll<SIEVE_SIZE, TestRunner>(runTime, false);
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
                                 GenericSieve<VectorStorage<bool>, 6, DynStride::NONE, true>,
                                 GenericSieve<VectorStorage<bool>, 6, DynStride::NONE, false>,
                                 GenericSieve<VectorStorage<bool>, 6, DynStride::OUTER, true>,
                                 GenericSieve<VectorStorage<bool>, 6, DynStride::OUTER, false>,
                                 GenericSieve<VectorStorage<bool>, 6, DynStride::BOTH, true>,
                                 GenericSieve<VectorStorage<bool>, 6, DynStride::BOTH, false>,
                                 GenericSieve<BitStorage<std::uint32_t>, 6, DynStride::NONE, true>,
                                 GenericSieve<BitStorage<std::uint32_t>, 6, DynStride::NONE, false>,
                                 GenericSieve<BitStorage<std::uint32_t>, 6, DynStride::OUTER, true>,
                                 GenericSieve<BitStorage<std::uint32_t>, 6, DynStride::OUTER, false>,
                                 GenericSieve<BitStorage<std::uint32_t>, 6, DynStride::BOTH, true>,
                                 GenericSieve<BitStorage<std::uint32_t>, 6, DynStride::BOTH, false>,
                                 GenericSieve<BitStorage<std::uint64_t>, 6, DynStride::NONE, true>,
                                 GenericSieve<BitStorage<std::uint64_t>, 6, DynStride::NONE, false>,
                                 GenericSieve<BitStorage<std::uint64_t>, 6, DynStride::OUTER, true>,
                                 GenericSieve<BitStorage<std::uint64_t>, 6, DynStride::OUTER, false>,
                                 GenericSieve<BitStorage<std::uint64_t>, 6, DynStride::BOTH, true>,
                                 GenericSieve<BitStorage<std::uint64_t>, 6, DynStride::BOTH, false>
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
    return runAll<SieveSize, Runner>(runTime);
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
