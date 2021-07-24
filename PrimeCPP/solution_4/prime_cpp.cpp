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

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Some helper to get the name of the compiler.

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

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// A generic sieve benchmark runner that takes a sieve type and config, and runs it, potentially in parallel.

template<typename Sieve, std::size_t SieveSize, typename Time>
struct Runner {
    inline auto operator()(const Time& runTime, const std::size_t numThreads = 1)
    {
        const auto runThread = [&] {
            auto error = false;
            auto passes = std::size_t{0};
            const auto start = std::chrono::high_resolution_clock::now();
            auto end = start;
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
            return std::tuple{error, passes, start, end, Sieve{}.getConfig()};
        };

        auto runs = std::vector<std::future<std::invoke_result_t<decltype(runThread)>>>{};
        while(runs.size() < numThreads) {
            runs.push_back(std::async(std::launch::async, runThread));
        }

        // Collect the result(s) of the benchmark, this is done through std::async to be compatible with the TestRunner,
        // but isn't done in parallel, contrary to the TestRunner which runs all tests in parallel.
        auto res = std::async([&] {
            auto totalPasses = std::size_t{0};
            auto earliestStart = std::chrono::high_resolution_clock::now();
            auto latestEnd = earliestStart;

            const auto [config, error] = [&] {
                auto error = false;
                for(auto i = std::size_t{0}; i < runs.size(); ++i) {
                    const auto [runError, passes, start, end, cfg] = runs[i].get();
                    error |= runError;
                    totalPasses += passes;
                    earliestStart = std::min(earliestStart, start);
                    latestEnd = std::max(latestEnd, end);
                    if(i + 1 >= runs.size()) {
                        return std::pair{cfg, error};
                    }
                }
                return std::pair{Config{}, true};
            }();

            const auto duration = latestEnd - earliestStart;
            const auto durationS = std::chrono::duration_cast<std::chrono::microseconds>(duration).count() / 1'000'000.0;

            const auto name = config.name + "-" + detail::getCompilerName();
            std::cout << name << ";" << totalPasses << ";" << durationS << ";" << numThreads << ";algorithm=" << config.algorithm
                      << ",faithful=" << (config.faithful ? "yes" : "no") << ",bits=" << config.bits << std::endl;
            return !error;
        });
        res.wait();
        return res;
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Convenience function that starts a runner, potentially in parallel.

template<typename RunnerT, typename Time>
static inline auto parallelRunner(const Time& runTime, const bool parallelize = true)
{
    auto runnerResults = std::vector<std::invoke_result_t<RunnerT, Time, std::size_t>>{};
    if(parallelize) {
        runnerResults.push_back(RunnerT{}(runTime, std::thread::hardware_concurrency()));
    }
    runnerResults.push_back(RunnerT{}(runTime, 1));
    return runnerResults;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Helper to append to a container using move semantics.

static inline void moveAppend(auto& dst, auto&& src)
{
    dst.insert(dst.end(), std::make_move_iterator(src.begin()), std::make_move_iterator(src.end()));
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Function to create and run all possible combinations of optimizations in order to find a set of optimal
// combinations any given system/hardware.

template<std::size_t SieveSize, template<typename, auto, typename> typename RunnerT, typename Time>
static inline auto runAll(const Time& runTime, const bool parallelize = true)
{
    // The possible optimizations.
    constexpr auto wheels = std::tuple{1, 6};
    constexpr auto strides = std::tuple{DynStride::NONE, DynStride::OUTER, DynStride::BOTH};
    constexpr auto sizes = std::tuple{true, false};
    constexpr auto inverts = std::tuple{true, false};
    using types_t = std::tuple<bool, std::uint8_t, std::uint16_t, std::uint32_t, std::uint64_t>;

    auto runnerResults = std::vector<std::future<bool>>{};

    // Nested compile time loop to create the cartesian product of the optimizations above.
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
                                                using vector_sieve_t = GenericSieve<VectorStorage<type_t, inverted>, wheelSize, stride, size>;
                                                using array_sieve_t = GenericSieve<ArrayStorage<type_t, inverted>, wheelSize, stride, size>;
                                                using bit_sieve_t = GenericSieve<BitStorage<type_t, inverted>, wheelSize, stride, size>;
                                                using masked_bit_sieve_t = GenericSieve<MaskedBitStorage<type_t, inverted>, wheelSize, stride, size>;
                                                using strided_bit_sieve_t = GenericSieve<StridedBitStorage<type_t, inverted>, wheelSize, stride, size>;

                                                moveAppend(runnerResults, parallelRunner<RunnerT<vector_sieve_t, SieveSize, Time>>(runTime, parallelize));
                                                moveAppend(runnerResults, parallelRunner<RunnerT<array_sieve_t, SieveSize, Time>>(runTime, parallelize));

                                                // The bit-based sieves cannot use bool, because C++ only allows true/false for bools.
                                                if constexpr(!std::is_same_v<type_t, bool>) {
                                                    moveAppend(runnerResults, parallelRunner<RunnerT<bit_sieve_t, SieveSize, Time>>(runTime, parallelize));
                                                    moveAppend(runnerResults,
                                                               parallelRunner<RunnerT<masked_bit_sieve_t, SieveSize, Time>>(runTime, parallelize));
                                                    moveAppend(runnerResults,
                                                               parallelRunner<RunnerT<strided_bit_sieve_t, SieveSize, Time>>(runTime, parallelize));
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

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Runs tests for every sieve from sieve size 0 up to SIEVE_SIZE.

[[maybe_unused]] static inline auto runTests(const auto& runTime)
{
    using run_time_t = std::remove_cvref_t<decltype(runTime)>;
    constexpr auto SIEVE_SIZE = 50'000;

    // The tests are run in parallel, but not using the parallelism of runAll/parallelRunner as this would run duplicates
    // of the same sieve. Rather the TestRunner internally spawns a thread for every sieve.
    auto res = runAll<SIEVE_SIZE, TestRunner>(runTime, false);
    moveAppend(res, parallelRunner<TestRunner<PreGenerated<SIEVE_SIZE>, SIEVE_SIZE, run_time_t>>(runTime, false));
    return res;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Runs the pre-generated benchmark.

template<std::size_t SieveSize>
[[maybe_unused]] static inline auto runPregen(const auto& runTime)
{
    using run_time_t = std::remove_cvref_t<decltype(runTime)>;
    return parallelRunner<Runner<PreGenerated<SieveSize>, SieveSize, run_time_t>>(runTime);
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Runs the base algorithm benchmarks that were determined to be optimal for Intel/AMD.

template<std::size_t SieveSize>
[[maybe_unused]] static inline auto runBase(const auto& runTime)
{
    using run_time_t = std::remove_cvref_t<decltype(runTime)>;

    auto res = std::vector<std::future<bool>>{};
    // clang-format off
    using base_runners_t = std::tuple<
                                      GenericSieve<StridedBitStorage<std::uint8_t, true>, 1, DynStride::NONE, true>,
                                      GenericSieve<VectorStorage<std::uint8_t, false>, 1, DynStride::BOTH, true>,
                                      GenericSieve<ArrayStorage<bool, true>, 1, DynStride::NONE, true>
                                     >;
    // clang-format on

    utils::for_constexpr(
        [&](const auto idx) {
            using runner_t = std::tuple_element_t<idx.value, base_runners_t>;
            moveAppend(res, parallelRunner<Runner<runner_t, SieveSize, run_time_t>>(runTime));
        },
        std::make_index_sequence<std::tuple_size_v<base_runners_t>>{});

    return res;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Runs the wheel algorithm benchmarks that were determined to be optimal for Intel/AMD.

template<std::size_t SieveSize>
[[maybe_unused]] static inline auto runWheel(const auto& runTime)
{
    using run_time_t = std::remove_cvref_t<decltype(runTime)>;

    auto res = std::vector<std::future<bool>>{};
    // clang-format off
    using wheel_runners_t = std::tuple<
                                       GenericSieve<BitStorage<std::uint32_t, true>, 6, DynStride::OUTER, true>,
                                       GenericSieve<VectorStorage<std::uint8_t, true>, 6, DynStride::OUTER, true>,
                                       GenericSieve<MaskedBitStorage<std::uint32_t, false>, 6, DynStride::OUTER, true>
                                      >;
    // clang-format on

    utils::for_constexpr(
        [&](const auto idx) {
            using runner_t = std::tuple_element_t<idx.value, wheel_runners_t>;
            moveAppend(res, parallelRunner<Runner<runner_t, SieveSize, run_time_t>>(runTime));
        },
        std::make_index_sequence<std::tuple_size_v<wheel_runners_t>>{});

    return res;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Runs the benchmark suite, testing all combinations for optimizations.

template<std::size_t SieveSize>
[[maybe_unused]] static inline auto runSuite(const auto& runTime)
{
    return runAll<SieveSize, Runner>(runTime);
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Launch the benchmark selected by the given pre-processor define.

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
    #elif RUN_BASE
    auto res = runBase<SIEVE_SIZE>(RUN_TIME);
    #elif RUN_WHEEL
    auto res = runWheel<SIEVE_SIZE>(RUN_TIME);
    #endif
#endif

    // Only if no errors occurred does the exit code represent success.
    return std::all_of(res.begin(), res.end(), [](auto& run) { return run.get(); }) ? EXIT_SUCCESS : EXIT_FAILURE;
}
