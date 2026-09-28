// ---------------------------------------------------------------------------
// PrimeCPP.cpp : High-performance 1-bit Wheel 8-of-30 Sieve of Eratosthenes
// Author: ndt0208
// Compliant with Dave's Garage Drag Race "faithful" rules.
// ---------------------------------------------------------------------------

#include <chrono>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <thread>
#include <vector>

#if defined(_WIN32)
#  include <malloc.h>
#endif

#if defined(__GNUC__) || defined(__clang__)
#  define ATTR_ALWAYS_INLINE __attribute__((always_inline)) inline
#  pragma GCC optimize("O3,unroll-loops")
#  if defined(__x86_64__) || defined(_M_X64)
#    pragma GCC target("avx2,bmi2,popcnt")
#  endif
#elif defined(_MSC_VER)
#  define ATTR_ALWAYS_INLINE __forceinline
#else
#  define ATTR_ALWAYS_INLINE inline
#endif

// Pre-halved steps for the 8-of-30 wheel (numbers coprime to 2, 3, 5):
// The 8 candidates per 30 numbers are: 1, 7, 11, 13, 17, 19, 23, 29.
// Halved steps: 3, 2, 1, 2, 1, 2, 3, 1 (sum = 15).
static constexpr unsigned int STEPS[8] = { 3, 2, 1, 2, 1, 2, 3, 1 };

class PrimeSieve
{
private:
    size_t m_sieveSize;
    size_t m_numWords;
    uint64_t* m_bits;

public:
    // Dynamically allocates the candidate buffer at runtime conforming to faithful rules
    explicit PrimeSieve(size_t size) : m_sieveSize(size)
    {
        const size_t numOdds = size >> 1;
        m_numWords = (numOdds + 63) >> 6;

#if defined(_WIN32)
        m_bits = static_cast<uint64_t*>(_aligned_malloc(m_numWords * sizeof(uint64_t), 64));
#else
        void* p = nullptr;
        if (posix_memalign(&p, 64, m_numWords * sizeof(uint64_t)) == 0 && p != nullptr) {
            m_bits = static_cast<uint64_t*>(p);
        } else {
            m_bits = static_cast<uint64_t*>(std::malloc(m_numWords * sizeof(uint64_t)));
        }
#endif
        std::memset(m_bits, 0, m_numWords * sizeof(uint64_t));
    }

    ~PrimeSieve()
    {
#if defined(_WIN32)
        if (m_bits) _aligned_free(m_bits);
#else
        if (m_bits) std::free(m_bits);
#endif
    }

    PrimeSieve(const PrimeSieve&) = delete;
    PrimeSieve& operator=(const PrimeSieve&) = delete;

    ATTR_ALWAYS_INLINE void run_sieve()
    {
        const size_t maxintsh = m_sieveSize >> 1;
        const size_t q = static_cast<size_t>(std::sqrt(static_cast<double>(m_sieveSize)));
        const size_t qh = q >> 1;
        uint64_t* const a = m_bits;

        size_t step = 1; // Start at prime 7 (7 -> 11 is step 1)
        size_t inc = STEPS[step];
        size_t factorh = 7 >> 1; // 7 / 2 = 3

        while (factorh <= qh) {
            const size_t wIdx = factorh >> 6;
            const size_t bIdx = factorh & 63;

            if ((a[wIdx] & (1ULL << bIdx)) == 0) {
                const size_t factor = (factorh << 1) + 1;
                size_t istep = step;
                size_t i = (factor * factor) >> 1;

                const size_t s0 = factor * STEPS[istep];
                const size_t s1 = factor * STEPS[(istep + 1) & 7];
                const size_t s2 = factor * STEPS[(istep + 2) & 7];
                const size_t s3 = factor * STEPS[(istep + 3) & 7];
                const size_t s4 = factor * STEPS[(istep + 4) & 7];
                const size_t s5 = factor * STEPS[(istep + 5) & 7];
                const size_t s6 = factor * STEPS[(istep + 6) & 7];
                const size_t s7 = factor * STEPS[(istep + 7) & 7];
                const size_t cycle = factor * 15;

                while (i + cycle < maxintsh) {
                    a[i >> 6] |= (1ULL << (i & 63)); i += s0;
                    a[i >> 6] |= (1ULL << (i & 63)); i += s1;
                    a[i >> 6] |= (1ULL << (i & 63)); i += s2;
                    a[i >> 6] |= (1ULL << (i & 63)); i += s3;
                    a[i >> 6] |= (1ULL << (i & 63)); i += s4;
                    a[i >> 6] |= (1ULL << (i & 63)); i += s5;
                    a[i >> 6] |= (1ULL << (i & 63)); i += s6;
                    a[i >> 6] |= (1ULL << (i & 63)); i += s7;
                }
                while (i < maxintsh) {
                    a[i >> 6] |= (1ULL << (i & 63));
                    i += factor * STEPS[istep];
                    istep = (istep + 1) & 7;
                }
            }

            factorh += inc;
            step = (step + 1) & 7;
            inc = STEPS[step];
        }
    }

    size_t count_primes() const
    {
        size_t count = 3; // Pre-sieved primes 2, 3, 5
        size_t factor = 7;
        size_t step = 1;
        size_t inc = STEPS[step] << 1;
        const uint64_t* const a = m_bits;

        while (factor <= m_sieveSize) {
            const size_t half = factor >> 1;
            if ((a[half >> 6] & (1ULL << (half & 63))) == 0) {
                count++;
            }
            factor += inc;
            step = (step + 1) & 7;
            inc = STEPS[step] << 1;
        }
        return count;
    }

    bool validate_results() const
    {
        switch (m_sieveSize) {
            case 10: return count_primes() == 4;
            case 100: return count_primes() == 25;
            case 1000: return count_primes() == 168;
            case 10000: return count_primes() == 1229;
            case 100000: return count_primes() == 9592;
            case 1000000: return count_primes() == 78498;
            case 10000000: return count_primes() == 664579;
            default: return false;
        }
    }
};

static std::pair<size_t, double> run_single_thread(double target_seconds, size_t limit)
{
    using Clock = std::chrono::steady_clock;
    auto start = Clock::now();
    size_t passes = 0;

    while (true) {
        PrimeSieve sieve(limit);
        sieve.run_sieve();
        passes++;

        const double elapsed = std::chrono::duration<double>(Clock::now() - start).count();
        if (elapsed >= target_seconds) {
            return { passes, elapsed };
        }
    }
}

static std::pair<size_t, double> run_multi_thread(double target_seconds, size_t limit, unsigned int threads)
{
    using Clock = std::chrono::steady_clock;
    auto start = Clock::now();

    std::vector<size_t> thread_passes(threads, 0);
    std::vector<std::thread> workers;
    workers.reserve(threads);

    for (unsigned int t = 0; t < threads; ++t) {
        workers.emplace_back([t, target_seconds, limit, &thread_passes]() {
            using TClock = std::chrono::steady_clock;
            auto tstart = TClock::now();
            size_t local_passes = 0;

            while (true) {
                PrimeSieve sieve(limit);
                sieve.run_sieve();
                local_passes++;

                const double elapsed = std::chrono::duration<double>(TClock::now() - tstart).count();
                if (elapsed >= target_seconds) {
                    thread_passes[t] = local_passes;
                    break;
                }
            }
        });
    }

    for (auto& w : workers) {
        if (w.joinable()) w.join();
    }

    const double elapsed = std::chrono::duration<double>(Clock::now() - start).count();
    size_t total = 0;
    for (size_t p : thread_passes) total += p;
    return { total, elapsed };
}

int main(int argc, char** argv)
{
    double seconds = 5.0;
    size_t limit = 1000000;

    if (argc > 1) seconds = std::atof(argv[1]);
    if (argc > 2) limit = std::strtoull(argv[2], nullptr, 10);

    PrimeSieve validator(limit);
    validator.run_sieve();
    if (!validator.validate_results()) {
        std::cerr << "[ERROR] Validation failed for limit=" << limit 
                  << " count=" << validator.count_primes() << std::endl;
        return 1;
    }

    // 1. Single-threaded benchmark
    auto [s_passes, s_time] = run_single_thread(seconds, limit);
    std::cout << "ndt0208-cpp-wheel8;" << s_passes << ";" << s_time 
              << ";1;algorithm=wheel,faithful=yes,bits=1" << std::endl;

    // 2. Multi-threaded benchmark
    unsigned int threads = std::thread::hardware_concurrency();
    if (threads == 0) threads = 1;
    auto [m_passes, m_time] = run_multi_thread(seconds, limit, threads);
    std::cout << "ndt0208-cpp-wheel8-par;" << m_passes << ";" << m_time 
              << ";" << threads << ";algorithm=wheel,faithful=yes,bits=1" << std::endl;

    return 0;
}
