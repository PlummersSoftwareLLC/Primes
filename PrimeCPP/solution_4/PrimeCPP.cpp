#include <chrono>
#include <map>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#include <climits>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstring>

#include "utils.hpp"
#include "validator.hpp"

template<typename T, typename U>
constexpr auto ceildiv(const T& dividend, const U& divisor)
{
    return static_cast<std::common_type_t<T, U>>(std::ceil(static_cast<double>(dividend) / divisor));
}

class BitStorage {
    using storage_t = std::uint16_t;
    static constexpr auto STORAGE_WIDTH = sizeof(storage_t) * CHAR_BIT;
    static constexpr auto INVERT = true;

    class BitReference {
      public:
        explicit BitReference(BitStorage& bitStorage, const std::size_t idx) : m_parent(bitStorage), m_idx(idx) {}

        inline BitReference& operator=(const bool value)
        {
            const auto byteIdx = m_idx / STORAGE_WIDTH;
            const auto bitIdx = m_idx % STORAGE_WIDTH;

            if(value ^ INVERT)
                m_parent.m_storage[byteIdx] |= (storage_t{1} << bitIdx);
            else
                m_parent.m_storage[byteIdx] &= ~(storage_t{1} << bitIdx);

            return *this;
        }

        inline operator bool() const
        {
            const auto byteIdx = m_idx / STORAGE_WIDTH;
            const auto bitIdx = m_idx % STORAGE_WIDTH;

            return ((m_parent.m_storage[byteIdx] >> bitIdx) & 1) ^ INVERT;
        }

      private:
        BitStorage& m_parent;
        const std::size_t m_idx;
    };

  public:
    explicit BitStorage(const std::size_t size, const bool defaultValue = false) : m_size(ceildiv(size, STORAGE_WIDTH)), m_storage(new storage_t[m_size])
    {
        for(auto i = std::size_t{0}; i < m_size; ++i) {
            m_storage[i] = (defaultValue ^ INVERT) ? ~storage_t{} : storage_t{};
        }
    }

    ~BitStorage() { delete[] m_storage; }

    inline BitReference operator[](const std::size_t idx) { return BitReference(*this, idx); }

  private:
    const std::size_t m_size;
    storage_t* m_storage = nullptr;
};

class PrimeSieve {
    using sieve_size_t = std::size_t;
    using bit_storage_t = BitStorage;

  public:
    PrimeSieve(const sieve_size_t sieveSize) : m_sieveSize(sieveSize), m_bits(sieveSize / 2 + 1, true) {}

    ~PrimeSieve() {}

    inline void runSieve()
    {
        for(auto i = sieve_size_t{1}; i * i <= m_sieveSize; ++i) {
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

    template<typename Duration>
    inline void printResults(const Duration& duration, int passes)
    {
        const auto durationS = std::chrono::duration_cast<std::chrono::microseconds>(duration).count() / 1'000'000.0;
        std::printf("BlackMark;%d;%f;1;algorithm=base,faithful=yes,bits=1\n", passes, durationS);
    }

    inline std::size_t countPrimes()
    {
        auto count = std::size_t{1};
        for(auto i = sieve_size_t{1}; i < m_sieveSize / 2; ++i)
            if(m_bits[i])
                ++count;
        return count;
    }

  private:
    const sieve_size_t m_sieveSize = 0;
    bit_storage_t m_bits;
};

int main()
{
    constexpr auto RUN_TIME = std::chrono::seconds(5);
    constexpr auto SIEVE_SIZE = 1'000'000;

    using runners_t = std::tuple<PrimeSieve>;

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
                        sieve.printResults(end - start, passes);
                    }
                    break;
                }
            }
        },
        std::make_index_sequence<std::tuple_size_v<runners_t>>{});

    return 0;
}
