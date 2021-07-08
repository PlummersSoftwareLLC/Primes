#pragma once

#include <algorithm>
#include <string>
#include <vector>

#include <climits>
#include <cstddef>

#include "compile_time.hpp"
#include "utils.hpp"

struct Config {
    const std::string name;
    const std::size_t threads;
    const std::string algorithm;
    const bool faithful;
    const std::size_t bits;
};

template<std::size_t SieveSize>
class PreGenerated {
  public:
    PreGenerated(const std::size_t sieveSize) : m_sieveSize(sieveSize) {}

    inline void runSieve() { m_bits = genSieve<SieveSize>(); }

    inline std::size_t countPrimes() { return getPrimes().size(); }

    inline std::vector<std::size_t> getPrimes()
    {
        auto primes = std::vector<std::size_t>{};
        if(m_sieveSize >= 2) {
            primes.push_back(2);
        }
        for(auto i = std::size_t{3}; i <= m_sieveSize; i += 2) {
            if(m_bits[i]) {
                primes.push_back(i);
            }
        }
        return primes;
    }

    inline Config getConfig() const
    {
        auto name = std::string{"BlackMark"};
        name += "-pregenerated-";
        name += "array<bool>";
        return {name, 1, "base", false, sizeof(typename decltype(m_bits)::value_type) * CHAR_BIT};
    }

  private:
    const std::size_t m_sieveSize;
    decltype(genSieve<SieveSize>()) m_bits;
};

template<typename Storage, std::size_t WheelSize, bool ConstantStride = false, bool HalfStorage = false>
class GenericSieve {
  public:
    GenericSieve(const std::size_t sieveSize) : m_sieveSize(sieveSize), m_bits(sieveSize / (HalfStorage ? 2 : 1) + 1) {}

    inline void runSieve()
    {
        const auto sieveSize = m_sieveSize / (HalfStorage ? 2 : 1);

        auto wheelIdx = idx_t{};
        for(auto i = START_NUM; i * i <= sieveSize; i += WHEEL_INC[++wheelIdx]) {
            for(auto num = i; i <= sieveSize; num += WHEEL_INC[++wheelIdx]) {
                if(m_bits[num]) {
                    i = num;
                    break;
                }
            }
            auto strideIdx = wheelIdx;
            const auto stride = [&] {
                if constexpr(ConstantStride) {
                    return (WheelSize > 0 && !HalfStorage) ? 2 : 1;
                }
                else {
                    return WHEEL_INC[++strideIdx];
                }
            };
            const auto factor = HalfStorage ? (i * 2 + 1) : i;
            const auto start = (factor * factor) / (HalfStorage ? 2 : 1);
            for(auto num = start; num <= sieveSize; num += factor * stride()) {
                m_bits[num] = false;
            }
        }
    }

    inline std::size_t countPrimes() { return getPrimes().size(); }

    inline std::vector<std::size_t> getPrimes()
    {
        const auto sieveSize = (m_sieveSize + 1) / (HalfStorage ? 2 : 1);

        auto primes = std::vector<std::size_t>{};
        std::copy_if(BASE_PRIMES.begin(), BASE_PRIMES.end() - 1, std::back_inserter(primes), [&](const auto& prime) { return prime <= m_sieveSize; });
        auto wheelIdx = idx_t{};
        for(auto i = START_NUM; i < sieveSize; i += WHEEL_INC[++wheelIdx]) {
            if(m_bits[i]) {
                const auto prime = HalfStorage ? (2 * i + 1) : i;
                primes.push_back(prime);
            }
        }
        return primes;
    }

    inline Config getConfig() const
    {
        constexpr auto check = WHEEL_INC.size();
        constexpr auto total = std::accumulate(BASE_PRIMES.begin(), BASE_PRIMES.end() - 1, std::size_t{1}, std::multiplies<std::size_t>());

        auto name = std::string{"BlackMark"};
        name += "-" + std::to_string(check) + "of" + std::to_string(total) + "-";
        name += (ConstantStride) ? "const_stride-" : "dyn_stride-";
        name += (HalfStorage) ? "half_storage-" : "full_storage-";
        name += m_bits;
        const auto algorithm = (check == 1) ? "base" : "wheel";
        return {name, 1, algorithm, true, m_bits.getBitCount()};
    }

  private:
    static constexpr auto BASE_PRIMES = genWheelPrimes<WheelSize + 1>();
    static constexpr auto START_NUM = BASE_PRIMES[BASE_PRIMES.size() - 1] / (HalfStorage ? 2 : 1);
    static constexpr auto WHEEL_INC = genWheel<WheelSize, HalfStorage>();

    using idx_t = utils::ModIndex<std::size_t, WHEEL_INC.size()>;

    const std::size_t m_sieveSize;
    Storage m_bits;
};
