#pragma once

#include <algorithm>
#include <string>
#include <vector>

#include <climits>
#include <cstddef>

#include "compile_time.hpp"

struct Config {
    const std::string name;
    const std::size_t threads;
    const std::string algorithm;
    const bool faithful;
    const std::size_t bits;
};

template<typename Storage, typename SieveSize = std::size_t>
class NaiveBase {
  public:
    NaiveBase(const SieveSize sieveSize) : m_sieveSize(sieveSize), m_bits(sieveSize + 1) {}

    inline void runSieve()
    {
        for(auto i = SieveSize{3}; i * i <= m_sieveSize; i += 2) {
            for(auto num = i; i <= m_sieveSize; num += 2) {
                if(m_bits[num]) {
                    i = num;
                    break;
                }
            }
            for(auto num = i * i; num <= m_sieveSize; num += 2 * i) {
                m_bits[num] = false;
            }
        }
    }

    inline std::size_t countPrimes() { return getPrimes().size(); }

    inline std::vector<std::size_t> getPrimes()
    {
        auto primes = std::vector<std::size_t>{};
        if(m_sieveSize >= 2) {
            primes.push_back(2);
        }
        for(auto i = SieveSize{3}; i <= m_sieveSize; i += 2) {
            if(m_bits[i]) {
                primes.push_back(i);
            }
        }
        return primes;
    }

    inline Config getConfig() const
    {
        auto name = std::string{"BlackMark"};
        name += "-naive-";
        name += m_bits;
        return {name, 1, "base", true, m_bits.getBitCount()};
    }

  private:
    const SieveSize m_sieveSize;
    Storage m_bits;
};

template<typename Storage, typename SieveSize = std::size_t>
class Base {
  public:
    Base(const SieveSize sieveSize) : m_sieveSize(sieveSize), m_bits(sieveSize / 2 + 1) {}

    inline void runSieve()
    {
        for(auto i = SieveSize{1}; i * i < m_sieveSize; ++i) {
            for(auto num = i; num < m_sieveSize / 2; ++num) {
                if(m_bits[num]) {
                    i = num;
                    break;
                }
            }
            const auto factor = i * 2 + 1;
            for(auto num = factor * factor / 2; num <= m_sieveSize / 2; num += factor)
                m_bits[num] = false;
        }
    }

    inline std::size_t countPrimes() { return getPrimes().size(); }

    inline std::vector<std::size_t> getPrimes()
    {
        auto primes = std::vector<std::size_t>{};
        if(m_sieveSize >= 2) {
            primes.push_back(2);
        }
        for(auto i = SieveSize{1}; i < (m_sieveSize + 1) / 2; ++i) {
            if(m_bits[i]) {
                primes.push_back(2 * i + 1);
            }
        }
        return primes;
    }

    inline Config getConfig() const
    {
        auto name = std::string{"BlackMark"};
        name += "-half_storage-";
        name += m_bits;
        return {name, 1, "base", true, m_bits.getBitCount()};
    }

  private:
    const SieveSize m_sieveSize;
    Storage m_bits;
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

template<typename Storage, std::size_t WheelSize, typename SieveSize = std::size_t>
class Wheel {
  public:
    Wheel(const std::size_t sieveSize) : m_sieveSize(sieveSize), m_bits(sieveSize + 1) {}

    inline void runSieve()
    {
        for(auto i = BASE_PRIMES[BASE_PRIMES.size() - 1], incIdx = std::size_t{0}; i * i <= m_sieveSize; i += WHEEL_INC[advanceIdx(incIdx)]) {
            for(auto num = i; i <= m_sieveSize; num += WHEEL_INC[advanceIdx(incIdx)]) {
                if(m_bits[num]) {
                    i = num;
                    break;
                }
            }
            constexpr auto stride = (WheelSize > 0) ? 2 : 1;
            for(auto num = i * i; num <= m_sieveSize; num += i * stride) {
                m_bits[num] = false;
            }
        }
    }

    inline std::size_t countPrimes() { return getPrimes().size(); }

    inline std::vector<std::size_t> getPrimes()
    {
        auto primes = std::vector<std::size_t>{};
        std::copy_if(BASE_PRIMES.begin(), BASE_PRIMES.end() - 1, std::back_inserter(primes), [&](const auto& prime) { return prime <= m_sieveSize; });
        for(auto i = BASE_PRIMES[BASE_PRIMES.size() - 1], incIdx = std::size_t{0}; i <= m_sieveSize; i += WHEEL_INC[advanceIdx(incIdx)]) {
            if(m_bits[i]) {
                primes.push_back(i);
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
        name += m_bits;
        return {name, 1, "wheel", true, m_bits.getBitCount()};
    }

  private:
    static constexpr auto BASE_PRIMES = genWheelPrimes<WheelSize + 1>();
    static constexpr auto WHEEL_INC = genWheel<WheelSize>();

    const std::size_t m_sieveSize;
    Storage m_bits;

    static inline std::size_t advanceIdx(std::size_t& idx)
    {
        if(++idx >= WHEEL_INC.size()) {
            idx = 0;
        }
        return idx;
    }
};
