#pragma once

#include <algorithm>
#include <string>
#include <vector>

#include <cstddef>

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
        name += "-1of2-";
        name += m_bits;
        return {name, 1, "base", true, m_bits.getBitCount()};
    }

  private:
    const SieveSize m_sieveSize;
    Storage m_bits;
};
