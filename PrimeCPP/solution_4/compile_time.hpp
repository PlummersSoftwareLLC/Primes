#pragma once

#include <algorithm>
#include <array>

#include <cstddef>

template<std::size_t SieveSize>
consteval auto genSieve()
{
    auto sieve = std::array<bool, SieveSize + 1>{};
    std::fill(sieve.begin(), sieve.end(), true);

    for(auto i = std::size_t{2}; i * i <= SieveSize; ++i) {
        for(auto num = i; i <= SieveSize; ++num) {
            if(sieve[num]) {
                i = num;
                break;
            }
        }
        for(auto num = i * i; num <= SieveSize; num += i) {
            sieve[num] = false;
        }
    }

    return sieve;
}

template<std::size_t SieveSize, const std::array<bool, SieveSize> Sieve>
consteval auto countPrimes()
{
    static_assert(SieveSize > 0, "Sieve cannot be empty");

    auto numPrimes = std::size_t{0};
    for(auto i = std::size_t{2}; i < SieveSize; ++i) {
        if(Sieve[i]) {
            ++numPrimes;
        }
    }

    return numPrimes;
}

template<std::size_t SieveSize>
consteval auto genPrimes()
{
    constexpr auto sieve = genSieve<SieveSize>();
    constexpr auto numPrimes = countPrimes<sieve.size(), sieve>();
    auto primes = std::array<std::size_t, numPrimes>{};

    for(auto i = std::size_t{2}, primeIdx = std::size_t{0}; i < sieve.size(); ++i) {
        if(sieve[i]) {
            primes[primeIdx++] = i;
        }
    }

    return primes;
}

template<std::size_t WheelSize, std::size_t SieveSize = 0>
consteval auto genWheel()
{
    constexpr auto primes = genPrimes<SieveSize>();
    if constexpr(primes.size() == WheelSize) {
        return primes;
    }
    else {
        return genWheel<WheelSize, SieveSize + 1>();
    }
}
