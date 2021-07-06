#pragma once

#include <algorithm>
#include <array>
#include <numeric>

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
consteval auto genWheelPrimes()
{
    constexpr auto primes = genPrimes<SieveSize>();
    if constexpr(primes.size() == WheelSize) {
        return primes;
    }
    else {
        return genWheelPrimes<WheelSize, SieveSize + 1>();
    }
}

template<std::size_t BaseSize, const std::array<std::size_t, BaseSize> Base, bool GetSize = false>
consteval auto calcWheel()
{
    constexpr auto limit = std::accumulate(Base.begin(), Base.end(), std::size_t{1}, std::multiplies<std::size_t>()) + 1;
    auto numbers = std::array<std::size_t, limit>{};
    std::iota(numbers.begin(), numbers.end(), std::size_t{1});
    auto elements = limit;
    for(auto& number: numbers) {
        for(const auto& baseElem: Base) {
            if(number % baseElem == 0) {
                number = 0;
                --elements;
                break;
            }
        }
    }

    if constexpr(GetSize) {
        return elements;
    }
    else {
        constexpr auto numElements = calcWheel<BaseSize, Base, true>();
        auto result = std::array<std::size_t, numElements>{};
        std::copy_if(numbers.begin(), numbers.end(), result.begin(), [](const auto& elem) { return elem != 0; });
        return result;
    }
}

template<std::size_t WheelSize, const std::array<std::size_t, WheelSize> Wheel, bool GetSize = false>
consteval auto calcWheelIncrements()
{
    auto increments = Wheel;
    std::adjacent_difference(Wheel.begin(), Wheel.end(), increments.begin());
    auto truncIncrements = std::array<std::size_t, WheelSize - 1>{};
    std::copy(increments.begin() + 1, increments.end(), truncIncrements.begin());
    return truncIncrements;
}

template<std::size_t WheelSize>
consteval auto genWheel()
{
    constexpr auto wheelBase = genWheelPrimes<WheelSize>();
    constexpr auto wheel = calcWheel<wheelBase.size(), wheelBase>();
    constexpr auto wheelIncs = calcWheelIncrements<wheel.size(), wheel>();
    return wheelIncs;
}
