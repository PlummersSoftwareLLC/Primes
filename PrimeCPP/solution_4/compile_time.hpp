#pragma once

#include <algorithm>
#include <array>
#include <numeric>

#include <cstddef>

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Compile time function to generate a sieve.

template<std::size_t SieveSize>
consteval auto genSieve()
{
    auto sieve = std::array<bool, SieveSize + 1>{};
    std::fill(sieve.begin(), sieve.end(), true);

    for(auto i = std::size_t{2}; i * i <= SieveSize; ++i) {
        while(!sieve[i]) {
            ++i;
        }

        for(auto num = i * i; num <= SieveSize; num += i) {
            sieve[num] = false;
        }
    }

    return sieve;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Compile time function to count the number of primes contained in a sieve.

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

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Compile time function that generates a list of primes up to and including the SieveSize.

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

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Compile time function that generates the base of a wheel, where the WheelSize represents the number of primes the
// wheel consists of.

template<std::size_t WheelSize, std::size_t SieveSize = 0>
consteval auto genWheelPrimes()
{
    constexpr auto primes = genPrimes<SieveSize>();
    if constexpr(primes.size() == WheelSize) {
        return primes;
    }
    else {
        // Recursion allows to generate WheelSize number of primes, by increasing the sieve size by 1 until
        // the sieve contains WheelSize primes.
        return genWheelPrimes<WheelSize, SieveSize + 1>();
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Compile time function that computes the wheel, given a list of base primes.

template<std::size_t BaseSize, const std::array<std::size_t, BaseSize> Base, bool GetSize = false>
consteval auto calcWheel()
{
    // The wheel contains all numbers not divisible by the base, up to the product of the base plus 1.
    constexpr auto limit = std::accumulate(Base.begin(), Base.end(), std::size_t{1}, std::multiplies<std::size_t>()) + 1;

    // List of all numbers in the range [1, limit].
    auto numbers = std::array<std::size_t, limit>{};
    std::iota(numbers.begin(), numbers.end(), std::size_t{1});

    // Mark all numbers divisible by the base with 0.
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
        // Return the number of elements, because this needs to be a constexpr in order to create a std::array
        // of those elements.
        return elements;
    }
    else {
        // Recursive call to determine the size as constexpr first, even though the elements variable contains the
        // same number, it is not a constexpr here and thus cannot be used to specify the size of a std::array.
        constexpr auto numElements = calcWheel<BaseSize, Base, true>();
        auto result = std::array<std::size_t, numElements>{};

        // Copy all numbers not marked as 0 to get the wheel.
        std::copy_if(numbers.begin(), numbers.end(), result.begin(), [](const auto& elem) { return elem != 0; });
        return result;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Compile time function that computes the increments/distances between numbers of a wheel.

template<std::size_t WheelSize, const std::array<std::size_t, WheelSize> Wheel, bool GetSize = false>
consteval auto calcWheelIncrements()
{
    auto increments = Wheel;
    std::adjacent_difference(Wheel.begin(), Wheel.end(), increments.begin());

    // Cut off the first number, because it doesn't represent a distance between two numbers of the wheel.
    auto truncIncrements = std::array<std::size_t, WheelSize - 1>{};
    std::copy(increments.begin() + 1, increments.end(), truncIncrements.begin());
    return truncIncrements;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Compile time function that generates the wheel (increments), and optionally already halves each value for the half-storage
// optimizations.

template<std::size_t WheelSize, bool HalfStorage = false>
consteval auto genWheel()
{
    static_assert(!(HalfStorage && WheelSize == 0), "Half storage does not work with the trivial wheel");
    constexpr auto wheelBase = genWheelPrimes<WheelSize>();
    constexpr auto wheel = calcWheel<wheelBase.size(), wheelBase>();
    constexpr auto wheelIncs = calcWheelIncrements<wheel.size(), wheel>();
    if constexpr(HalfStorage) {
        auto halvedWheelIncs = wheelIncs;
        std::for_each(halvedWheelIncs.begin(), halvedWheelIncs.end(), [](auto& val) { val /= 2; });
        return halvedWheelIncs;
    }
    else {
        return wheelIncs;
    }
}
