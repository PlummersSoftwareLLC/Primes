#pragma once

#include <map>

#include <cstddef>

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Compares computed number of primes against historical data.

static inline bool validate(const std::size_t sieveSize, const std::size_t primeCount)
{
    // clang-format off
    // Historical data for validating our results - the number of primes to be found under some limit, such as 168 primes under 1000
    static const std::map<const std::size_t, const std::size_t> s_resultsDictionary = {
        {            10,           4},
        {           100,          25},
        {         1'000,         168},
        {        10'000,       1'229},
        {       100'000,       9'592},
        {     1'000'000,      78'498},
        {    10'000'000,     664'579},
        {   100'000'000,   5'761'455},
        { 1'000'000'000,  50'847'534},
        {10'000'000'000, 455'052'511},
    };
    // clang-format on

    const auto result = s_resultsDictionary.find(sieveSize);
    if(s_resultsDictionary.end() == result)
        return false;
    return result->second == primeCount;
}
