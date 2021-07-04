#pragma once

#include <type_traits>
#include <utility>

#include <cmath>
#include <cstddef>

namespace utils {

template<typename Fn, std::size_t... Idxs>
auto for_constexpr(Fn&& func, std::index_sequence<Idxs...>)
{
    if constexpr(std::is_void_v<std::invoke_result_t<Fn, std::integral_constant<std::size_t, 0>>>) {
        (func(std::integral_constant<std::size_t, Idxs>{}), ...);
    }
    else {
        if((func(std::integral_constant<std::size_t, Idxs>{}) && ...))
            return true;
        return false;
    }
}

template<typename...>
struct always_false : std::false_type {
};
template<typename... Ts>
inline constexpr auto always_false_v = always_false<Ts...>::value;

template<typename T, typename U>
constexpr auto ceildiv(const T& dividend, const U& divisor)
{
    return static_cast<std::common_type_t<T, U>>(std::ceil(static_cast<double>(dividend) / divisor));
}

} // namespace utils
