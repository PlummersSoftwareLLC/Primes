#pragma once

#include <tuple>
#include <type_traits>
#include <utility>

#include <cmath>
#include <cstddef>

namespace utils {

template<typename...>
struct always_false : std::false_type {
};
template<typename... Ts>
inline constexpr auto always_false_v = always_false<Ts...>::value;

template<typename Fn, std::size_t... Idxs>
decltype(auto) for_constexpr(Fn&& func, std::index_sequence<Idxs...>)
{
    if constexpr((std::is_void_v<std::invoke_result_t<Fn, std::integral_constant<std::size_t, Idxs>>> && ...)) {
        (func(std::integral_constant<std::size_t, Idxs>{}), ...);
    }
    else if constexpr((std::is_same_v<std::invoke_result_t<Fn, std::integral_constant<std::size_t, Idxs>>, bool> && ...)) {
        if((func(std::integral_constant<std::size_t, Idxs>{}) && ...))
            return true;
        return false;
    }
    else if constexpr((!std::is_void_v<std::invoke_result_t<Fn, std::integral_constant<std::size_t, Idxs>>> && ...)) {
        return std::tuple{func(std::integral_constant<std::size_t, Idxs>{})...};
    }
    else {
        static_assert(always_false_v<Fn>, "All control paths must either return void, bool, or arbitrary non-void types");
    }
}

template<typename Fn, typename Tuple>
decltype(auto) for_constexpr(Fn&& func, Tuple&& tuple)
{
    return for_constexpr([&](const auto& idx) { return func(std::get<idx.value>(tuple)); },
                         std::make_index_sequence<std::tuple_size_v<std::remove_reference_t<Tuple>>>{});
}

template<typename T, typename U>
constexpr auto ceildiv(const T& dividend, const U& divisor)
{
    return static_cast<std::common_type_t<T, U>>(std::ceil(static_cast<double>(dividend) / divisor));
}

} // namespace utils
