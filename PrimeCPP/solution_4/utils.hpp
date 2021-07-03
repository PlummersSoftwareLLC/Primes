#pragma once

#include <type_traits>
#include <utility>

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

} // namespace utils
