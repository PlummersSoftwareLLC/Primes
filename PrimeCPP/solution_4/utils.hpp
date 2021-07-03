#pragma once

#include <algorithm>
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
constexpr decltype(auto) for_constexpr(Fn&& func, std::index_sequence<Idxs...>)
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
constexpr decltype(auto) for_constexpr(Fn&& func, Tuple&& tuple)
{
    return for_constexpr([&](const auto& idx) { return func(std::get<idx.value>(tuple)); },
                         std::make_index_sequence<std::tuple_size_v<std::remove_reference_t<Tuple>>>{});
}

template<typename T, typename U>
constexpr auto ceildiv(const T& dividend, const U& divisor)
{
    constexpr auto ceil = [](const auto& num) {
        const auto trunc = static_cast<std::int64_t>(num);
        if(num == trunc) {
            return num;
        }
        return static_cast<std::remove_cvref_t<decltype(num)>>(trunc + 1);
    };

    return static_cast<std::common_type_t<T, U>>(ceil(static_cast<double>(dividend) / divisor));
}

template<typename T, T Width>
class ModIndex {
    static_assert(Width > T{}, "Width must be at least 1");

  public:
    constexpr ModIndex() : m_idx(T{}) {}
    constexpr ModIndex(const T& idx) : m_idx(std::clamp(idx, T{}, Width - 1)) {}

    constexpr inline T& operator++()
    {
        if(++m_idx >= Width) {
            m_idx = T{};
        }
        return m_idx;
    }

    constexpr inline T operator++(int)
    {
        const auto curIdx = m_idx;
        ++(*this);
        return curIdx;
    }

    constexpr inline T& operator--()
    {
        if(m_idx < T{1}) {
            return (m_idx = Width - 1);
        }
        return --m_idx;
    }

    constexpr inline T operator--(int)
    {
        const auto curIdx = m_idx;
        --(*this);
        return curIdx;
    }

    constexpr inline operator T() const { return m_idx; }

  private:
    T m_idx;
};

} // namespace utils
