#pragma once

#include <string>
#include <vector>

#include <climits>
#include <cstdint>

#include "utils.hpp"

namespace detail {

template<template<typename, bool> typename Parent, typename ElemT, bool Invert>
class ElementReference {
  public:
    explicit ElementReference(Parent<ElemT, Invert>& parent, const std::size_t idx) : m_parent(parent), m_idx(idx) {}

    inline ElementReference& operator=(const ElemT& value)
    {
        m_parent.m_storage[m_idx] = Invert ? !value : value;
        return *this;
    }

    inline operator ElemT() const { return Invert ? !m_parent.m_storage[m_idx] : m_parent.m_storage[m_idx]; }

  private:
    Parent<ElemT, Invert>& m_parent;
    const std::size_t m_idx;
};

} // namespace detail

template<typename T, bool Invert = true>
class VectorStorage {
    using ElemRef = detail::ElementReference<VectorStorage, T, Invert>;
    friend ElemRef;

  public:
    explicit VectorStorage(const std::size_t size) : m_storage(size, !Invert) {}

    inline ElemRef operator[](const std::size_t idx) { return ElemRef{*this, idx}; }

    inline operator std::string() const
    {
        auto desc = std::string{"vector"};
        if constexpr(std::is_same_v<std::remove_cv_t<T>, bool>) {
            desc += "<bool>";
        }
        else if constexpr(std::is_same_v<std::remove_cv_t<T>, std::uint8_t>) {
            desc += "<u8>";
        }
        else if constexpr(std::is_same_v<std::remove_cv_t<T>, std::uint16_t>) {
            desc += "<u16>";
        }
        else if constexpr(std::is_same_v<std::remove_cv_t<T>, std::uint32_t>) {
            desc += "<u32>";
        }
        else if constexpr(std::is_same_v<std::remove_cv_t<T>, std::uint64_t>) {
            desc += "<u64>";
        }
        else {
            static_assert(utils::always_false_v<T>, "Unknown vector element type");
        }
        desc += Invert ? "-inv" : "";

        return desc;
    }

    std::size_t getBitCount() const
    {
        if constexpr(std::is_same_v<std::remove_cv_t<T>, bool>) {
            return 1;
        }
        else {
            return sizeof(T) * CHAR_BIT;
        }
    }

  private:
    std::vector<T> m_storage;
};

template<typename T, bool Invert = true>
class BitStorage {
    static constexpr auto STORAGE_WIDTH = sizeof(T) * CHAR_BIT;

    class BitReference {
      public:
        explicit BitReference(BitStorage& bitStorage, const std::size_t idx) : m_parent(bitStorage), m_idx(idx) {}

        inline BitReference& operator=(const bool value)
        {
            const auto byteIdx = m_idx / STORAGE_WIDTH;
            const auto bitIdx = m_idx % STORAGE_WIDTH;

            if(value ^ Invert)
                m_parent.m_storage[byteIdx] |= (T{1} << bitIdx);
            else
                m_parent.m_storage[byteIdx] &= ~(T{1} << bitIdx);

            return *this;
        }

        inline operator bool() const
        {
            const auto byteIdx = m_idx / STORAGE_WIDTH;
            const auto bitIdx = m_idx % STORAGE_WIDTH;

            return ((m_parent.m_storage[byteIdx] >> bitIdx) & 1) ^ Invert;
        }

      private:
        BitStorage& m_parent;
        const std::size_t m_idx;
    };

  public:
    explicit BitStorage(const std::size_t size) : m_size(utils::ceildiv(size, STORAGE_WIDTH)), m_storage(new T[m_size])
    {
        for(auto i = std::size_t{0}; i < m_size; ++i) {
            m_storage[i] = Invert ? T{} : ~T{};
        }
    }

    ~BitStorage() { delete[] m_storage; }

    inline BitReference operator[](const std::size_t idx) { return BitReference(*this, idx); }

    inline operator std::string() const
    {
        auto desc = std::string{"bits"};
        if constexpr(std::is_same_v<std::remove_cv_t<T>, std::uint8_t>) {
            desc += "<u8>";
        }
        else if constexpr(std::is_same_v<std::remove_cv_t<T>, std::uint16_t>) {
            desc += "<u16>";
        }
        else if constexpr(std::is_same_v<std::remove_cv_t<T>, std::uint32_t>) {
            desc += "<u32>";
        }
        else if constexpr(std::is_same_v<std::remove_cv_t<T>, std::uint64_t>) {
            desc += "<u64>";
        }
        else {
            static_assert(utils::always_false_v<T>, "Unknown storage element type");
        }
        desc += Invert ? "-inv" : "";

        return desc;
    }

    std::size_t getBitCount() const { return 1; }

  private:
    const std::size_t m_size;
    T* m_storage = nullptr;
};
