#pragma once

#include <array>
#include <bit>
#include <string>
#include <vector>

#include <climits>
#include <cstdint>
#include <cstring>

#include "utils.hpp"

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Helper that transforms type to string for pretty printing.

namespace detail {

template<typename T>
static inline std::string formatType()
{
    if constexpr(std::is_same_v<std::remove_cvref_t<T>, bool>) {
        return "<bool>";
    }
    else if constexpr(std::is_same_v<std::remove_cvref_t<T>, std::uint8_t>) {
        return "<u8>";
    }
    else if constexpr(std::is_same_v<std::remove_cvref_t<T>, std::uint16_t>) {
        return "<u16>";
    }
    else if constexpr(std::is_same_v<std::remove_cvref_t<T>, std::uint32_t>) {
        return "<u32>";
    }
    else if constexpr(std::is_same_v<std::remove_cvref_t<T>, std::uint64_t>) {
        return "<u64>";
    }
    else {
        static_assert(utils::always_false_v<T>, "Unknown type");
    }
}

} // namespace detail

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Thin wrapper around std::vector that implements the interface expected by the generic sieve.

template<typename T, bool Invert = true>
class VectorStorage {
    using Index = std::size_t;

    // Helper that allows transparent inversion without having to change the logic of the sieve.
    class ElementReference {
      public:
        explicit ElementReference(VectorStorage& parent, const Index idx) : m_parent(parent), m_idx(idx) {}

        inline ElementReference& operator=(const T& value)
        {
            m_parent.m_storage[m_idx] = Invert ? !value : value;
            return *this;
        }

        inline operator T() const { return Invert ? !m_parent.m_storage[m_idx] : m_parent.m_storage[m_idx]; }

      private:
        VectorStorage& m_parent;
        const Index m_idx;
    };

  public:
    explicit VectorStorage(const std::size_t size) : m_storage(size, !Invert) {}

    inline ElementReference operator[](const Index idx) { return ElementReference{*this, idx}; }

    inline operator std::string() const
    {
        auto desc = Invert ? std::string{"inv_"} : std::string{""};
        desc += "vec";
        desc += detail::formatType<T>();
        return desc;
    }

    std::size_t getBitCount() const
    {
        if constexpr(std::is_same_v<std::remove_cv_t<T>, bool>) {
            // The size of an element of std::vector<bool> is usually 1-bit, although
            // this is implementation defined and not guaranteed by the standard.
            return 1;
        }
        else {
            return sizeof(T) * CHAR_BIT;
        }
    }

    Index makeIdx(const std::size_t start) const { return Index{start}; }

  private:
    std::vector<T> m_storage;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Thin wrapper around raw array that implements the interface expected by the generic sieve.

template<typename T, bool Invert = true>
class ArrayStorage {
    using Index = std::size_t;

    class ElementReference {
      public:
        explicit ElementReference(ArrayStorage& parent, const Index idx) : m_parent(parent), m_idx(idx) {}

        inline ElementReference& operator=(const T& value)
        {
            m_parent.m_storage[m_idx] = Invert ? !value : value;
            return *this;
        }

        inline operator T() const { return Invert ? !m_parent.m_storage[m_idx] : m_parent.m_storage[m_idx]; }

      private:
        ArrayStorage& m_parent;
        const Index m_idx;
    };

  public:
    explicit ArrayStorage(const std::size_t size) : m_size(size), m_storage(new T[size]) { std::memset(m_storage, !Invert, m_size * sizeof(T)); }

    ~ArrayStorage() { delete[] m_storage; }

    inline ElementReference operator[](const Index idx) { return ElementReference{*this, idx}; }

    inline operator std::string() const
    {
        auto desc = Invert ? std::string{"inv_"} : std::string{""};
        desc += "arr";
        desc += detail::formatType<T>();
        return desc;
    }

    inline std::size_t getBitCount() const { return sizeof(T) * CHAR_BIT; }

    inline Index makeIdx(const std::size_t start) const { return Index{start}; }

  private:
    const std::size_t m_size;
    T* m_storage;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Storage that stores each flag as 1 bit inside an array of type T.

template<typename T, bool Invert = true>
class BitStorage {
    using Index = std::size_t;

    static constexpr auto STORAGE_WIDTH = sizeof(T) * CHAR_BIT;

    // Helper that transparently handles inversion and bit fiddling so the sieve logic does not need to change.
    class BitReference {
      public:
        explicit BitReference(BitStorage& parent, const Index idx) : m_parent(parent), m_idx(idx) {}

        inline BitReference& operator=(const bool value)
        {
            // Since STORAGE_WIDTH is known at compile-time and guaranteed to be a power of 2,
            // the compiler will optimize out the division and use either a mask or bit-shift.
            const auto byteIdx = m_idx / STORAGE_WIDTH;
            const auto bitIdx = m_idx % STORAGE_WIDTH;

            if(value ^ Invert) {
                m_parent.m_storage[byteIdx] |= (T{1} << bitIdx);
            }
            else {
                m_parent.m_storage[byteIdx] &= ~(T{1} << bitIdx);
            }

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
        const Index m_idx;
    };

  public:
    BitStorage() : m_size(0), m_storage(nullptr) {}

    // Constructor for copying a pre-generated sieve.
    template<std::size_t SieveSize>
    explicit BitStorage(const std::array<T, SieveSize>& bitSieve) : m_size(SieveSize)
                                                                  , m_storage(new T[m_size])
    {
        std::memcpy(m_storage, bitSieve.data(), m_size * sizeof(T));
    }

    explicit BitStorage(const std::size_t size) : m_size(utils::ceildiv(size, STORAGE_WIDTH)), m_storage(new T[m_size])
    {
        // Compiler will optimize out this loop and replace it with a memset.
        for(auto i = std::size_t{0}; i < m_size; ++i) {
            m_storage[i] = Invert ? T{} : ~T{};
        }
    }

    // Move assignment to assign a sieve constructed using the pre-generated sieve copy constructor.
    BitStorage& operator=(BitStorage&& other)
    {
        m_size = other.m_size;
        m_storage = other.m_storage;

        other.m_size = 0;
        other.m_storage = nullptr;
        return *this;
    }

    ~BitStorage() { delete[] m_storage; }

    inline BitReference operator[](const Index idx) { return BitReference(*this, idx); }

    inline operator std::string() const
    {
        auto desc = Invert ? std::string{"inv_"} : std::string{""};
        desc += "bits";
        desc += detail::formatType<T>();
        return desc;
    }

    std::size_t getBitCount() const { return 1; }

    Index makeIdx(const std::size_t start) const { return Index{start}; }

  private:
    std::size_t m_size;
    T* m_storage = nullptr;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Bit storage that uses a lookup table to access a single bit.

template<typename T, bool Invert = true>
class MaskedBitStorage {
    using Index = std::size_t;

    // Generates a mask for every possible bit position.
    static constexpr auto genMaskLUT(bool invert)
    {
        auto maskLUT = std::array<std::size_t, STORAGE_WIDTH>{};
        for(auto i = std::size_t{0}; i < maskLUT.size(); ++i) {
            maskLUT[i] = std::size_t{1} << i;
            if(invert) {
                maskLUT[i] = ~maskLUT[i];
            }
        }
        return maskLUT;
    }

    static constexpr auto STORAGE_WIDTH = sizeof(T) * CHAR_BIT;
    static constexpr auto BIT_MASK = STORAGE_WIDTH - 1;
    static constexpr auto BIT_SHIFT = std::popcount(BIT_MASK);
    static constexpr auto MASK_LUT = genMaskLUT(false);
    static constexpr auto MASK_LUT_INV = genMaskLUT(true);

    class BitReference {
      public:
        explicit BitReference(MaskedBitStorage& parent, const Index idx) : m_parent(parent), m_idx(idx) {}

        inline BitReference& operator=(const bool value)
        {
            const auto byteIdx = m_idx >> BIT_SHIFT;
            const auto bitIdx = m_idx & BIT_MASK;

            // Lookup mask instead of computing bit-shift on-the-fly.

            if(value ^ Invert) {
                m_parent.m_storage[byteIdx] |= MASK_LUT[bitIdx];
            }
            else {
                m_parent.m_storage[byteIdx] &= MASK_LUT_INV[bitIdx];
            }

            return *this;
        }

        inline operator bool() const
        {
            const auto byteIdx = m_idx >> BIT_SHIFT;
            const auto bitIdx = m_idx & BIT_MASK;

            if constexpr(Invert) {
                return !(m_parent.m_storage[byteIdx] & MASK_LUT[bitIdx]);
            }
            else {
                return (m_parent.m_storage[byteIdx] & MASK_LUT[bitIdx]);
            }
        }

      private:
        MaskedBitStorage& m_parent;
        const Index m_idx;
    };

  public:
    MaskedBitStorage() : m_size(0), m_storage(nullptr) {}

    explicit MaskedBitStorage(const std::size_t size) : m_size(utils::ceildiv(size, STORAGE_WIDTH)), m_storage(new T[m_size])
    {
        for(auto i = std::size_t{0}; i < m_size; ++i) {
            m_storage[i] = Invert ? T{} : ~T{};
        }
    }

    ~MaskedBitStorage() { delete[] m_storage; }

    inline BitReference operator[](const Index idx) { return BitReference(*this, idx); }

    inline operator std::string() const
    {
        auto desc = Invert ? std::string{"inv_"} : std::string{""};
        desc += "maskedbits";
        desc += detail::formatType<T>();
        return desc;
    }

    std::size_t getBitCount() const { return 1; }

    Index makeIdx(const std::size_t start) const { return Index{start}; }

  private:
    std::size_t m_size;
    T* m_storage = nullptr;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Bit storage that uses a stride of STORAGE_WIDTH to store consecutive bits.
// For example using two bytes (uint8_t) to store the bits labeled 0, 1, 2, 3 looks like this using the regular bit storage:
// 0123xxxx xxxxxxxx
// With strided bit storage consecutive bits are 8-bits apart, looping around (and incrementing the bit pos) at the end.
// 02xxxxxx 13xxxxxx

template<typename T, bool Invert = true>
class StridedBitStorage {
    static constexpr auto STORAGE_WIDTH = sizeof(T) * CHAR_BIT;

  private:
    // Custom index to minimize necessary compution in the hot-loop.
    class Index {
      public:
        friend StridedBitStorage;

        constexpr Index(const std::size_t start, const std::size_t width) : m_width(width), m_byte(0), m_bit(0), m_done(false)
        {
            // In order to avoid dividing to determine the byte/bit positions, use the += operator to increment from 0
            // looping around on overflow.
            *this += start;
            // Mask needs to be generated here, because if no overflow occurs the += operator does not generate one.
            genMask();
        }

        // Used to increment index inside the hot loop, needs to be as fast as possible.
        constexpr inline Index& operator+=(const std::size_t inc)
        {
            m_byte += inc;

            // Loop unrolling is faster than run-time loop here.
            utils::for_constexpr(
                [&](const auto) {
                    // Wrap around on overflow and increment bit position.
                    if(m_byte >= m_width) {
                        m_byte -= m_width;
                        ++m_bit;
                        genMask();
                        // Simplifying the end-condition check from a ">=" to a boolean saves cycles in the hot-loop.
                        // And this overflow happens only STORAGE_WIDTH times, instead of on every iteration of the hot-loop.
                        if(m_bit >= STORAGE_WIDTH) {
                            m_done = true;
                        }
                    }
                },
                // Unrolling STORAGE_WIDTH times, as this is maximum number of overflows that can occur.
                std::make_index_sequence<STORAGE_WIDTH>{});

            return *this;
        }

        // Operators to allow the sieve to compute the factor using this custom index.
        constexpr inline std::size_t operator*(const Index& rhs) const { return static_cast<std::size_t>(*this) * static_cast<std::size_t>(rhs); }
        constexpr inline std::size_t operator*(const std::size_t& rhs) const { return static_cast<std::size_t>(*this) * rhs; }

        // Operator to allow the sieve to iterate over the storage to determine the primes at the end.
        constexpr inline auto operator<(const std::size_t rhs) const { return static_cast<std::size_t>(*this) < rhs; }

        // Operator used by the hot-loop to check the end-condition. Using a boolean check here saves cycles.
        constexpr inline auto operator<=(const std::size_t) const { return !m_done; }

        // Explicit conversion operator to allow the sieve to retrieve the logical index position.
        constexpr explicit inline operator std::size_t() const { return m_bit * m_width + m_byte; }

      private:
        // Generates mask to save on bit-shifting during the hot-loop.
        constexpr inline void genMask()
        {
            m_mask = (T{1} << m_bit);
            m_maskInv = ~(T{1} << m_bit);
        }

        const std::size_t m_width;
        std::size_t m_byte;
        std::size_t m_bit;
        bool m_done;
        T m_mask;
        T m_maskInv;
    };

    // Helper that handles bit access and inversion.
    class StridedBitReference {
      public:
        explicit StridedBitReference(StridedBitStorage& parent, const Index& idx) : m_parent(parent), m_idx(idx) {}

        inline StridedBitReference& operator=(const bool value)
        {
            if(value ^ Invert) {
                m_parent.m_storage[m_idx.m_byte] |= m_idx.m_mask;
            }
            else {
                m_parent.m_storage[m_idx.m_byte] &= m_idx.m_maskInv;
            }

            return *this;
        }

        inline operator bool() const { return ((m_parent.m_storage[m_idx.m_byte] >> m_idx.m_bit) & 1) ^ Invert; }

      private:
        StridedBitStorage& m_parent;
        const Index& m_idx;
    };

  public:
    explicit StridedBitStorage(const std::size_t size) : m_size(utils::ceildiv(size, STORAGE_WIDTH)), m_storage(new T[m_size])
    {
        for(auto i = std::size_t{0}; i < m_size; ++i) {
            m_storage[i] = Invert ? T{} : ~T{};
        }
    }

    ~StridedBitStorage() { delete[] m_storage; }

    inline StridedBitReference operator[](const Index& idx) { return StridedBitReference(*this, idx); }

    inline operator std::string() const
    {
        auto desc = Invert ? std::string{"inv_"} : std::string{""};
        desc += "stridedbits";
        desc += detail::formatType<T>();
        return desc;
    }

    std::size_t getBitCount() const { return 1; }

    Index makeIdx(const std::size_t start) const { return Index{start, m_size}; }

  private:
    std::size_t m_size;
    T* m_storage = nullptr;
};
