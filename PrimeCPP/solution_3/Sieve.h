#pragma once
#include <cassert>
#include <cstddef>
#include <cstdint>

// from https://baptiste-wicht.com/posts/2014/07/compile-integer-square-roots-at-compile-time-in-cpp.html
static constexpr uint64_t ct_sqrt(uint64_t res, uint64_t l, uint64_t r)
{
	if (l == r)
	{
		return r;
	}
	else
	{
		const auto mid = (r + l) / 2;

		if (mid * mid >= res)
		{
			return ct_sqrt(res, l, mid);
		}
		else
		{
			return ct_sqrt(res, mid + 1, r);
		}
	}
}

static constexpr uint64_t ct_sqrt(uint64_t res)
{
	return ct_sqrt(res, 1, res);
}

class Sieve
{
public:
	using U = uint64_t;
	static constexpr uint64_t maxSize = 50000000L; // stack size limited

private:
	static constexpr auto wordsize = sizeof(U) * 8;
	static constexpr auto words = (maxSize + wordsize - 1) / wordsize;

	U bitmap[words] = {0}; // 0 means contains, 1 means missing
	uint64_t sieveSize = maxSize;

	constexpr void insert(uint64_t index)
	{
		const auto bi = index / wordsize;
		const auto off = index % wordsize;
		bitmap[bi] &= ~(((U)1) << off);
	}

	constexpr void remove(uint64_t index)
	{
		const auto bi = index / wordsize;
		const auto off = index % wordsize;
		bitmap[bi] |= (((U)1) << off);
	}

public:
	constexpr bool contains(uint64_t index) const
	{
		const auto bi = index / wordsize;
		const auto off = index % wordsize;
		return 0 == (bitmap[bi] & (((U)1) << off));
	}

	constexpr Sieve() {}

	constexpr Sieve(const uint64_t size) : sieveSize(size) {}

	constexpr uint64_t size() const
	{
		return sieveSize;
	}

	constexpr uint64_t count() const
	{
		uint64_t count = sieveSize > 1 ? 1 : 0;
		for (uint64_t num = 3; num <= sieveSize; num += 2)
		{
			if (contains(num))
			{
				count++;
			}
		}

		return count;
	}

	constexpr void runSieve()
	{
		uint64_t factor = 3;
		const auto q = (int)ct_sqrt(sieveSize);

		while (factor <= q)
		{
			for (uint64_t num = factor; num < sieveSize; num += 2)
			{
				if (contains(num))
				{
					factor = num;
					break;
				}
			}
			for (uint64_t num = factor * factor; num < sieveSize; num += factor * 2)
			{
				remove(num);
			}

			factor += 2;
		}
	}

	U *u() { return bitmap; }
};
