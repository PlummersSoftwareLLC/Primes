

#include <cstdint>
#include <cstdio>
#include <cstdlib>

/* Code was taken from PrimeCPP/solution_3 and adapted to generate a static
 * table of primes. */

static constexpr uint64_t ct_sqrt(uint64_t res, uint64_t l, uint64_t r) {
  if (l == r) {
    return r;
  } else {
    const auto mid = (r + l) / 2;

    if (mid * mid >= res) {
      return ct_sqrt(res, l, mid);
    } else {
      return ct_sqrt(res, mid + 1, r);
    }
  }
}

static constexpr uint64_t ct_sqrt(uint64_t res) { return ct_sqrt(res, 1, res); }

constexpr uint64_t reqSize = 10000000L;
constexpr auto wordsize = sizeof(uint64_t) * 8;
constexpr auto words = (reqSize + wordsize - 1) / wordsize;
constexpr uint64_t maxSize = words * wordsize;

struct sieve_t {
  uint64_t *bitmap;
  uint64_t primeCount;
};

sieve_t runSieve() {

  sieve_t s{};
  s.bitmap = static_cast<uint64_t *>(malloc(words * sizeof(uint64_t)));
  for (int i = 0; i < words; i++) {
    s.bitmap[i] = 0;
  }

  uint64_t factor = 3;
  constexpr auto q = ct_sqrt(maxSize);

  s.bitmap[0] = 0b011;

  while (factor <= q) {
    for (uint64_t num = factor; num < maxSize; num += 2) {
      const auto bi = num / wordsize;
      const auto off = num % wordsize;
      if (0 == (s.bitmap[bi] & (1ul << off))) {
        factor = num;
        break;
      }
    }
    for (uint64_t num = factor * factor; num < maxSize; num += factor * 2) {
      const auto bi = num / wordsize;
      const auto off = num % wordsize;
      s.bitmap[bi] |= (1ul << off);
    }

    factor += 2;
  }

  for (uint64_t i = 4; i <= maxSize; i += 2) {
    const auto bi = i / wordsize;
    const auto off = i % wordsize;
    s.bitmap[bi] |= (1ul << off);
  }

  s.primeCount = 1;
  for (uint64_t num = 3; num <= maxSize; num += 2) {
    const auto bi = num / wordsize;
    const auto off = num % wordsize;
    s.primeCount += (s.bitmap[bi] & (1ul << off)) == 0 ? 1 : 0;
  }

  return s;
}

int main(int argc, char **argv) {

  auto res = runSieve();
  printf("#ifndef PRIMES_H\n");
  printf("#define PRIMES_H\n");
  printf("#include <cstdint>\n");
  printf("constexpr uint64_t primes[] {\n");

  for (int64_t i = 0, l = words; i < l; ++i) {
    uint64_t val = res.bitmap[i];
    printf("   0b");
    for (int64_t j = 63; j >= 0; j--) {
      printf("%c", (val & (1ul << j)) == 0 ? '1' : '0');
    }
    if (i + 1 == l) {
      printf("\n");
    } else {
      printf(",\n");
    }
  }
  printf("};\n");
  printf("#endif // PRIMES_H\n");

  return 0;
}
