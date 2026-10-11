from std.memory.alloc import unsafe_alloc
from std.memory import unsafe_memset_zero
from std.time import perf_counter_ns
from std.sys import simd_width_of, argv, stderr
from std.math import sqrt

# 64-bit lanes per SIMD register on the build host: 4 for AVX2, 8 for AVX-512, 2 for NEON.
comptime W = simd_width_of[DType.uint64]()
# Primes up to this are marked densely, larger ones sparsely.
comptime DENSE_MAX = 127
# Words per unrolled dense block.
comptime BLOCK = 64
# Words still expected to be in L1 after a sweep (28 KB of a 32 KB L1D).
comptime HOT = 3584
comptime PAT_WORDS = DENSE_MAX * W + 2 * BLOCK
comptime Ptr64 = Pointer[UInt64, MutUntrackedOrigin]


struct PrimeSieve(Movable):
    """Odd-only 1-bit sieve. Bit k stands for 2k+1; a set bit marks a composite.
    """

    var words: Ptr64
    var pattern: Ptr64
    var size: Int
    var nbits: Int
    var nwords: Int
    var rot: Int

    def __init__(out self, size: Int):
        self.size = size
        self.nbits = (size + 1) >> 1
        self.nwords = (self.nbits + 63) >> 6
        self.words = unsafe_alloc[UInt64](self.nwords, alignment=64)
        unsafe_memset_zero(self.words, self.nwords)
        self.pattern = unsafe_alloc[UInt64](PAT_WORDS, alignment=64)
        self.rot = self.nwords

    def __deinit__(deinit self):
        self.words.unsafe_free()
        self.pattern.unsafe_free()

    @always_inline
    def next_rot(mut self, lo: Int, hi: Int) -> Int:
        """Where the next sweep starts: just below where the last one started,
        so it first walks the lines the last sweep touched most recently."""
        var r = self.rot - HOT
        if r <= lo:
            r = hi
        self.rot = r
        return r

    @always_inline
    def set_bits(mut self, factor: Int, b0: Int, end: Int) -> Int:
        var w = self.words
        var b = b0
        while b < end:
            w[unsafe_offset=b >> 6] |= UInt64(1) << UInt64(b & 63)
            b += factor
        return b

    @always_inline
    def dense_range(mut self, period: Int, s1: Int, a: Int, e: Int):
        var w = self.words
        var pat = self.pattern
        var j = (a - s1) % period
        var i = a
        while i + BLOCK <= e:
            var wp = w.unsafe_offset(i)
            var pp = pat.unsafe_offset(j)
            comptime for k in range(0, BLOCK, W):
                wp.unsafe_store[alignment=32](
                    k,
                    wp.unsafe_load[width=W, alignment=32](k)
                    | pp.unsafe_load[width=W, alignment=32](k),
                )
            i += BLOCK
            j += BLOCK
            if j >= period:
                j -= period
        while i < e:
            w[unsafe_offset=i] |= pat[unsafe_offset=j]
            i += 1
            j += 1
            if j >= period:
                j -= period

    def mark_dense(mut self, factor: Int, start: Int):
        """Multiples of factor repeat every factor words. Compose those words
        by stepping factor one multiple at a time, then OR them across the
        sieve one SIMD vector per store."""
        var pat = self.pattern
        var nwords = self.nwords
        var s1 = ((start >> 6) + W) & ~(W - 1)
        if s1 + 2 * BLOCK >= nwords:
            _ = self.set_bits(factor, start, self.nbits)
            return
        var b = self.set_bits(factor, start, s1 << 6)
        var r = b - (s1 << 6)
        if factor < 64:
            var steps = (64 + factor - 1) // factor
            var shift = 64 % factor
            for jj in range(factor):
                var m: UInt64 = 0
                for t in range(steps):
                    var pos = r + t * factor
                    if pos < 64:
                        m |= UInt64(1) << UInt64(pos)
                pat[unsafe_offset=jj] = m
                r -= shift
                if r < 0:
                    r += factor
        else:
            unsafe_memset_zero(pat, factor)
            while r < (factor << 6):
                pat[unsafe_offset=r >> 6] |= UInt64(1) << UInt64(r & 63)
                r += factor
        var period = factor * W
        while period < BLOCK:
            period += factor * W
        for jj in range(factor, period + BLOCK):
            pat[unsafe_offset=jj] = pat[unsafe_offset=jj - factor]
        var mid = s1 + ((self.next_rot(s1, nwords) - s1) & ~(BLOCK - 1))
        self.dense_range(period, s1, mid, nwords)
        self.dense_range(period, s1, s1, mid)

    @always_inline
    def sparse_range[C: Int](mut self, factor: Int, start: Int, c0: Int, c1: Int):
        """Eight stripes of multiples, one per bit position in a byte. Each
        store clears one composite with a single-bit mask; the masks depend
        only on factor mod 16, so they are compile-time immediates."""
        comptime p0 = C + 16
        comptime s0 = (p0 * p0) >> 1
        comptime k0 = UInt8(1 << ((s0) & 7))
        comptime k1 = UInt8(1 << ((s0 + p0) & 7))
        comptime k2 = UInt8(1 << ((s0 + 2 * p0) & 7))
        comptime k3 = UInt8(1 << ((s0 + 3 * p0) & 7))
        comptime k4 = UInt8(1 << ((s0 + 4 * p0) & 7))
        comptime k5 = UInt8(1 << ((s0 + 5 * p0) & 7))
        comptime k6 = UInt8(1 << ((s0 + 6 * p0) & 7))
        comptime k7 = UInt8(1 << ((s0 + 7 * p0) & 7))
        var bytes = self.words.unsafe_bitcast[UInt8]()
        var base = start >> 3
        var o1 = ((start + factor) >> 3) - base
        var o2 = ((start + 2 * factor) >> 3) - base
        var o3 = ((start + 3 * factor) >> 3) - base
        var o4 = ((start + 4 * factor) >> 3) - base
        var o5 = ((start + 5 * factor) >> 3) - base
        var o6 = ((start + 6 * factor) >> 3) - base
        var o7 = ((start + 7 * factor) >> 3) - base
        var bp = bytes.unsafe_offset(base + c0 * factor)
        for _ in range(c1 - c0):
            bp[unsafe_offset=0] |= k0
            bp[unsafe_offset=o1] |= k1
            bp[unsafe_offset=o2] |= k2
            bp[unsafe_offset=o3] |= k3
            bp[unsafe_offset=o4] |= k4
            bp[unsafe_offset=o5] |= k5
            bp[unsafe_offset=o6] |= k6
            bp[unsafe_offset=o7] |= k7
            bp = bp.unsafe_offset(factor)

    @always_inline
    def mark_sparse_cls[C: Int](mut self, factor: Int, start: Int):
        var nbits = self.nbits
        var n = 0
        if nbits - start - 7 * factor > 0:
            n = (nbits - start - 7 * factor + 8 * factor - 1) // (8 * factor)
        var midw = self.next_rot(start >> 6, self.nwords)
        var cm = ((midw << 6) - start) // (8 * factor)
        if cm < 0:
            cm = 0
        if cm > n:
            cm = n
        self.sparse_range[C](factor, start, cm, n)
        _ = self.set_bits(factor, start + n * 8 * factor, nbits)
        self.sparse_range[C](factor, start, 0, cm)

    @always_inline
    def mark_sparse(mut self, factor: Int, start: Int):
        var c = factor & 15
        comptime for cc in range(1, 16, 2):
            if c == cc:
                self.mark_sparse_cls[cc](factor, start)

    @always_inline
    def is_composite(self, k: Int) -> Bool:
        return (self.words[unsafe_offset=k >> 6] >> UInt64(k & 63)) & 1 != 0

    def run(mut self):
        var factor = 3
        var q = Int(sqrt(Float64(self.size)))
        while factor <= q:
            var start = (factor * factor) >> 1
            if factor <= DENSE_MAX:
                self.mark_dense(factor, start)
            else:
                self.mark_sparse(factor, start)
            factor += 2
            while factor <= q and self.is_composite(factor >> 1):
                factor += 2

    def count_primes(self) -> Int:
        if self.size < 2:
            return 0
        var c = 0
        for k in range(self.nbits):
            if not self.is_composite(k):
                c += 1
        return c


def reference_count(n: Int) -> Int:
    if n < 2:
        return 0
    var f = List[Bool](length=n + 1, fill=True)
    var c = 0
    for i in range(2, n + 1):
        if f[i]:
            c += 1
            var j = i * i
            while j <= n:
                f[j] = False
                j += i
    return c


def expected_count(size: Int) -> Int:
    if size == 10:
        return 4
    if size == 100:
        return 25
    if size == 1_000:
        return 168
    if size == 10_000:
        return 1_229
    if size == 100_000:
        return 9_592
    if size == 1_000_000:
        return 78_498
    if size == 10_000_000:
        return 664_579
    if size == 100_000_000:
        return 5_761_455
    return -1


def validate() -> Bool:
    var ok = True
    for s in range(0, 30_000):
        var sieve = PrimeSieve(s)
        sieve.run()
        if sieve.count_primes() != reference_count(s):
            print("FAIL", s, sieve.count_primes(), reference_count(s))
            ok = False
    var s = 10
    while s <= 100_000_000:
        var sieve = PrimeSieve(s)
        sieve.run()
        var got = sieve.count_primes()
        print(s, got, "PASS" if got == expected_count(s) else "FAIL")
        ok = ok and got == expected_count(s)
        s *= 10
    return ok


def main() raises:
    var args = argv()
    if len(args) > 1 and String(args[1]) == "--validate":
        if not validate():
            raise Error("validation failed")
        return
    var sieve_size = 1_000_000
    var passes = 0
    var t0 = perf_counter_ns()
    var t = t0
    var count = 0
    while t - t0 < 5_000_000_000:
        var sieve = PrimeSieve(sieve_size)
        sieve.run()
        passes += 1
        t = perf_counter_ns()
        if t - t0 >= 5_000_000_000:
            count = sieve.count_primes()
    if count != expected_count(sieve_size):
        print("invalid count", count, file=stderr)
    print(
        "lee101_1bit_dense_simd;",
        passes,
        ";",
        Float64(t - t0) / 1e9,
        ";1;algorithm=base,faithful=yes,bits=1",
        sep="",
    )
