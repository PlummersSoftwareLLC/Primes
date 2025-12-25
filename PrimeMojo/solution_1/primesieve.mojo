import time
import math
from collections import BitSet


trait Runnable:
    fn __call__(self: Self, sieve_size: Int) -> Self:
        ...

    fn run(mut self: Self) -> None:
        ...

    fn countPrimes(self: Self) -> Int:
        ...

    fn printResults(self: Self, duration: UInt64, passes: UInt64) -> None:
        ...


struct bitArray:
    var array: List[UInt8]

    fn __init__(out self: Self, size: Int):
        self.array = List[UInt8](length=(size + 7) >> 3, fill=0xFF)

    # test to stay consistent with BitSet interface
    fn test(self, index: Int) -> Bool:
        byte_index = index >> 3
        bit_index = index & 7
        return (self.array.unsafe_get(byte_index) & (1 << bit_index)) != 0

    fn clear(mut self: Self, index: Int):
        byte_index = index >> 3
        bit_index = index & 7
        self.array.unsafe_get(byte_index) &= ~(1 << bit_index)

    fn countBits(self) -> Int:
        count = 0
        for byte in self.array:
            count += bin(byte).count("1")
        return count


struct prime_sieve_1bit(Runnable):
    var array: bitArray
    var bitset_size: Int
    var sieve_size: Int

    fn __init__(out self: Self, sieve_size: Int):
        self.bitset_size = (sieve_size + 1) // 2
        self.sieve_size = sieve_size
        self.array = bitArray(self.bitset_size)

    # Implement Runnable.__call__ with matching signature
    fn __call__(self: Self, sieve_size: Int) -> Self:
        return Self(sieve_size)

    fn getBit(self, index: Int) -> Bool:
        if index % 2 == 0:
            return False
        return self.array.test(index // 2)

    fn clearBit(mut self: Self, index: UInt):
        self.array.clear(index // 2)

    fn countPrimes(self) -> Int:
        return self.array.countBits()

    fn run(mut self: Self) -> None:
        factor = 3
        q = Int(math.sqrt(self.sieve_size))

        while factor <= q:
            for num in range(factor, self.sieve_size):
                if self.getBit(num):
                    factor = num
                    break

            for num in range(factor * 3, self.sieve_size, factor * 2):
                self.clearBit(num)

            factor += 2

    fn printResults(self: Self, duration: UInt64, passes: UInt64) -> None:
        try:
            var final_string = (
                "ELucasCurrie_1bit;{0};{1};1;algorithm=base,faithful=yes,bits=1"
            ).format(passes, round(Float32(duration) / 1_000_000_000), 3)

            print(final_string)
        except:
            print("Error formatting results string")


struct prime_sieve_8bit(Runnable):
    var limit: Int
    var sieve_size: Int
    var array: List[UInt8]

    fn __init__(out self: Self, sieve_size: Int):
        self.limit = sieve_size >> 1
        self.sieve_size = sieve_size
        self.array = List[UInt8](length=self.limit, fill=0xFF)

    fn __call__(self: Self, sieve_size: Int) -> Self:
        return Self(sieve_size)

    fn countPrimes(self) -> Int:
        count = 1
        for i in range(1, self.limit):
            if self.array[i] != 0:
                count += 1
        return count

    @always_inline
    fn run(mut self: Self) -> None:
        var q = Int(math.sqrt(self.sieve_size))
        var factor = 3
        while factor <= q:
            divisor = factor >> 1
            while not (self.array.unsafe_get(divisor) != 0 or divisor >= q):
                divisor += 1
            factor = (divisor << 1) + 1

            if factor > q:
                break
            start = (factor * factor) >> 1

            while start < self.limit:
                self.array.unsafe_get(start) = 0
                start += factor
            factor += 2

    fn printResults(self: Self, duration: UInt64, passes: UInt64) -> None:
        try:
            var final_string = (
                "ELucasCurrie_8bit;{0};{1};1;algorithm=base,faithful=yes,bits=8"
            ).format(passes, round(Float32(duration) / 1_000_000_000), 3)
            print(final_string)
        except:
            print("Error formatting results string")


struct prime_sieve_1bit_meta[sieve_size: Int](Runnable):
    alias bitset_size = (sieve_size + 1) // 2
    var array: BitSet[(sieve_size + 1) // 2]

    fn __init__(out self: Self, printable_support: Int):
        self.array = BitSet[self.bitset_size]()
        for i in range(self.bitset_size):
            self.array.set(i)

    fn __call__(self: Self, runtime_sieve_size: Int) -> Self:
        return Self(runtime_sieve_size)

    fn getBit(self, index: Int) -> Bool:
        if index % 2 == 0:
            return False
        return self.array.test(index // 2)

    fn clearBit(mut self: Self, index: UInt):
        self.array.clear(index // 2)

    fn countPrimes(self) -> Int:
        return len(self.array)

    fn run(mut self: Self):
        factor = 3
        q = Int(math.sqrt(sieve_size))

        while factor <= q:
            for num in range(factor, sieve_size):
                if self.getBit(num):
                    factor = num
                    break

            for num in range(factor * 3, sieve_size, factor * 2):
                self.clearBit(num)

            factor += 2

    fn printResults(self: Self, duration: UInt64, passes: UInt64) -> None:
        try:
            var final_string = (
                "ELucasCurrie_1bit_meta;{0};{1};1;algorithm=base,faithful=no,bits=1"
            ).format(passes, round(Float32(duration) / 1_000_000_000), 3)
            print(final_string)
        except:
            print("Error formatting results string")


struct prime_sieve_8bit_meta[sieve_size: Int](Runnable):
    var limit: Int
    var array: InlineArray[UInt8, (sieve_size >> 1)]

    fn __init__(out self: Self, printable_support: Int):
        self.limit = sieve_size >> 1
        self.array = InlineArray[UInt8, (sieve_size >> 1)](fill=0xFF)

    fn __call__(self: Self, runtime_sieve_size: Int) -> Self:
        return Self(runtime_sieve_size)

    fn countPrimes(self) -> Int:
        count = 1
        for i in range(1, self.limit):
            if self.array[i]:
                count += 1
        return count

    @always_inline
    fn run(mut self: Self) -> None:
        var q = Int(math.sqrt(sieve_size))
        var factor = 3
        while factor <= q:
            divisor = factor >> 1
            while not (self.array.unsafe_get(divisor) != 0 or divisor >= q):
                divisor += 1
            factor = (divisor << 1) + 1

            if factor > q:
                break
            start = (factor * factor) >> 1

            while start < self.limit:
                self.array.unsafe_get(start) = 0
                start += factor
            factor += 2

    fn printResults(self: Self, duration: UInt64, passes: UInt64) -> None:
        try:
            var final_string = (
                "ELucasCurrie_8bit_meta;{0};{1};1;algorithm=base,faithful=no,bits=8"
            ).format(passes, round(Float32(duration) / 1_000_000_000), 3)
            print(final_string)
        except:
            print("Error formatting results string")


def run_and_time_sieve[
    type: Runnable
](prime_sieve: type, validation_data: Dict[Int, Int] = {}) -> None:
    var sieve_size: Int = 1_000_000
    sieve = prime_sieve(sieve_size)
    start_time: UInt64 = time.monotonic()
    passes: UInt64 = 0

    while (time.monotonic() - start_time) < 5_000_000_000:
        sieve = prime_sieve(sieve_size)
        sieve.run()
        passes += 1
    duration: UInt64 = time.monotonic() - start_time

    if sieve.countPrimes() != validation_data[sieve_size]:
        print("Error: invalid result!")
        print(
            "Expected {}, got {}".format(
                validation_data[sieve_size], sieve.countPrimes()
            )
        )

    sieve.printResults(duration, passes)


def main() -> None:
    validation_data = {
        10: 4,
        100: 25,
        1_000: 168,
        10_000: 1_229,
        100_000: 9_592,
        1_000_000: 78_498,
        10_000_000: 664_579,
        100_000_000: 5_761_455,
        1_000_000_000: 50_847_534,
        10_000_000_000: 455_052_511,
    }
    alias sieve_size: Int = 1_000_000
    run_and_time_sieve(prime_sieve_1bit_meta[sieve_size](0), validation_data)
    run_and_time_sieve(prime_sieve_8bit_meta[sieve_size](0), validation_data)
    run_and_time_sieve(prime_sieve_1bit(0), validation_data)
    run_and_time_sieve(prime_sieve_8bit(0), validation_data)
