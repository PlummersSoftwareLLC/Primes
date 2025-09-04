import time
import math
from collections import BitSet


struct prime_sieve[sieve_size: Int]:
    alias bitset_size = (sieve_size + 1) // 2
    var array: BitSet[(sieve_size + 1) // 2]

    def __init__(out self: Self):
        self.array = BitSet[self.bitset_size]()
        for i in range(self.bitset_size):
            self.array.set(i)

    def getBit(self, index: Int) -> Bool:
        if index % 2 == 0:
            return False
        return self.array.test(index // 2)

    def clearBit(mut self: Self, index: UInt):
        self.array.clear(index // 2)

    def countPrimes(self) -> Int:
        return len(self.array)

    def run(mut self: Self):
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


struct prime_sieve_8bit[sieve_size: Int]:
    var limit: Int
    var array: InlineArray[Bool, (sieve_size >> 1)]

    def __init__(out self: Self):
        self.limit = sieve_size >> 1
        self.array = InlineArray[Bool, (sieve_size >> 1)](fill=True)

    def countPrimes(self) -> Int:
        count = 1
        for i in range(1, self.limit):
            if self.array[i]:
                count += 1
        return count

    @always_inline
    def run(mut self: prime_sieve_8bit):
        var q = Int(math.sqrt(sieve_size))
        var factor = 3
        while factor <= q:
            divisor = factor >> 1
            while not (self.array[divisor] or divisor >= q):
                divisor += 1
            factor = (divisor << 1) + 1

            if factor > q:
                break
            start = (factor * factor) >> 1

            while start < self.limit:
                self.array[start] = False
                start += factor
            factor += 2


def run_and_time_sieve(validation_data: Dict[Int, Int] = {}) -> None:
    alias sieve_size: Int = 1_000_000
    sieve = prime_sieve[sieve_size]()
    start_time: UInt64 = time.monotonic()
    passes: UInt64 = 0

    while (time.monotonic() - start_time) < 5_000_000_000:
        sieve = prime_sieve[sieve_size]()
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

    var final_string = (
        "ELucasCurrie_1Bit;{0};{1};1;algorithm=base,faithful=yes,bit=1"
    ).format(passes, round(Float32(duration) / 1_000_000_000), 3)
    print(final_string)


def run_and_time_sieve_8bit(validation_data: Dict[Int, Int] = {}) -> None:
    alias sieve_size: Int = 1_000_000
    sieve = prime_sieve_8bit[sieve_size]()
    start_time: UInt64 = time.monotonic()
    passes: UInt64 = 0

    while (
        time.monotonic() - start_time
    ) < 5_000_000_000:  # run for at least 5 seconds
        sieve = prime_sieve_8bit[sieve_size]()
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

    var final_string = (
        "ELucasCurrie_8bit;{0};{1};1;algorithm=base,faithful=yes,bit=8"
    ).format(passes, round(Float32(duration) / 1_000_000_000), 3)
    print(final_string)


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
    run_and_time_sieve(validation_data)
    run_and_time_sieve_8bit(validation_data)
