from array import array
from math import sqrt
from typing import Callable
import timeit

def runSieve(sieveSize: int) -> tuple[Callable[[int], bool], Callable[[], int]]:
    # Create a contiguous array of 64-bit integers to hold the sieve state
    rawbits = [True] * (sieveSize // 2)

    def getBit(index: int):
        # // 128 because we don't care about even indexes
        # Note that indexmasks for even bits will force an exception.
        return rawbits[index // 2]

    def clearBit(index: int):
        nonlocal rawbits # modify the rawbits from the enclosing scope.
        rawbits[index // 2] = False

    def checkPrime(num: int):
        if num < 2:
            return False
        elif num == 2:
            return True
        elif num % 2 == 0:
            return False
        else:
            return getBit(num) and True

    def countPrimes():
        return sum(rawbits)

    factor = 3
    q = int(sqrt(sieveSize))

    for factor in range(3, q, 2):
        if getBit(factor):
            for num in range(factor * 3, sieveSize, factor * 2):
                clearBit(num)

    return checkPrime, countPrimes


def runSieve_bit_array(
        sieveSize: int
) -> tuple[Callable[[int], bool], Callable[[], int]]:
    # Create a contiguous array of 64-bit integers to hold the sieve state
    allbits = 2**64 - 1
    rawbits = array('Q', [allbits] * ((sieveSize + 15) // 16))
    indexmasks = tuple(1 << (x // 2) if x % 2 == 1 else None for x in range(0, 128))
    indexinversemasks = tuple((x if x is None else allbits ^ x) for x in indexmasks)

    def getBit(index: int):
        # // 128 because we don't care about even indexes
        # Note that indexmasks for even bits will force an exception.
        return rawbits[index // 128] & indexmasks[index % 128]

    def clearBit(index: int):
        nonlocal rawbits # modify the rawbits from the enclosing scope.
        rawbits[index // 128] &= indexinversemasks[index % 128]

    def checkPrime(num: int):
        if num < 2:
            return False
        elif num == 2:
            return True
        elif num % 2 == 0:
            return False
        else:
            return getBit(num) and True

    def countPrimes():
        ignored = (len(rawbits) * 128 - sieveSize) // 2
        return sum(bin(x).count("1") for x in rawbits) - ignored

    factor = 3
    q = int(sqrt(sieveSize))

    for factor in range(3, q, 2):
        if getBit(factor):
            for num in range(factor * 3, sieveSize, factor * 2):
                clearBit(num)

    return checkPrime, countPrimes


def validateResults(sieveSize: int, countPrimes: Callable[[], int]) -> bool:
    primeCounts = { 10 : 4,                 # Historical data for validating our
                    100 : 25,               # results - the number of primes to
                    1000 : 168,             # be found under some limit, such as
                    10000 : 1229,           # 168 primes under 1000
                    100000 : 9592,
                    1000000 : 78498,
                    10000000 : 664579,
                    100000000 : 5761455
                  }
    return countPrimes() == primeCounts[sieveSize]

def printResults(
        sieveSize: int,
        checkPrime: Callable[[int], bool],
        countPrimes: Callable[[], int],
        showResults: bool,
        duration: float,
        passes: int
) -> None:
    primes = tuple(num for num in range(2, sieveSize) if checkPrime(num))
    if showResults:
        print(', '.join(str(prime) for prime in primes))
    assert len(primes) == countPrimes()
    print(f"Passes: {passes}, Time: {duration}, Avg: {duration / passes}, Limit: {sieveSize}, Count: {len(primes)}, Valid: {validateResults(sieveSize, countPrimes)}")

def time_and_test():
    tStart = timeit.default_timer()
    sieveSize = 1000000
    passes = 0
    while (timeit.default_timer() - tStart < 10):
        checkPrime, countPrimes = runSieve(sieveSize)
        passes += 1
    tD = timeit.default_timer() - tStart
    printResults(sieveSize, checkPrime, countPrimes, False, tD, passes)

if __name__ == '__main__':
    time_and_test()
