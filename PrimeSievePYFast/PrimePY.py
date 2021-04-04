# Author: Alexandru Pisarenco (github.com/apisarenco)
#
# Based on Dave Plummer's code, for Dave's Garage episode comparing C++, C#, and Python

from math import sqrt  # Used by the sieve
import timeit  # For timing the durations
from typing import Iterable

primeCounts = {
    10: 1,  # Historical data for validating our results - the number of primes
    100: 25,  # to be found under some limit, such as 168 primes under 1000
    1000: 168,
    10000: 1229,
    100000: 9592,
    1000000: 78498,
    10000000: 664579,
    100000000: 5761455
}


def init_raw_bits(limit: int) -> bytearray:
    sieveSize = limit + 1
    return bytearray(0xFF for _ in range((sieveSize >> 4) + 1))


def validate_result(raw_bits: bytearray, limit: int) -> bool:
    """ Check to see if this is an upper_limit we can
    """
    if limit in primeCounts:  # the data, and (b) our count matches. Since it will return
        return primeCounts[limit] == count_primes(
            raw_bits, limit
        )  # false for an unknown upper_limit, can't assume false == bad
    return False


def byte_index(index: int) -> int:
    return index >> 4


def bit_index(index: int) -> int:
    return (index >> 1) - (byte_index(index) << 3)


def get_bit(raw_bits: bytearray, index: int) -> bool:
    """ Gets a bit from the array of bits, but automatically just filters out even numbers
    as false, and then only uses half as many bits for actual storage
    """
    if index & 1 == 0:  # even numbers are automaticallty returned as non-prime
        return False
    else:
        return raw_bits[byte_index(index)] & (1 << bit_index(index)) != 0


def clear_bit(raw_bits: bytearray, index: int):
    """ Reciprocal of get_bit, ignores even numbers and just stores the odds.
    Since the prime sieve work should never waste time clearing even numbers,
    this code will assert if you try to
    """
    if index & 1 == 0:
        return False
    else:
        raw_bits[byte_index(index)] &= 0xFF ^ (1 << bit_index(index))


def get_primes(raw_bits: bytearray, limit: int) -> Iterable[int]:
    if limit > 2:
        yield 2
    for b in range(3, limit + 1, 2):
        if raw_bits[byte_index(b)] & (1 << bit_index(b)):
            yield b


def run_sieve(raw_bits: bytearray, limit: int):
    """ Calculate the primes up to the specified limit
    """
    factor = 3
    q = sqrt(limit + 1)

    while factor < q:
        for num in range(factor, limit + 1, 2):
            if get_bit(raw_bits, num):
                factor = num
                break

        # If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
        # We can then step by factor * 2 because every second one is going to be even by definition

        for num in range(factor * 3, limit + 1, factor * 2):
            clear_bit(raw_bits, num)

        factor += 2  # No need to check evens, so skip to next odd (factor = 3, 5, 7, 9...)


def count_primes(raw_bits: bytearray, limit: int) -> int:
    """ Return the count of bits that are still set in the sieve.
    Assumes you've already called run_sieve, of course!
    """
    return sum(1 for _ in get_primes(raw_bits, limit))


def print_results(*, raw_bits, show_results, duration, passes, limit):
    """ Displays the primes found (or just the total count, depending on what you ask for)
    """
    if show_results:
        print(", ".join(num
                        for num in get_primes(raw_bits=raw_bits, limit=limit)))
    count = sum(1 for _ in get_primes(raw_bits=raw_bits, limit=limit))
    print(
        f"Passes: {passes}, Time: {duration}, Avg: {duration/passes}, Limit: {limit}, "
        f"Count: {count}, Valid: {validate_result(raw_bits, limit=limit)}")


def main():
    tStart = timeit.default_timer()  # Record our starting time
    passes = 0  # We're going to count how many passes we make in fixed window of time
    limit = 1_000_000
    # Run until more than 10 seconds have elapsed
    while timeit.default_timer() - tStart < 10:
        raw_bits = init_raw_bits(limit)  #  Calc the primes up to a million
        run_sieve(raw_bits, limit=limit)  #  Find the results
        passes = passes + 1  #  Count this pass

    # After the "at least 10 seconds", get the actual elapsed
    tD = timeit.default_timer() - tStart

    print_results(
        raw_bits=raw_bits,
        show_results=False,
        duration=tD,
        passes=passes,
        limit=limit,
    )


# Try numba
try:
    import numba
except ImportError:
    ...  # No numba :(
else:
    run_sieve = numba.jit(run_sieve, nopython=True)
    clear_bit = numba.jit(clear_bit, nopython=True)
    byte_index = numba.jit(byte_index, nopython=True)
    bit_index = numba.jit(bit_index, nopython=True)
    get_bit = numba.jit(get_bit, nopython=True)

if __name__ == '__main__':
    main()
