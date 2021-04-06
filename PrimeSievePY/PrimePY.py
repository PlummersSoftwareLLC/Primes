# Python Prime Sieve
#
# MyFirstPython Program (tm) Dave Plummer 8/9/2018
#
# This is the main prime_sieve class. Call it with the number you wish as an upper limit, then
# call the runSieve method to do the calculation. printResults will dump the count to check validity.
#
# Updated 3/22/2021 for Dave's Garage episode comparing C++, C#, and Python

from math import sqrt  # Used by the sieve
import time  # For timing durations
from typing import ClassVar, Dict  # For specifying types of variables
import itertools  # For chaining iterators
from numba import njit, b1, int32
import numpy as np
from functools import lru_cache


class PrimeSieve:
    rawbits: np.ndarray
    """
    Contents of the sieve.
    We only store odd numbers, as they are the only interesting ones.
    """

    sieve_size: int
    """
    Highest prime we'll consider
    """

    prime_counts: ClassVar[Dict[int, int]] = {
        10: 1,
        100: 25,
        1000: 168,
        10000: 1229,
        100000: 9592,
        1000000: 78498,
        10000000: 664579,
        100000000: 5761455
    }
    """
    Historical data for validating our results (the number of primes to be found under some limit,
    such as 168 primes under 1000
    """

    def __init__(self, limit):
        self.sieve_size = limit

    def validate_results(self):
        """
        Check whether the number of found primes is correct
        :return: whether or not the number of primes found is correct.
        """
        if self.sieve_size in self.prime_counts:  # the data, and (b) our count matches. Since it will return
            return self.prime_counts[
                       self.sieve_size] == self.count_primes()  # false for an unknown upper_limit, can't assume false == bad
        return False

    def get_bit(self, index):
        """
        Gets a bit from the array of bits.
        :param index:
        """
        return self.rawbits[index // 2]

    @staticmethod
    @lru_cache(maxsize=2048)
    @njit(b1[:](int32))
    def sieve(sieve_size):
        factor = 3
        q = sqrt(sieve_size)
        rawbits = np.full(sieve_size // 2, True, b1)

        while factor < q:
            for num in range(factor, sieve_size, 2):
                if rawbits[num // 2]:
                    factor = num
                    break

            for num in range(factor * 3, sieve_size, factor * 2):
                rawbits[num // 2] = False
            factor += 2
        return rawbits

    def run_sieve(self):
        """
        Calculate the primes up to the specified limit
        """
        self.rawbits = PrimeSieve.sieve(self.sieve_size)

    def count_primes(self):
        """
        :return: the count of bits that are still set in the sieve, being the amount of primes.
        """
        return np.count_nonzero(self.rawbits)

    def print_results(self, show_results, duration, passes):
        """
        Displays the primes found (or just the total count, depending on what you ask for)

        :param show_results: Show all primes
        :param duration:  The total time execution took
        :param passes:  The amount of passes made
        """

        if show_results:
            print(", ".join(
                map(str,
                    itertools.chain([2], filter(
                        lambda n: self.rawbits[n // 2],
                        range(3, self.sieve_size, 2)))
                    )
            ))

        print("Passes: " + str(passes) + ", Time: " + str(duration) + ", Avg: " + str(
            duration / passes) + ", Limit: " + str(self.sieve_size) + ", Count: " + str(
            self.count_primes()) + ", Valid: " + str(self.validate_results()))


# Entrypoint
if __name__ == '__main__':
    tStart = time.time()  # Record our starting time
    passes = 0  # We're going to count how many passes we make in fixed window of time

    while time.time() - tStart < 10:  # Run until more than 10 seconds have elapsed
        sieve = PrimeSieve(1000000)  # Calc the primes up to a million
        sieve.run_sieve()  # Find the results
        passes += 1  # Count this pass

    tD = time.time() - tStart  # After the "at least 10 seconds", get the actual elapsed

    sieve.print_results(False, tD, passes)  # Display outcome
    
    
# Results:
###
# Original:                             56 passes
# Using time.time as opposed to timeit: 65 passes
# Adding a better counting method:      69 passes
# Finding next prime with increment 2:  85 passes
# More efficient division by two:       114 passes
# Inline clear_bit:                     209 passes
# Naive JIT:                            2020 passes
# LRU Caching                           4024627 passes
# Proper JIT with numpy:                10301 passes
