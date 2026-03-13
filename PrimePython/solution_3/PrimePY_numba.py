"""
Python Prime Sieve using Numba JIT

Based on MyFirstPython Program (tm) Dave Plummer 8/9/2018
Adapted by Emil Sauer Lynge (emillynge) 08/07/2021 (solution_3, numpy)
Further adapted by Tyler/DOC (TylerDOC1776) to use Numba @njit to compile the sieve loop to native machine code.

The core sieve function is decorated with @njit (nopython mode), which compiles
the find-next-prime while loop and the strided slice assignment to native code on
first call. All subsequent calls run at near-C speed with no Python interpreter
overhead.

Note: the first call to run_sieve triggers JIT compilation. The benchmark
warm-up call absorbs this cost before the timer starts.
"""
import numpy as np
from math import sqrt
from numba import njit


@njit
def _run_sieve(bits, size):
    factor = 1
    q = sqrt(size) / 2
    while factor <= q:
        # Find next prime - compiled to a tight native loop
        while not bits[factor]:
            factor += 1
        # Mark composites starting at p^2, stepping by 2p (odd multiples only)
        start = 2 * factor * (factor + 1)
        step  = 2 * factor + 1
        bits[start::step] = False
        factor += 1


class PrimeSieve:

    """This is the main PrimeSieve class. Call it with the number you wish as
    an upper limit, then call the run_sieve method to do the calculation.
    print_results will dump the count to check validity."""

    prime_counts = { 10 : 4,
                    100 : 25,
                    1000 : 168,
                    10000 : 1229,
                    100_000 : 9592,
                    1000000 : 78498,
                    10000000 : 664579,
                    100000000 : 5761455
                    }

    def __init__(self, limit):
        self._size = limit
        self._bits = np.ones(((self._size + 1) // 2), dtype=np.bool_)

    def validate_results(self):
        """Look up our count of primes in the historical data (if we have it)
        to see if it matches"""
        if self._size in self.prime_counts:
            return self.prime_counts[self._size] == self.count_primes()
        return False

    def run_sieve(self):
        """Calculate the primes up to the specified limit"""
        _run_sieve(self._bits, self._size)

    def count_primes(self):
        """Return the count of bits that are still set in the sieve.
        Assumes you've already called run_sieve, of course!"""
        if self._size < 2:
            return 0
        return int(np.sum(self._bits))

    def get_primes(self):
        """Returns the found prime numbers as a numpy array.
        Requires a prior run_sieve call"""
        if self._size < 2:
            return np.array([], dtype=np.int64)
        primes = np.where(self._bits)[0] * 2 + 1
        primes[0] = 2
        return primes

    def print_results(self, show_results, duration, passes):
        """Displays the primes found (or just the total count,
        depending on what you ask for)"""
        count = 0
        for num in self.get_primes():
            count += 1
            if show_results:
                print("%s, " % num, end="")

        if show_results:
            print()
        print("Passes: %s, Time: %s, Avg: %s, Limit: %s, Count: %s, Valid: %s" % (
            passes, duration, duration/passes, self._size, count, self.validate_results()))

        print()
        print("TylerDOC1776_numba;%s;%s;1;algorithm=base,faithful=no,bits=8" % (passes, duration))


# MAIN Entry
if __name__ == "__main__":
    from argparse import ArgumentParser
    from timeit import default_timer

    parser = ArgumentParser(description="Python Prime Sieve (Numba JIT)")
    parser.add_argument("--limit", "-l", help="Upper limit for calculating prime numbers", type=int, default=1_000_000)
    parser.add_argument("--time",  "-t", help="Time limit", type=float, default=5)
    parser.add_argument("--show",  "-s", help="Print found prime numbers", action="store_true")

    args = parser.parse_args()
    limit   = args.limit
    timeout = args.time

    # Warm up the JIT before the timed benchmark
    _warmup = PrimeSieve(limit)
    _warmup.run_sieve()

    time_start = default_timer()
    passes = 0

    while (default_timer() - time_start < timeout):
        sieve = PrimeSieve(limit)
        sieve.run_sieve()
        passes += 1

    time_delta = default_timer() - time_start

    sieve.print_results(args.show, time_delta, passes)
