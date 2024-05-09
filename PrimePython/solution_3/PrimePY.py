"""
Python Prime Sieve using Numpy

Based on MyFirstPython Program (tm) Dave Plummer 8/9/2018
Adapted by Emil Sauer Lynge 08/07/2021

This particular version is based on the PrimePython/solution2, which itself is adapted from Dave Plummer original.
"""
import numpy as np
from math import sqrt


class PrimeSieve:

    """This is the main PrimeSieve class. Call it with the number you wish as
    an upper limit, then call the run_sieve method to do the calculation.
    print_results will dump the count to check validity."""

    prime_counts = { 10 : 4,                 # Historical data for validating our results - the number of primes
                     100 : 25,                # to be found under some limit, such as 168 primes under 1000
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

    @property
    def debug_sieve(self):
        return np.vstack(((np.arange(0, self._bits.size) * 2 + 1) * self._bits , self._bits)).T

    def validate_results(self):                      # Check to see if this is an upper_limit we can

        """Look up our count of primes in the historical data (if we have it)
        to see if it matches"""

        if self._size in self.prime_counts:                              # the data, and (b) our count matches. Since it will return
            return self.prime_counts[self._size] == self.count_primes()  # false for an unknown upper_limit, can't assume false == bad
        return False

    def run_sieve(self):

        """Calculate the primes up to the specified limit"""

        # Rounding to use as an upper index. Adding 1 to account for the square special case.
        q = round(sqrt(self._size) / 2) + 1

        # Builtin functions are faster than custom numerics (in CPython).
        # The `enumerate` function lets us keep track of the current index.
        for factor, bit in enumerate(self._bits[1:q], start=1):
            if bit:
                prime = 2*factor + 1                # The prime number represented by this index.
                start = factor + prime * factor     # Any indices for prime numbers between `factor` and `start`
                                                     # have already been zeroed by previous iterations.
                self._bits[start::prime] = False    # Every `prime`'th odd number is a multiple of `prime`.

    def count_primes(self):

        """Return the count of bits that are still set in the sieve.
        Assumes you've already called run_sieve, of course!"""
        if self._size < 2:
            return 0

        return np.sum(self._bits)

    def get_primes(self):

        """Returns a generator to iterate over the found prime numbers.
        Requires a prior run_sieve call"""
        if self._size < 2:
            return tuple()
        primes = np.where(self._bits)[0] * 2 + 1
        primes[0] = 2
        return primes

    def print_results(self, show_results, duration, passes):

        """Displays the primes found (or just the total count,
        depending on what you ask for)"""

        count = 0
        for num in self.get_primes():  # Count (and optionally dump) the primes that were found below the limit
            count += 1
            if show_results:
                print("%s, " % num, end="")

        if show_results:
            print()
        print("Passes: %s, Time: %s, Avg: %s, Limit: %s, Count: %s, Valid: %s" % (passes, duration, duration/passes, self._size, count, self.validate_results()))

        # Following 2 lines added by rbergen to conform to drag race output format
        print();
        print("emillynge_numpy; %s;%s;1;algorithm=base,faithful=no,bits=8" % (passes, duration));


# MAIN Entry
if __name__ == "__main__":
    from argparse import ArgumentParser
    from timeit import default_timer  # For timing the durations

    parser = ArgumentParser(description="Python Prime Sieve")
    parser.add_argument("--limit", "-l", help="Upper limit for calculating prime numbers", type=int, default=1_000_000)
    parser.add_argument("--time", "-t", help="Time limit", type=float, default=5)
    parser.add_argument("--show", "-s", help="Print found prime numbers", action="store_true")

    args = parser.parse_args()
    limit = args.limit
    timeout = args.time
    show_results = args.show

    time_start = default_timer()                           # Record our starting time
    passes = 0                                             # We're going to count how many passes we make in fixed window of time

    while (default_timer() - time_start < timeout):        # Run until more than time seconds have elapsed
        sieve = PrimeSieve(limit)                          # Calc the primes up to limit
        sieve.run_sieve()                                  # Find the results
        passes = passes + 1                                # Count this pass

    time_delta = default_timer() - time_start              # After the "at least time seconds", get the actual elapsed

    sieve.print_results(show_results, time_delta, passes)  # Display outcome
