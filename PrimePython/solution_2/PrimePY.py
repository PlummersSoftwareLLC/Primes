"""
Python Prime Sieve

MyFirstPython Program (tm) Dave Plummer 8/9/2018
Updated 3/22/2021 for Dave's Garage episode comparing C++, C#, and Python
"""

from math import sqrt


class PrimeSieve:

    """This is the main PrimeSieve class. Call it with the number you wish as
    an upper limit, then call the run_sieve method to do the calculation.
    print_results will dump the count to check validity."""

    prime_counts = { 10 : 4,                 # Historical data for validating our results - the number of primes
                    100 : 25,                # to be found under some limit, such as 168 primes under 1000
                    1000 : 168,
                    10000 : 1229,
                    100000 : 9592,
                    1000000 : 78498,
                    10000000 : 664579,
                    100000000 : 5761455
                  }

    def __init__(self, limit):
        self._size = limit
        self._bits = bytearray(b"\x01") * ((self._size + 1) // 2)

    def validate_results(self):                      # Check to see if this is an upper_limit we can

        """Look up our count of primes in the historical data (if we have it)
        to see if it matches"""

        if self._size in self.prime_counts:                              # the data, and (b) our count matches. Since it will return
            return self.prime_counts[self._size] == self.count_primes()  # false for an unknown upper_limit, can't assume false == bad
        return False

    def run_sieve(self):

        """Calculate the primes up to the specified limit"""

        factor = 1
        # sqrt doesn't seem to make any difference in CPython,
        # but works much faster than "x**.5" in Pypy
        q = sqrt(self._size) / 2
        bitslen = len(self._bits)

        while factor <= q:
            factor = self._bits.index(b"\x01", factor)

            # If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
            # We can then step by factor * 2 because every second one is going to be even by definition
            start = 2 * factor * (factor + 1)
            step  = factor * 2 + 1
            size  = bitslen - start
            self._bits[start :: step] = b"\x00" * (size // step + bool(size % step))  # bool is (a subclass of) int in python

            factor += 1

    def count_primes(self):

        """Return the count of bits that are still set in the sieve.
        Assumes you've already called run_sieve, of course!"""

        return self._bits.count(b"\x01") if self._size > 1 else 0

    def get_primes(self):

        """Returns a generator to iterate over the found prime numbers.
        Requires a prior run_sieve call"""

        if self._size > 1:
            yield 2  # Since we auto-filter evens, we have to special case the number 2 which is prime
        if self._size > 2:
            num = 1
            while num > 0:
                yield num * 2 + 1
                num = self._bits.find(1, num + 1)

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
        print("ssovest; %s;%s;1;algorithm=base,faithful=yes,bits=8" % (passes, duration));


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

    while (default_timer() - time_start < timeout):        # Run until more than 10 seconds have elapsed
        sieve = PrimeSieve(limit)                          # Calc the primes up to a million
        sieve.run_sieve()                                  # Find the results
        passes = passes + 1                                # Count this pass

    time_delta = default_timer() - time_start              # After the "at least 10 seconds", get the actual elapsed

    sieve.print_results(show_results, time_delta, passes)  # Display outcome
