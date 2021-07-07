# Python Prime Sieve
#
# MyFirstPython Program (tm) Dave Plummer 8/9/2018
#
# This is the main PrimeSieve class. Call it with the number you wish as an upper limit, then
# call the run_sieve method to do the calculation. print_results will dump the count to check validity.
#
# Updated 3/22/2021 for Dave's Garage episode comparing C++, C#, and Python
# Updated 7/7/2021 to be more pythonic/efficient

from math import sqrt  # Used by the sieve
import timeit  # For timing the durations


PRIMECOUNTS = {
    10: 4,  # Historical data for validating our results - the number of primes
    100: 25,  # to be found under some limit, such as 168 primes under 1000
    1000: 168,
    10000: 1229,
    100000: 9592,
    1000000: 78498,
    10000000: 664579,
    100000000: 5761455,
}


class PrimeSieve(object):
    # more of a memory optimization, but doesn't hurt to demo!
    __slots__ = ["sieve_size", "raw_bits"]

    def __init__(self, limit):
        # Upper limit, highest prime we'll consider
        self.sieve_size = limit
        # Storage for sieve - since we filter evens, just half as many bits
        self.raw_bits = [True] * ((self.sieve_size + 1) // 2)

    def validate_results(self):
        """Look up our count of primes in the historical data (if we have it) to see if it matches"""
        # Check to see if this is an upper_limit we can
        if self.sieve_size in PRIMECOUNTS:
            # the data, and (b) our count matches.
            return PRIMECOUNTS[self.sieve_size] == self.count_primes()
        # Since it will return false for an unknown upper_limit, can't assume false == bad
        return False

    def get_bit(self, index):
        """Gets a bit from the array of bits, but automatically just filters out even numbers as false,
        and then only uses half as many bits for actual storage
        """
        if index % 2 != 0:  # even numbers are automaticallty returned as non-prime
            return self.raw_bits[index // 2]
        else:
            return

    def clear_bit(self, index):
        """Reciprocal of get_bit, ignores even numbers and just stores the odds. Since the prime sieve work should
        never waste time clearing even numbers
        """
        if index % 2 != 0:
            self.raw_bits[index // 2] = False
        else:
            return

    def run_sieve(self):
        """Calculate the primes up to the specified limit"""
        factor = 3
        q = sqrt(self.sieve_size)

        while factor < q:
            for num in range(factor, self.sieve_size):
                if self.get_bit(num):
                    factor = num
                    break

            # If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
            # We can then step by factor * 2 because every second one is going to be even by definition

            for num in range(factor * 3, self.sieve_size, factor * 2):
                self.clear_bit(num)

            factor += 2  # No need to check evens, so skip to next odd (factor = 3, 5, 7, 9...)

    def count_primes(self):
        """Return the count of bits that are still set in the sieve.
        Assumes you've already called run_sieve, of course!
        """
        return sum(1 for b in self.raw_bits if b)

    def print_results(self, showResults, duration, passes):
        """Displays the primes found (or just the total count, depending on what you ask for)"""
        if showResults:
            # Since we auto-filter evens, we have to special case the number 2 which is prime
            print("2, ", sep="")

        count = 1
        for num in range(3, self.sieve_size):
            # Count (and optionally dump) the primes that were found below the limit
            if self.get_bit(num):
                if showResults:
                    print(f"{num}, ", sep="")
                count += 1

        assert count == self.count_primes()
        # Following 2 lines added by rbergen to conform to drag race output format
        print(
            f"\nPasses: {passes}, Time: {duration}, Avg: {duration/passes}, Limit: {self.sieve_size}, Count: {count}, Valid: {self.validate_results()}"
        )
        print(f"\ndavepl;{passes};{duration};1;algorithm=base,faithful=yes")


# MAIN Entry

t_start = timeit.default_timer()  # Record our starting time
passes = 0  # We're going to count how many passes we make in fixed window of time

# Run until more than 10 seconds have elapsed
while timeit.default_timer() - t_start < 10:
    sieve = PrimeSieve(1000000)  #  Calc the primes up to a million
    sieve.run_sieve()  #  Find the results
    passes = passes + 1  #  Count this pass

# After the "at least 10 seconds", get the actual elapsed
tD = timeit.default_timer() - t_start

sieve.print_results(False, tD, passes)  # Display outcome
