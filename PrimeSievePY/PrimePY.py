# Python Prime Sieve
#
# MyFirstPython Program (tm) Dave Plummer 8/9/2018
#
# Updated 3/22/2021 for Dave's Garage episode comparing C++, C#, and Python

from sys import stdout  # So I can print without an automatic python newline
from math import sqrt  # Used by the sieve
import timeit  # For timing the durations

UPPER_LIMIT = 0  # Upper limit, highest prime we'll consider


class PrimeSieve:
    """This is the main PrimeSieve class. Call it with the number you wish as an 
    upper limit, then call the run_sieve method to do the calculation. 
    print_results will dump the count to check validity."""

    rawbits = None  # Storage for sieve - since we filter evens, just half as many bits

    prime_counts = {
        10: 1,  # Historical data for validating our results - the number of primes
        100: 25,  # to be found under some limit, such as 168 primes under 1000
        1000: 168,
        10000: 1229,
        100000: 9592,
        1000000: 78498,
        10000000: 664579,
        100000000: 5761455,
    }

    def __init__(self, limit=UPPER_LIMIT):
        self.sieve_size = limit
        self.rawbits = [True] * int((self.sieve_size + 1) / 2)

    def validate_results(self):
        """Look up our count of primes in the historical data (if we have it) 
        to see if it matches the data, and (b) our count matches. Since it will 
        return False for an unknown upper_limit, can't assume False == bad"""

        if self.sieve_size in self.prime_counts:  # Check if this is an upper_limit
            return self.prime_counts[self.sieve_size] == self.count_primes()
        return False

    def get_bit(self, index):
        """Gets a bit from the array of bits, but automatically just filters out 
        even numbers as false, and then only uses half as many bits for actual 
        storage"""
        if index % 2 == 0:  # even numbers are by default returned as non-prime
            return False
        return self.rawbits[int(index / 2)]

    def clear_bit(self, index):
        """Reciprocal of get_bit, ignores even numbers and just stores the odds. 
        Since the prime sieve work should never waste time clearing even 
        numbers, this code will assert if you try to"""
        if index % 2 == 0:
            assert (
                "If you're setting even bits, "
                "you're sub-optimal for some reason!"
            )
            return False
        else:
            self.rawbits[int(index / 2)] = False

    # primeSieve
    #
    #

    def run_sieve(self):
        """Calculate the primes up to the specified limit"""
        factor = 3
        q = sqrt(self.sieve_size)

        while factor < q:
            for num in range(factor, self.sieve_size):
                if self.get_bit(num) == True:
                    factor = num
                    break

            # If marking factor 3, you wouldn't mark 6 (it's a mult of 2)
            # so start with the 3rd instance of this factor's multiple.
            # We can then step by factor * 2 because every second one is
            # going to be even by definition

            for num in range(factor * 3, self.sieve_size, factor * 2):
                self.clear_bit(num)

            # No need to check evens, so skip to next odd
            # (factor = 3, 5, 7, 9...)
            factor += 2

    def count_primes(self):
        """Return the count of bits that are still set in the sieve. 
        Assumes you've already called run_sieve, of course!"""
        return sum(1 for b in self.rawbits if b)

    def print_results(self, show_results, duration, passes):
        """Displays the primes found (or just the total count, 
        depending on what you ask for)"""
        if show_results:
            # Since we auto-filter evens,
            # we have to special case the number 2 which is prime
            stdout.write("2, ")

        # Count (and optionally dump) the primes that were found below the limit
        count = 1
        for num in range(3, self.sieve_size):
            if self.get_bit(num) == True:
                if show_results:
                    stdout.write(str(num) + ", ")
                count += 1

        assert count == self.count_primes()
        stdout.write("\n")
        print(
            "Passes: "
            + str(passes)
            + ", Time: "
            + str(duration)
            + ", Avg: "
            + str(duration / passes)
            + ", Limit: "
            + str(self.sieve_size)
            + ", Count: "
            + str(count)
            + ", Valid: "
            + str(self.validate_results())
        )


# MAIN Entry
if __name__ == "__main__":
    start_time = timeit.default_timer()  # Record our starting time
    passes = 0  # We'll count how many passes we make in fixed window of time

    while (
        timeit.default_timer() - start_time < 10
    ):  # Run until more than 10 seconds have elapsed
        sieve = PrimeSieve(1000000)  #  Calc the primes up to a million
        sieve.run_sieve()  #  Find the results
        passes = passes + 1  #  Count this pass

    time_delta = (
        timeit.default_timer() - start_time
    )  # After the "at least 10 seconds", get the actual elapsed

    sieve.print_results(False, time_delta, passes)  # Display outcome
