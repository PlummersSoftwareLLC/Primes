"""
Python Prime Sieve

MyFirstPython Program (tm) Dave Plummer 8/9/2018

This is the main prime_sieve class. Call it with the number you wish as an upper limit, then
call the runSieve method to do the calculation. printResults will dump the count to check validity.

Updated 3/22/2021 for Dave's Garage episode comparing C++, C#, and Python
"""

from sys import stdout                      # So I can print without an automatic python newline
from math import sqrt                       # Used by the sieve
import timeit                               # For timing the durations

class PrimeSieve(object):
    """ Prime number sieve. """

    rawbits = None    # Storage for sieve - since we filter evens, just half as many bits
    sieve_size = 0    # Upper limit, highest prime we'll consider

    primeCounts = { 10 : 1,                 # Historical data for validating our results - the number of primes
                    100 : 25,               # to be found under some limit, such as 168 primes under 1000
                    1000 : 168,
                    10000 : 1229,
                    100000 : 9592,
                    1000000 : 78498,
                    10000000 : 664579,
                    100000000 : 5761455
                  }

    def __init__(self, limit):
        self.sieve_size = limit
        self.rawbits = [True] * (int((self.sieve_size+1)/2))

    # Look up our count of primes in the historical data (if we have it) to see if it matches

    def validate_results(self):
        """
        Check to see if this is an upper_limit we can
        the data, and (b) our count matches. Since it will return
        false for an unknown upper_limit, can't assume false == bad
        """
        if self.sieve_size in self.primeCounts:
            return self.primeCounts[self.sieve_size] == self.count_primes()
        return False

    def get_bit(self, index):
        """
        Gets a bit from the array of bits, but automatically just filters out even numbers as false,
        and then only uses half as many bits for actual storage
        """

        if index % 2 == 0: # even numbers are automaticallty returned as non-prime
            return False
        else:
            return self.rawbits[int(index/2)]

    def clear_bit(self, index):
        """
        Reciprocal of GetBit, ignores even numbers and just stores the odds. Since the prime sieve work should
        never waste time clearing even numbers, this code will assert if you try to
        """

        assert index % 2 != 0, "If you're setting even bits, you're sub-optimal for some reason!"
        self.rawbits[int(index/2)] = False

    def run_sieve(self):
        """ Calculate the primes up to the specified limit """

        factor = 3
        max_factor = sqrt(self.sieve_size)

        while factor < max_factor:
            for num in range (factor, self.sieve_size):
                if self.get_bit(num) is True:
                    factor = num
                    break

            # If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
            # We can then step by factor * 2 because every second one is going to be even by definition

            for num in range (factor * 3, self.sieve_size, factor * 2):
                self.clear_bit(num)

            factor += 2 # No need to check evens, so skip to next odd (factor = 3, 5, 7, 9...)

    def count_primes(self):
        """
        Return the count of bits that are still set in the sieve. Assumes you've already called
        runSieve, of course!
        """
        return sum(1 for b in self.rawbits if b)

    def print_results(self, show_results, duration, num_passes):
        """ Displays the primes found (or just the total count, depending on what you ask for) """

        if show_results: # Since we auto-filter evens, we have to special case the number 2 which is prime
            stdout.write("2, ")

        count = 1
        for num in range (3, self.sieve_size): # Count (and optionally dump) the primes that were found below the limit
            if self.get_bit(num) is True:
                if show_results:
                    stdout.write(str(num) +", ")
                count+=1

        assert count == self.count_primes()
        stdout.write("\n")
        print("Passes: " + str(num_passes) + ", Time: " + str(duration) + ", Avg: " + str(duration/num_passes) + ", Limit: " + str(self.sieve_size) + ", Count: " + str(count) + ", Valid: " + str(self.validate_results()))

# MAIN Entry

tStart = timeit.default_timer()                         # Record our starting time
passes = 0                                              # We're going to count how many passes we make in fixed window of time

while timeit.default_timer() - tStart < 10:             # Run until more than 10 seconds have elapsed
    sieve = PrimeSieve(1000000)                         #  Calc the primes up to a million
    sieve.run_sieve()                                   #  Find the results
    passes = passes + 1                                 #  Count this pass

tD = timeit.default_timer() - tStart                    # After the "at least 10 seconds", get the actual elapsed

sieve.print_results(False, tD, passes)                  # Display outcome
