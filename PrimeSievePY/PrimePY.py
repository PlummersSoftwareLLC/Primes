
# Python Prime Sieve
#
# MyFirstPython Program (tm) Dave Plummer 8/9/2018
#
# This is the main prime_sieve class. Call it with the number you wish as an upper limit, then
# call the runSieve method to do the calculation. printResults will dump the count to check validity.
#
# Updated 3/22/2021 for Dave's Garage episode comparing C++, C#, and Python

from math import sqrt  # Used by the sieve
import timeit  # For timing the durations

# Historical data for validating our results - the number of primes
# to be found under some upper_limit, such as 168 primes under 1000
PRIME_COUNTS = {10: 1,
                100: 25,
                1000: 168,
                10000: 1229,
                100000: 9592,
                1000000: 78498,
                10000000: 664579,
                100000000: 5761455
                }

    def __init__(this, limit):
        this.sieveSize = limit
        this.rawbits = [True] * (int((this.sieveSize+1)/2))

    # Look up our count of primes in the historical data (if we have it) to see if it matches

    def validateResults(this):                      # Check to see if this is an upper_limit we can
        if this.sieveSize in this.primeCounts:      # the data, and (b) our count matches. Since it will return
            return this.primeCounts[this.sieveSize] == this.countPrimes() # false for an unknown upper_limit, can't assume false == bad
        return False

    # GetBit
    # 
    # Gets a bit from the array of bits, but automatically just filters out even numbers as false,
    # and then only uses half as many bits for actual storage

    def GetBit(this, index):

    def __getitem__(self, index: int) -> bool:
        """
        Gets a bit from the list of bits

        :param index: int Index in list
        :return: Value of the list at index, or False for even index
        """
        if index % 2 == 0:
            return False
        else:
            return this.rawbits[int(index/2)]

    # ClearBit
    #
    # Reciprocal of GetBit, ignores even numbers and just stores the odds. Since the prime sieve work should
    # never waste time clearing even numbers, this code will assert if you try to

    def ClearBit(this, index):

        if (index % 2 == 0):
            assert("If you're setting even bits, you're sub-optimal for some reason!")
            return False
        else:
            this.rawbits[int(index/2)] = False

    # primeSieve
    # 
    # Calculate the primes up to the specified limit

    def runSieve(this):

        factor = 3
        q = sqrt(this.sieveSize)

        while (factor < q):
            for num in range (factor, this.sieveSize):
                if (this.GetBit(num) == True):
                    factor = num
                    break

            # If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
            # We can then step by factor * 2 because every second one is going to be even by definition

            for num in range (factor * 3, this.sieveSize, factor * 2): 
                this.ClearBit(num)

            factor += 2 # No need to check evens, so skip to next odd (factor = 3, 5, 7, 9...)

    # countPrimes
    #
    # Return the count of bits that are still set in the sieve. Assumes you've already called
    # runSieve, of course!

    def countPrimes(this):
        return sum(1 for b in this.rawbits if b);

    # printResults
    #
    # Displays the primes found (or just the total count, depending on what you ask for)

    def printResults(this, showResults, duration, passes):

        if (showResults): # Since we auto-filter evens, we have to special case the number 2 which is prime
            stdout.write("2, ");

    def __repr__(self) -> str:
        """
        Formal string representation

        :return: str that contains main information about results
        """
        return f'{self.__class__.__name__}[found {self.count_primes()} primes less than {self._sieve_size}, is valid=\
{self._validate_results()}] '

    def __str__(self) -> str:
        """
        All found primes

        :return: str that contains all found primes during algorithm
        """
        return f'{", ".join(str(num) for num in range(1, self._sieve_size) if self[num])}'

    __call__ = run_sieve
    __len__ = count_primes


sieve.printResults(False, tD, passes)                   # Display outcome






