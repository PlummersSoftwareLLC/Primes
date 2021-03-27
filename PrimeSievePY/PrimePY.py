
# Python Prime Sieve
#
# MyFirstPython Program (tm) Dave Plummer 8/9/2018
#
# This is the main PrimeSieve class. Call it with the number you wish as an upper upper_limit, then
# call the run_sieve method to do the calculation. __repr__ will dump the count to check validity.
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

class PrimeSieve:
    _raw_bits = None  # Storage for sieve - since we filter evens, just half as many bits
    _sieve_size = 0  # Upper upper_limit, highest prime we'll consider

    def __init__(self, upper_limit: int):
        self._sieve_size = upper_limit
        self._raw_bits: list[bool] = [True] * ((self._sieve_size + 1) // 2)

    def _validate_results(self) -> bool:
        """
        Checks result of sieve

        :return: True if sieve size is historical data and it matches for actual count of primes
        """
        return PRIME_COUNTS.get(self._sieve_size) == self.count_primes()

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

        return self._raw_bits[index // 2]

    def _set_to_false(self, index: int) -> None:
        """
        Sets bit of list at index to False, ignores even indexes

        :raises AssertionError: When try to set to False at even index
        :param index: int
        """
        assert index % 2 != 0, 'If you`re setting even bits, you`re sub-optimal for some reason!'
        self._raw_bits[index // 2] = False

    def run_sieve(self) -> None:
        """
        Calculate the primes up to the specified upper_limit
        """
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

    def count_primes(self) -> int:
        """
        Length of only primes numbers

        :return: int Number of primes, calculated for specific upper_limit
        """
        return sum(1 for b in self._raw_bits if b)

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






