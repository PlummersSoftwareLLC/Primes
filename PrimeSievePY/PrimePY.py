
# Python Prime Sieve
#
# MyFirstPython Program (tm) Dave Plummer 8/9/2018
#
# This is the main prime_sieve class. Call it with the number you wish as an upper limit, then
# call the runSieve method to do the calculation. printResults will dump the count to check validity.
#
# Updated 3/22/2021 for Dave's Garage episode comparing C++, C#, and Python

class prime_sieve(object):

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
        self.size = limit
        self.bits = bytearray(b"\x00\x01" * ((self.size + 1) // 2))

    # Look up our count of primes in the historical data (if we have it) to see if it matches

    def validateResults(self):                      # Check to see if this is an upper_limit we can
        if self.size in self.primeCounts:           # the data, and (b) our count matches. Since it will return
            return self.primeCounts[self.size] == self.countPrimes() # false for an unknown upper_limit, can't assume false == bad
        return False

    # primeSieve
    # 
    # Calculate the primes up to the specified limit

    def runSieve(self):

        factor = 3
        q = self.size**0.5

        while (factor < q):
            factor = self.bits.index(b"\x01", factor)

            # If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
            # We can then step by factor * 2 because every second one is going to be even by definition

            self.bits[factor * 3 :: factor * 2] = b"\x00" * ((self.size - factor * 3) // (factor * 2) + 1)

            factor += 2 # No need to check evens, so skip to next odd (factor = 3, 5, 7, 9...)

    # countPrimes
    #
    # Return the count of bits that are still set in the sieve. Assumes you've already called
    # runSieve, of course!

    def countPrimes(self):
        return sum(self.bits);

    # printResults
    #
    # Displays the primes found (or just the total count, depending on what you ask for)

    def printResults(self, showResults, duration, passes):

        if showResults: # Since we auto-filter evens, we have to special case the number 2 which is prime
            print("2, ", end="");

        count = 1
        for num in range (3, self.size, 2): # Count (and optionally dump) the primes that were found below the limit
            if self.bits[num]:
                if showResults:
                    print(str(num) + ", ", end="")
                count += 1

        assert(count == self.countPrimes())
        print();
        print("Passes: %s, Time: %s, Avg: %s, Limit: %s, Count: %s, Valid: %s" % (passes, duration, duration/passes, self.size, count, self.validateResults()))
  
# MAIN Entry
if __name__ == "__main__":

    from timeit import default_timer                        # For timing the durations

    tStart = default_timer()                                # Record our starting time
    passes = 0                                              # We're going to count how many passes we make in fixed window of time

    while (default_timer() - tStart < 10):                  # Run until more than 10 seconds have elapsed
        sieve = prime_sieve(1_000_000)                      #  Calc the primes up to a million
        sieve.runSieve()                                    #  Find the results
        passes = passes + 1                                 #  Count this pass
        
    tD = default_timer() - tStart                           # After the "at least 10 seconds", get the actual elapsed

    sieve.printResults(False, tD, passes)                   # Display outcome