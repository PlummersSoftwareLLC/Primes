"""
Python Prime Sieve

MyFirstPython Program (tm) Dave Plummer 8/9/2018

This is the main prime_sieve class. Call it with the number you wish as an upper limit, then
call the runSieve method to do the calculation. printResults will dump the count to check validity.

Updated 3/22/2021 for Dave's Garage episode comparing C++, C#, and Python

Dave this is my attempt at a half pythonic / half C version.
For a reference on more pythonic style coding I reccomend "Python Cookbook" by David Beazley.
Also any talks on YouTube by David Beazley or Raymond Hettinger are usually pretty good.
That said, using pure python wouldn't really be consider a good idea in this case.  You would want to use
one or possibly more of: Numpy, Numba, ctypes, or Cython. 
"""

from math import sqrt                       
from time import time                       

class PrimeSieve():

    #rawbits = None   # Dave these class variables are redundant since you always redefine instance variables in __init__ 
    #sieveSize = 0    
    prime_counts = { 10 : 5,                # Dave I believe there are actually 5 primes under 10
                    100 : 25,               
                    1000 : 168,
                    10000 : 1229,
                    100000 : 9592,
                    1000000 : 78498,
                    10000000 : 664579,
                    100000000 : 5761455
                  }

    def __init__(self, n):
        self.sieve_size = n
        self.rawbits = bytearray([255] * (n // 8 + 1))  # bytearray() is mutable. bytes() is immutable.

    def validate_results(self):                      # Return a default value False for any size not in prime_counts
        return PrimeSieve.prime_counts.get(self.sieve_size, False) == self.count_primes() 

    def get_bit(self, index):
        if (index % 2 == 0): 
            return False
        index = index // 2      # // is integer division
        return ((self.rawbits[index // 8]) & (1 << (index % 8))) != 0 # Not great python but probably easier for a C dev

    def clear_bit(self, index):
        if (index % 2 == 0):
            print("You're setting even bits, which is sub-optimal.")
            return False
        index = index // 2
        self.rawbits[index // 8] &= ~(1 << (index % 8))

    def run_sieve(self):
        factor = 3
        q = sqrt(self.sieve_size)
        while (factor < q):
            for num in range (factor, self.sieve_size):
                if self.get_bit(num):
                    factor = num
                    break

            for num in range (factor * 3, self.sieve_size, factor * 2): 
                self.clear_bit(num)

            factor += 2 

    def count_primes(self):
        return sum(self.get_bit(i) for i in range(self.sieve_size))

    def print_results(self, show_results, duration, passes):
        if (show_results): 
            print("2,", end = " ")    # Print without a newline.

        count = 1
        for num in range (3, self.sieve_size): 
            if self.get_bit(num):
                if show_results:
                    print(f"{num},", end = " ")   # Print without a newline.
                count+=1

        print("")
        print(f"Passes: {passes}, Time: {duration}, Avg: {duration/passes}, Limit: {self.sieve_size}, Count: {count}, Valid: {self.validate_results()}")
  
if __name__ == '__main__':
    start_time = time()
    passes = 0                                              

    while time() - start_time < 10:           
        sieve = PrimeSieve(1000000)                
        sieve.run_sieve()                     
        passes += 1                           
        
    elapsed_time = time() - start_time         

    sieve.print_results(False, elapsed_time, passes)       