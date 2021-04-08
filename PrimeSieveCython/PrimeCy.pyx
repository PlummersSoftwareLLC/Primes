from numpy import sqrt
from numpy import ones
from numpy import int32
from time import time

# prime_sieve
#
# Does the calculation to determine the number of primes
# smaller than 'limit'
cdef int prime_sieve(int limit):

    rawbits = ones(int(limit/2), dtype=int32)
    cdef int[:] rawbits_view = rawbits
    cdef int factor = 3
    cdef float q = sqrt(limit)
    cdef int num
    cdef int index

    while factor <= q:
        for num in range(factor, limit, 2):
            index = int(num/2)
            if rawbits_view[index]:
                factor = num
                break

        for num in range(factor*3, limit, factor*2):
            index = int(num/2)
            rawbits_view[index] = 0

        factor += 2

    return sum(rawbits_view)

# run
#
# Benchmarking function - runs prime_sieve() as many times as possible
# for 10 seconds. Outputs benchmark results, including total passes, 
# average runtime etc. Compares the count with historical data 
# if possible.
cdef run(int limit):

    primeCounts = { 10 : 4,
                    100 : 25,
                    1000 : 168,
                    10000 : 1229,
                    100000 : 9592,
                    1000000 : 78498,
                    10000000 : 664579,
                    100000000 : 5761455
                  }

    cdef int passes = 0
    cdef double start = time()
    cdef double current = start
    cdef int count = 0
    while current-start < 10:
        count = prime_sieve(limit)
        passes += 1
        current = time()
    cdef double duration = current-start
    valid = True
    if limit in primeCounts and count != primeCounts.get(limit): valid = False
    print("Passes: " + str(passes) + ", Time: " + str(duration) + ", Avg: " + str(duration/passes) + ", Limit: " + str(limit) + ", Count: " + str(count) + ", Valid: " + str(valid))

# call_prime_sieve
#
# Wrapper for the prime_sieve() function in case this python file 
# is imported into another as a module.
def call_prime_sieve(limit):
    return prime_sieve(limit)

# main
# 
# Wrapper for the run() function, so the benchmark can be executed
# from a different python script by importing this one as a module,
# then calling PrimeCy.main()
def main():
    run(10_000)