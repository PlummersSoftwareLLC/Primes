import numpy as np
import math
import timeit

# -----------------------------------------------------------------------------
primeCounts = { 10 : 1,
                100 : 25,
                1000 : 168,
                10000 : 1229,
                100000 : 9592,
                1000000 : 78498,
                10000000 : 664579,
                100000000 : 5761455
              }

# -----------------------------------------------------------------------------
def sieve(n):

    bits = np.ones(n, bool)
    bits[0] = bits[1] = False

    b = math.ceil(math.sqrt(n))
    for i in range(2,b):
        if bits[i]:
            bits[i*i::i] = False

    return bits

# -----------------------------------------------------------------------------
def print_results(tD, passes, bits, limit):

    primes = np.flatnonzero(bits)
    print("Passes:{}, Time:{}, Avg:{}, Limit:{}, Count: {}, Valid: {}".format(
        passes, tD, tD/passes, limit, len(primes), len(primes) == primeCounts[limit] 
    ))
# -----------------------------------------------------------------------------
if __name__ == "__main__":

    limit, passes = 1_000_000, 0

    tStart = timeit.default_timer()
    while (timeit.default_timer() - tStart < 10):
        bits = sieve(limit)
        passes = passes + 1
    tD = timeit.default_timer() - tStart

    print_results(tD, passes, bits, limit)
