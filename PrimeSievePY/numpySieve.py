import numpy as np
import math
import timeit

def runSieve(number): # 0.54
    prime = np.full(math.floor((number-1)/2), True) # index n is number 2n + 1
    # 1 isnt prime
    prime[0] = False
    # Check from 3
    i = 1
    limit = math.sqrt(number)
    while i < limit:
        if prime[i]:
            prime[i*3 + 1::2*i + 1] = False
        i += 1

    return prime

def getPrimes(number):
    sieve_result = runSieve(number)
    return np.insert((np.argwhere(sieve_result)*2+1).T[0], 0, 2)

known_values = {
    10: 4, 
    100: 25, 
    1000: 168, 
    10000: 1229, 
    100000: 9592, 
    1000000: 78498, 
    10000000: 664579,
    100000000: 5761455,
    1000000000: 50847534,
}

def validate():
    for num, primes in known_values.items():
        start = timeit.default_timer()
        result = runSieve(num)
        end = timeit.default_timer()
        print(f"{num} took {end-start} seconds and returned {len(result)} primes, the true value is {primes} which is {len(result) == primes}")


def timefunc(func):
    print("Start timing process")
    num_itters = 1_000_000
    primes = []
    tStart = timeit.default_timer()                         # Record our starting time
    passes = 0                                              # We're going to count how many passes we make in fixed window of time
    while (timeit.default_timer() - tStart < 10):           # Run until more than 10 seconds have elapsed
        primes = func(num_itters)                           #  Find the results
        passes = passes + 1                                 #  Count this pass

    tend = timeit.default_timer()
    duration = tend-tStart

    print(f"Function: {func.__name__}, Passes: {passes}, Time: {duration}, Avg: {duration/passes}, Limit: {num_itters}, Count: {passes}, Valid: {len(primes) == known_values.get(num_itters) or sum(primes)+1 == known_values.get(num_itters)}")  # +1 because 2 isnt in the array
    # Function: runSieve, Passes: 12468, Time: 10.0003159, Avg: 0.0008020785931985885, Limit: 1000000, Count: 12468, Valid: True
    # Function: getPrimes, Passes: 3899, Time: 10.001967299999999, Avg: 0.0025652647601949216, Limit: 1000000, Count: 3899, Valid: True


timefunc(runSieve)
timefunc(getPrimes)