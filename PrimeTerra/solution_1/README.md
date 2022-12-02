# Terra solution by Enter1he


Non-faithfull solution counting the number of primes up to 1 million. Based on the idea of sorting numbers before
running sieve putting all prime values at the start. Because of that there's no need in marking all non-prime values.

## Run instructions

There's no way to install Terra through the package so the best way is to get
release from a terralang GitHub.
After that to run the program you need a Terra compiler in your PATH

Put the folowing command in terminal:
'''
terra Primes.t
'''
Resulting program would be right in the program folder

## Output
From AMD Ryzen 5 3500U 3.7 Ghz 4 cores
'''
Computing primes to 1000000 on 1 thread for 5 seconds.

 Passes: 79499, Time: 2.547000, Avg: 0.000000, Limit: 1000000, Count: 78498, Valid: 64
'''