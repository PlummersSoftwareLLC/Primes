# The idea

The idea is that if the sieve is ready up to `n` we can use multiple (let say `k`) go routines to fill the sieve between `n` and `n*n`. So we split this space `[n:n*n)` to `k` non overlapping chunks and each of this workers (go routine) reads from the shared memory `[0:n)` and writes to its own chunk. In this way there is no need to use concurrent access to the memory.

# The sieve

In the sieve we encode only the odd numbers bigger that 3. So an odd number `n` is encoded as prime iff `Sieve[(n-3)/2]` is `false`. Because a `boolean` in go occupies 8 bits, we can say that our sieve is a 4 bits per number (because we save only the odd ones).

# Where to start

We make no assumptions about the first odd primes. We do not suppose that 3,5,7 or 11 are primes !

As the first odd prime is **at least** 3, so the base algorithm starts **at least** at 3<sup>2</sup> = 9. Using this we know that the sieve is ready upto 9 (excluded). And as 9 is a square, it is not prime, the smallest prime above 9 is **at least** 11. So we can start filling the sieve using the odd pries inside [3,9) to build the sieve up to 11<sup>2</sup>=121 (excluded), so by filling [9,121).

But it is more efficient if the last chunk is as biggest as possible, and so the best solution is to start filling `[9:q)` where q<sup>2r</sup> is slightly bigger then the maximal limit. The right choice of `q` is really important.

# How much workers to use

Using the go benchmarking, on my computer, the best the best results for a sieve up to 1,000,000 are obtained with 3 times the number of the available cpu cores. But to build for example up to 10,000,000 it is better to use 4 times the number of the available cpu cores.

So I fix the default value to `3*runtime.NumCPU()` but this can be changed using the `-routines` cli flag.

