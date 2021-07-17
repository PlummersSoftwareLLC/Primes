# The idea

The idea is that if the sieve is ready up to `n` we can use multiple (let say `k`) go routines to fill the sieve between `n` and `n*n`. So we split this space `[n:n*n)` to `k` non overlapping chunks and each of this workers (go routine) reads from the shared memory `[0:n)` and writes to its own chunk. In this way there is no need to use concurrent access to the memory.

# The sieve

In the sieve we encode only the odd numbers bigger that 3. So an odd number `n` is encoded as prime iff `Sieve[(n-3)/2]` is `false`. Because a `boolean` in go occupies 8 bits, we can say that our sieve is a 4 bits per number (because we save only the odd ones).

# Where to start

Because the first three odd numbers (3, 5 and 7) are primes, we can consider that the sieve is ready up to 7 and start to fill the region `[7:121)`. But it is more efficient if the last chunk is as biggest as possible, and so the best solution is to start filling `[7:q)` where q<sup>2r</sup> is slightly bigger then the maximal limit. The right choice of `q` is really important.

# How much workers to use

In my experience the best results are obtained with 3 times the number of the available cpu cores.

