# Sieve Algorithm Comparisons (C)

Several prime sieve algorithms are tested in this program.  Each
is run with the same 5 second passes limit and averages
the ms per pass.

# Notes on Performance

It can be difficult to predict which algorithms will
work faster than others.  These are all tested over
the range of primes up to one million.

My computer is an Intel i8-8700K @ 3.7GHz with 32 Gb Ram.
But none of these algorithms allocate more than about 1 MB
at a time.

For algorithms that use a bit-mask to store the sieve,
I found a word size of 32 bits was fastest (even though
I'm compiling on a 64-bit machine and 64 bit executable).

This is the output on my machine as of April 2021:

```
Calculate primes up to 1000000.
Timer resolution: 1000 ticks per second.
Word size: 32 bits.

      Byte-map - 1 of 2 tested:  6146 passes completed in 5 seconds (0.814 ms per pass).
       Bit-map - 1 of 2 tested:  8400 passes completed in 5 seconds (0.595 ms per pass).
       Bit-map - 2 of 6 tested: 10572 passes completed in 5 seconds (0.473 ms per pass).
      Bit-map - 8 of 30 tested: 12349 passes completed in 5 seconds (0.405 ms per pass).
                   1/2 Bit-map:  7656 passes completed in 5 seconds (0.653 ms per pass).
                   1/3 Bit-map:  6746 passes completed in 5 seconds (0.741 ms per pass).
```

## The Algorithms

1. **Byte-map 1 of 2 tested**: The most straightforward implementation storing each "bit"
   in a byte (so allocating one million bytes for the sieve buffer).  The
   code never bothers to test even numbers for primality - nor does it
   bother to mark them in the sieve as "not prime".  We just ignore them.
2. **Bit-map 1 of 2 tested**: This is also a straightforward algorithm, but is 8 times
   more efficient of memory since we store sieve bits in single bits in memory.
   We allocate one million bits.  Similarly ignore 1/2 the bits for the even
   numbers.
3. **Bit-map 2 of 6 tested**: This algorithm further ignores all multiples of 3 as well
   as multiples of 2.  It turns out only 2 out of every 6 numbers can be a prime
   candidate (those that are congruent to 1 or 5 mod 6).  So we ignore the rest.
4. **Bit-map 8 of 30 tested**: Taking an even more extreme approach, we also ignore
   the multiples of 5.  This starts to get more complex to only enumerate the 8 possible
   numbers out of every 30 that are prime candidates.  But it turns out this algorithm
   is ***the fastest***.  It's more that twice as fast as naive byte-map.
5. **1/2 Bit-map**: I presumed that not actually allocating space for the even
   numbers in the bit-map could be a win (allowing more bits to fit in the CPU caches).
   But it turns out the added complexity of the indexing and masking makes this a little
   slower than just ignore 1/2 the bits of the sieve buffer.
6. **1/3 Bit-map**: Same here - code getting more complicated to only store 1/3 as
   many bits as the naive version.  But it's slower still.
