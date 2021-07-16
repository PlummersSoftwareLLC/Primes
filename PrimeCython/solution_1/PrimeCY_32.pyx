"""
Cython Prime Sieve

Based on:
    Cython solution by tjol
    Cython solution by rpkak
    Original Python solution by davepl
"""

from libc.math cimport sqrt
from libc.time cimport clock, CLOCKS_PER_SEC
from libc.stdint cimport int32_t
from openmp cimport omp_get_wtime
from cython.parallel import prange

from array import array


def make_mask(x):
    return (2**32 - 1 - (1 << x)).to_bytes(4, "little")


TYPECODE = "l" if sizeof(long) == 4 else "i"
cdef int32_t[:] BITMASKS = array(TYPECODE, b"".join([make_mask(x) for x in range(32)]))


cdef class PrimeSieve:

    """This is the main PrimeSieve class. Call it with the number you wish as
    an upper limit, then call the run_sieve method to do the calculation.
    print_results will dump the count to check validity."""

    prime_counts = { 10 : 4,                 # Historical data for validating our results - the number of primes
                    100 : 25,                # to be found under some limit, such as 168 primes under 1000
                    1000 : 168,
                    10000 : 1229,
                    100000 : 9592,
                    1000000 : 78498,
                    10000000 : 664579,
                    100000000 : 5761455
                  }

    cdef size_t _size
    cdef size_t _num_bits
    cdef size_t _block_size
    cdef int32_t[:] _bits

    def __cinit__(self, limit, blocksize):
        self._size = limit
        self._num_bits = (self._size + 1) // 2
        self._bits = array(TYPECODE, b"\xff\xff\xff\xff") * ((self._num_bits + 31) // 32)
        self._block_size = blocksize

    def validate_results(self):

        """Look up our count of primes in the historical data (if we have it)
        to see if it matches"""

        if self._size in self.prime_counts:                              # the data, and (b) our count matches. Since it will return
            return self.prime_counts[self._size] == self.count_primes()  # false for an unknown upper_limit, can't assume false == bad
        return False

    cdef void _run_block(PrimeSieve self, size_t block_start, size_t block_end) nogil:

        """Process a specified part of the primes array.
        Processing the sieve block by block makes it more CPU cache-friendly.
        The speed gain is insignificant on 1_000_000, but huge on higher limits.
        Also, now we can parallelize calculation of the primes in one sieve"""

        cdef size_t index, step, start, i, start_chunk, next_start_chunk, end_chunk
        cdef int32_t bitmask

        cdef size_t factor = 1
        cdef size_t q = <size_t> sqrt(block_end) + 1

        while factor < q:

            while not (self._bits[factor // 32] & ~BITMASKS[factor % 32]):
                factor += 1
                continue

            start = 2 * factor * (factor + 1)
            step  = factor * 2 + 1

            if start < block_start:
                start += ((block_start - start) // step) * step
            if start < block_start:
                start += step

            # Splitting steps into 32 groups, so we'll need to make only 32 modulo operations per prime.
            # As a nice bonus, every step now is a multiple of 32, this gives us some space for other optimizations.
            end_chunk = (block_end + 31) // 32
            next_start_chunk = start // 32
            i = 0
            while i < 32:

                if next_start_chunk >= end_chunk:
                    break

                bitmask = 0xffffffff
                start_chunk = next_start_chunk

                # We don't want to process the same sequence of 32 bit chunks more than once.
                # This saves some time only for primes <= 31, but that's a lot.
                while next_start_chunk == start_chunk:
                    bitmask &= BITMASKS[start % 32]
                    i += 1
                    start += step
                    next_start_chunk = start // 32

                for index from start_chunk <= index < end_chunk by step:
                    self._bits[index] &= bitmask

            factor += 1

    cdef void run_sieve(PrimeSieve self, int threads):
        """Calculate the primes up to the specified limit"""

        cdef size_t q = <size_t>(sqrt(self._size) / 2)
        cdef ssize_t num_blocks = self._num_bits / self._block_size
        if self._num_bits % self._block_size:
            num_blocks += 1

        if threads > 1:
            self._run_block(0, q + 1)

        cdef int i
        for i in prange(0, num_blocks, num_threads=threads, nogil=True):
            self._run_block(self._block_size * i, min(self._num_bits, self._block_size * (i + 1)))

    def count_primes(self):

        """Return the count of bits that are still set in the sieve.
        Assumes you've already called run_sieve, of course!"""

        cdef size_t index
        cdef size_t total = 0
        if self._size >= 2:
            for index from 0 <= index < self._num_bits by 1:
                total += (self._bits[index // 32] >> (index % 32)) & 1
        return int(total)

    def get_primes(self):

        """Returns a generator to iterate over the found prime numbers.
        Requires a prior run_sieve call"""

        cdef size_t index
        if self._size > 1:
            yield 2  # Since we auto-filter evens, we have to special case the number 2 which is prime
        if self._size > 2:
            for index in range(1, self._num_bits):
                if self._bits[index // 32] & ~BITMASKS[index % 32]:
                    yield index * 2 + 1

    def print_results(self, show_results, duration, passes, threads):

        """Displays the primes found (or just the total count,
        depending on what you ask for)"""

        if show_results:
            for num in self.get_primes():
                print("%s, " % num, end="")
            print()

        print("Passes: %s, Time: %s, Avg: %s, Limit: %s, Count: %s, Valid: %s" % (passes, duration, duration/passes, self._size, self.count_primes(), self.validate_results()))

        # Following 2 lines added by rbergen to conform to drag race output format
        print();
        print("ssovest-cy;%s;%s;%s;algorithm=other,faithful=yes,bits=1" % (passes, duration, threads));


cdef double dclock():
    return <double>(clock()) / CLOCKS_PER_SEC


cdef PrimeSieve sieve
cdef double time_start, time_delta, timeout
cdef size_t limit, blocksize
cdef int passes, threads

if __name__ == "__main__":
    from argparse import ArgumentParser

    parser = ArgumentParser(description="Cython Prime Sieve")
    parser.add_argument("--limit", "-l", help="Upper limit for calculating prime numbers", type=int, default=1_000_000)
    parser.add_argument("--time", "-t", help="Time limit", type=float, default=5)
    parser.add_argument("--show", "-s", help="Print found prime numbers", action="store_true")
    parser.add_argument("--bsize", "-b", help="Block size, in bytes. Must be a multiple of 4", type=int, default=31_252)
    parser.add_argument("--threads", "-p", help="Number of threads", type=int, default=1)

    args = parser.parse_args()

    if args.bsize % 4:
        exit("Block size must be a multiple of 4")

    limit = args.limit if args.limit >= 0 else 1_000_000
    timeout = args.time if args.time > 0 else 5
    show_results = args.show
    blocksize = (args.bsize * 8) if args.bsize > 0 else 250_016
    threads = max(1, args.threads)

    if threads == 1:
        timer = dclock
    else:
        timer = omp_get_wtime

    time_start = timer()                                   # Record our starting time
    passes = 0                                             # We're going to count how many passes we make in fixed window of time

    while (timer() - time_start < timeout):                # Run until more than N seconds have elapsed
        sieve = PrimeSieve(limit, blocksize)               # Calc the primes up to the limit
        sieve.run_sieve(threads)                           # Find the results
        passes = passes + 1                                # Count this pass

    time_delta = (timer() - time_start)                    # After the "at least N seconds", get the actual elapsed

    sieve.print_results(show_results, time_delta, passes, threads)  # Display outcome
