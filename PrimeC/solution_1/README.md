# C solution by mckoss

Single threaded implementation in C of prime number sieve.

Several algorithm optimizations are applie to achieve a fast running time.

- Not just ignoring even bits in the sieve bitmap, but also skipping higher
  multiples of 3 and 5 as well.
- Precomputing bit-masks for re-use in marking passes.

## Run instructions

Compile with GCC optimizing for fast execution and then running the
compiled program by executing this script:

>> $ run.sh

## Output

