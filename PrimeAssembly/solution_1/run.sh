#!/bin/sh

# rbergen original variants

./primes_uff_byte.run
./primes_ff_byte.run
./primes_uff_bitbtr.run
./primes_ff_bitbtr.run
./primes_uff_bitshift.run
./primes_ff_bitshift.run

# joonicks evolved variants

./primes_ff_unroll4.run
./primes_ff_hardwired.run
