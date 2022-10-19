#!/bin/sh
CC="gcc -Ofast -march=native -mtune=native -funroll-all-loops" 
for x in sieve_memcopy primes_words; do
    $CC -o $x $x.c -lm
done
