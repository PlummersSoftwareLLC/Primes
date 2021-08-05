#!/bin/sh
CC="gcc -Ofast -march=native -mtune=native -funroll-all-loops" 
for x in primes_words primes_stripped-block; do
    $CC -o $x $x.c -lm
done
