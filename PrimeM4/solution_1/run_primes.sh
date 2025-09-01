#!/bin/bash
for script in primes.m4 primes-bit.m4
do
    m4 "$@" $script
done
