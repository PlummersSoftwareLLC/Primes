#!/bin/sh

crystal build --release --no-debug primes.cr

for i in $(seq 10); do
    ./primes
done