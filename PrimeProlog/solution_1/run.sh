#!/bin/bash

cd $(dirname "${BASH_SOURCE[0]}")

for file in `find . -name "primes-*.pl"`; do
    swipl -O ./$file
done

