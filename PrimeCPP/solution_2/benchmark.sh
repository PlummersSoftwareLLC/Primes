#!/bin/bash

if [[ $1 == both || $1 == 1 || $1 == array ]]; then
    ./primes_array ${@:2}
fi

if [[ $1 == both || $1 == 2 || $1 == mask ]]; then
    ./primes_mask ${@:2}
fi
