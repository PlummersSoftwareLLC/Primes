#!/bin/sh

./sieves/sieve8 "$@" &&
./sieves/sieve32 "$@" &&
./sieves/sieve_ptr "$@" &&
./sieves/sieve_other "$@" &&
./sieves/sieve8_b "$@" &&
./sieves/sieve32_b "$@" &&
./sieves/sieve_ptr_b "$@" &&
./sieves/sieve_other_b "$@"
