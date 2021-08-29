#!/bin/sh

nasm -felf64 primes_ff_unroll4.asm -o primes_ff_unroll4.o

gcc -no-pie primes_ff_unroll4.o -o primes_ff_unroll4.run
