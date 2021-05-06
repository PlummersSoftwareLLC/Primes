#!/bin/sh

nasm -felf64 primes_uff.asm -o primes_uff.o
nasm -felf64 primes_ff.asm -o primes_ff.o

gcc -no-pie primes_uff.o -o primes_uff.run
gcc -no-pie primes_ff.o -o primes_ff.run
