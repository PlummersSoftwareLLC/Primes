#!/bin/sh

nasm -felf64 primes_uff_byte.asm -o primes_uff_byte.o
nasm -felf64 primes_ff_byte.asm -o primes_ff_byte.o
nasm -felf64 primes_uff_bit.asm -o primes_uff_bit.o
nasm -felf64 primes_ff_bit.asm -o primes_ff_bit.o

gcc -no-pie primes_uff_byte.o -o primes_uff_byte.run
gcc -no-pie primes_ff_byte.o -o primes_ff_byte.run
gcc -no-pie primes_uff_bit.o -o primes_uff_bit.run
gcc -no-pie primes_ff_bit.o -o primes_ff_bit.run
