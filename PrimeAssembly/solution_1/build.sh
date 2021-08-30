#!/bin/sh

# rbergen original variants
nasm -felf64 primes_uff_byte.asm -o primes_uff_byte.o
nasm -felf64 primes_ff_byte.asm -o primes_ff_byte.o
nasm -felf64 primes_uff_bitbtr.asm -o primes_uff_bitbtr.o
nasm -felf64 primes_ff_bitbtr.asm -o primes_ff_bitbtr.o
nasm -felf64 primes_uff_bitshift.asm -o primes_uff_bitshift.o
nasm -felf64 primes_ff_bitshift.asm -o primes_ff_bitshift.o

gcc -no-pie primes_uff_byte.o -o primes_uff_byte.run
gcc -no-pie primes_ff_byte.o -o primes_ff_byte.run
gcc -no-pie primes_uff_bitbtr.o -o primes_uff_bitbtr.run
gcc -no-pie primes_ff_bitbtr.o -o primes_ff_bitbtr.run
gcc -no-pie primes_uff_bitshift.o -o primes_uff_bitshift.run
gcc -no-pie primes_ff_bitshift.o -o primes_ff_bitshift.run

# joonicks evolved variants

nasm -felf64 primes_ff_unroll4.asm -o primes_ff_unroll4.o
nasm -felf64 primes_ff_hardwired.asm -o primes_ff_hardwired.o

gcc -no-pie primes_ff_unroll4.o -o primes_ff_unroll4.run
gcc -no-pie primes_ff_hardwired.o -o primes_ff_hardwired.run
