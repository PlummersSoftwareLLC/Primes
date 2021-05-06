#!/bin/bash

nasm -felf64 primes_uff.asm -o primes_uff.o
gcc -no-pie primes_uff.o -o primes_uff.run
