#!/bin/bash

nasm -felf64 primes.asm -o primes.o
gcc -no-pie primes.o -o primes
