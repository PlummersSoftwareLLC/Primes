#!/bin/bash

ASM_ARGS="-pthread -O3 -m64 -mtune=native"

clang++ $ASM_ARGS PrimeAssembly.s -o primes