#!/bin/bash
# Compilation and execution on Git-Bash (GNU C Compiler)
OFLAGS="-Wall -Wextra -pedantic -Ofast -march=native -mtune=native -funsafe-math-optimizations"
gcc $OFLAGS sieve_memcopy.c -o sieve_memcopy -lm
./sieve_memcopy
