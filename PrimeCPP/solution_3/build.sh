#!/bin/bash

CXX_ARGS="-pthread -O3 -m64 -mtune=native -std=c++20 -fconstexpr-steps=1000000000 -fconstexpr-depth=100000000"

clang++ $CXX_ARGS -c PrimeCPP_CONSTEXPR.cpp -o primes_constexpr.o &&
clang++ $CXX_ARGS -c shadowfunc.cpp -o shadowfunc.o &&
clang++ $CXX_ARGS primes_constexpr.o shadowfunc.o -o primes_constexpr