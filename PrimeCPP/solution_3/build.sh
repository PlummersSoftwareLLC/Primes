#!/bin/bash

CXX_ARGS="-pthread -O3 -m64 -mtune=native -std=c++20 -fconstexpr-steps=1000000000 -fconstexpr-depth=100000000"

clang++ $CXX_ARGS PrimeCPP_CONSTEXPR.cpp -o primes_constexpr