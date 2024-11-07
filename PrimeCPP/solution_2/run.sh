#!/bin/bash

# g++ -Ofast  -std=c++17 -lc++ PrimeCPP.cpp -oPrimes.exe
# gcc -Ofast -std=c++17 PrimeCPP.cpp -lc++ -oPrimes_gcc.exe
# clang -Ofast -std=c++17 -lc++ PrimeCPP.cpp -oPrimes_clang.exe

if [[ $# == 0 || $1 == 1 || $1 == array ]]; then
    echo -e "Building and running the array approach...\n"
    clang++ -march=native -mtune=native -pthread -Ofast -std=c++17 PrimeCPP_array.cpp -oprimes_array.exe
    ./primes_array.exe
    echo
fi

if [[ $# == 0 || $1 == 2 || $1 == mask ]]; then
    echo -e "Building and running the mask approach...\n"
    clang++ -march=native -mtune=native -pthread -Ofast -std=c++17 PrimeCPP_mask.cpp -oprimes_mask.exe
    ./primes_mask.exe
    echo
fi
