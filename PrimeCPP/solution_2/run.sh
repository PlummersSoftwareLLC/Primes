#!/bin/bash

# g++ -Ofast  -std=c++17 -lc++ PrimeCPP.cpp -oPrimes.exe
# gcc -Ofast -std=c++17 PrimeCPP.cpp -lc++ -oPrimes_gcc.exe
# clang -Ofast -std=c++17 -lc++ PrimeCPP.cpp -oPrimes_clang.exe

if [[ "$#" -eq "0" -o "$1" -eq "1" -o "$1" -eq "array" ]]; then
    echo "Building and running the array approach"
    clang++ -march=native -mtune=native -pthread -Ofast -std=c++17 PrimeCPP_array.cpp -oprimes_array.exe
    ./primes_array.exe
fi

if [[ "$#" -eq "0" -o "$1" -eq "2" -o "$1" -eq "mask" ]]; then
    echo "Building and running the mask approach"
    clang++ -march=native -mtune=native -pthread -Ofast -std=c++17 PrimeCPP_mask.cpp -oprimes_mask.exe
    ./primes_mask.exe
fi
