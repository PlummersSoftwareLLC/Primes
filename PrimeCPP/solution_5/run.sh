#!/bin/bash
# gcc -Ofast -std=c++17 SeiveBitOrs.cpp -lc++ -oSeiveBitOrs_gcc.exe
# g++ -Ofast  -std=c++17 SeiveBitOrs.cpp -oSeiveBitOrs_g++.exe
# clang -Ofast -std=c++17 -lc++ SeiveBitOrs.cpp -oSeiveBitOrs_clang.exe

clang++ -march=native -mtune=native -Ofast -std=c++17 SeiveBitOrs.cpp -o SeiveBitOrs_clang++.exe
cp SeiveBitOrs_clang++.exe SeiveBitOrs.exe

./SeiveBitOrs.exe
