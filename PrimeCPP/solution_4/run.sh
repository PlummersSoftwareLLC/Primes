#!/bin/bash

clang++ -Wall -Wextra -pedantic -std=c++17 -pthread -O3 -march=native -mtune=native PrimeCPP.cpp -oPrimeCPP
./PrimeCPP
rm -rf PrimeCPP
