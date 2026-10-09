#!/bin/sh
set -e

gcc -O3 -march=x86-64-v3 -D_GNU_SOURCE main_st.c -x assembler sieve.asm -o tacitvs_st -lpthread
gcc -O3 -march=x86-64-v3 -D_GNU_SOURCE main_mt.c -x assembler sieve.asm -o tacitvs_mt -lpthread
