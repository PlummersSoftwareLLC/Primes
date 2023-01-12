#!/bin/sh
CC="gcc -Ofast -march=native -mtune=native -funroll-all-loops -fno-asynchronous-unwind-tables -fno-exceptions -fomit-frame-pointer -Wno-psabi"  
PAR="-fopenmp"
PAREXT="_epar"
for x in sieve_extend; do
    for y in u32_v8 u64_v4 u64_v8; do
        $CC -o $x-$y $x.c -D$y -s
        $CC $PAR -o $x$PAREXT-$y $x.c -D$y -s
    done
done
