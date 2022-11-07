#!/bin/sh
CC="gcc -Ofast -march=native -mtune=native -funroll-all-loops -fno-asynchronous-unwind-tables"  
PAR="-fopenmp"
PAREXT="_epar"
for x in sieve_extend; do
    $CC -o $x $x.c -lm
    $CC $PAR -o $x$PAREXT $x.c -lm
done
