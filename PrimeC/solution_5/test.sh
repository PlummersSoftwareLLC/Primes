#!/bin/sh
CC="gcc -Ofast -march=native -mtune=native -funroll-all-loops" 
for x in $1; do
    $CC -o $x $x.c -lm
done
./$1