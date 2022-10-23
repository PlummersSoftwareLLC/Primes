#!/bin/sh
CC="gcc -Ofast -march=native -mtune=native -funroll-all-loops" 
$CC -o $1 $1.c -lm
./$1 $2 $3 $4 $5 $6 $7