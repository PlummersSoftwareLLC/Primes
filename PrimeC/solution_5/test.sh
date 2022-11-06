#!/bin/sh
rm --force *.s
rm --force *.o
CC="-Ofast -march=native -funroll-all-loops -mtune=native -fno-asynchronous-unwind-tables -malign-data=cacheline -fno-exceptions -masm=intel -fverbose-asm  -mavx -W -Wall -Wno-unused-function"
#CC="-Ofast -march=native -mtune=native -funroll-all-loops -malign-data=cacheline" 
PAR="-fopenmp"
PAREXT="_epar"
gcc -c -g -Wa,-asdlh  $CC $1.c > $1.s
gcc -c -g -Wa,-asdlh  $CC $PAR $1.c > $1$PAREXT.s
gcc $CC -o $1 $1.c -lm
gcc $CC $PAR -o $1$PAREXT $1.c -lm
./$1 $2 $3 $4 $5 $6 $7
