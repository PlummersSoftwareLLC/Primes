#!/bin/sh

CFLAGS=`python3-config --embed --includes`
LDFLAGS=`python3-config --embed --ldflags`
CC=gcc

cython -3 --embed PrimeCY.pyx -o PrimeCY.c
cython -3 --embed PrimeCY_pointers.pyx -o PrimeCY_pointers.c
cython -3 --embed PrimeCY_bitarray.pyx -o PrimeCY_bitarray.c

# PrimeCY.pyx does not benefit from GCC optimisations
$CC $CFLAGS $LDFLAGS PrimeCY.c -o PrimeCY

# The others do, quite a lot!
$CC -O3 $CFLAGS $LDFLAGS PrimeCY_pointers.c -o PrimeCY_pointers
$CC -O3 $CFLAGS $LDFLAGS PrimeCY_bitarray.c -o PrimeCY_bitarray
