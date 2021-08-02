#!/bin/sh

CFLAGS=`python3-config --embed --includes`
LDFLAGS=`python3-config --embed --ldflags`
CC=gcc

cython -3 --embed PrimeCY_bytearray.pyx -o PrimeCY_bytearray.c
cython -3 --embed PrimeCY_bitarray.pyx -o PrimeCY_bitarray.c
cython -3 --embed PrimeCY_32.pyx -o PrimeCY_32.c -X cdivision=True -X boundscheck=False

$CC -O3 $CFLAGS $LDFLAGS PrimeCY_bytearray.c -o PrimeCY_bytearray
$CC -O3 $CFLAGS $LDFLAGS PrimeCY_bitarray.c -o PrimeCY_bitarray
$CC -O3 -march=native -fopenmp $CFLAGS $LDFLAGS PrimeCY_32.c -o PrimeCY_32