#!/bin/sh

cython -3 --embed PrimeCY.pyx -o PrimeCY.c
gcc -I/usr/local/include/python3.9 -lpython3.9 -lm PrimeCY.c -o PrimeCY

cython -3 --embed Prime32.pyx -o Prime32.c -X cdivision=True -X boundscheck=False
gcc Prime32.c -Ofast -o Prime32 `python3-config --embed --includes --ldflags` -fopenmp
