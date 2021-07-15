#!/bin/sh

cython -3 --embed Prime32.pyx -o Prime32.c -X cdivision=True -X boundscheck=False
gcc Prime32.c -Ofast -o sieve `python3-config --embed --includes --ldflags` -fopenmp