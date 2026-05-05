#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd "$SCRIPT_DIR"

exec gfortran -O3 -march=native -fopenmp -std=f2008 cwager_fortran_mt.f90 -o cwager_fortran_mt
