#!/bin/sh
#CC="gcc -Ofast -march=native -mtune=native -funroll-all-loops -DCOMPILE_64_BIT"
CC="gcc -Ofast -march=native -mtune=native -funroll-all-loops"
PAR="-fopenmp"
for x in sieve_1of2 sieve_8of30 sieve_8of30_only_write_read_bits sieve_48of210 sieve_48of210_only_write_read_bits sieve_480of2310_only_write_read_bits sieve_5760of30030_only_write_read_bits; do
    $CC -o $x $x.c -lm
done
for x in sieve_1of2_par sieve_8of30_par sieve_48of210_par sieve_480of2310_par sieve_5760of30030_par sieve_1of2_epar sieve_8of30_epar sieve_48of210_epar sieve_480of2310_epar sieve_5760of30030_epar; do
    $CC $PAR -o $x $x.c -lm
done
