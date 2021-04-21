#!/bin/bash
# Serial versions
for x in sieve_1of2 sieve_8of30 sieve_8of30_only_write_read_bits sieve_48of210 sieve_48of210_only_write_read_bits sieve_480of2310_only_write_read_bits sieve_5760of30030_only_write_read_bits; do
    ./$x
done
# Thread parallel versions, computing each sieve quicker. Comment this line to run on all threads, likely larger than 4 will not help much.
export OMP_NUM_THREADS=4
for x in sieve_1of2_par sieve_8of30_par sieve_48of210_par sieve_480of2310_par sieve_5760of30030_par; do
    ./$x
done
# Embarrassingly parallel versions, computing multiple sieves at the same time. Choose a number corresponding to all cores in your system
export OMP_NUM_THREADS=6
for x in sieve_1of2_epar sieve_8of30_epar sieve_48of210_epar sieve_480of2310_epar sieve_5760of30030_epar; do
    ./$x
done
