#!/bin/sh
# Serial versions
for x in sieve_1of2 sieve_8of30 sieve_8of30_only_write_read_bits sieve_48of210 sieve_48of210_only_write_read_bits sieve_480of2310_only_write_read_bits sieve_5760of30030_only_write_read_bits; do
    ./$x
done
# Thread parallel versions, computing each sieve quicker.
# 
# Chooses a number corresponding to all processing units in your system,
# but max 4 since it does not scale extremely well
NUM_PROCS=`nproc`
if [ "$NUM_PROCS" -gt "4" ]; then
    NUM_PROCS=4
fi
for x in sieve_1of2_par sieve_8of30_par sieve_48of210_par sieve_480of2310_par sieve_5760of30030_par; do
    OMP_NUM_THREADS=$NUM_PROCS ./$x
done
# Embarrassingly parallel versions, computing multiple sieves at the same time.
# Chooses a number corresponding to all processing units in your system
NUM_PROCS=`nproc`
for x in sieve_1of2_epar sieve_8of30_epar sieve_48of210_epar sieve_480of2310_epar sieve_5760of30030_epar; do
    OMP_NUM_THREADS=$NUM_PROCS ./$x
done
