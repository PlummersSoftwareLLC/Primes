#!/bin/bash

ITERATIONS=20
OUT=$(mktemp)
for i in $(seq -w $ITERATIONS); do
    echo -n "Iteration ${i}: "
    perl primes.pl | tee -a $OUT
done

BEST_RESULT=$(awk '{print $2}' $OUT | sort | tail -n1)
echo "-----"
echo "Best result is: ${BEST_RESULT}"