#!/bin/bash

start=$(date +%s%N)
end=$((start + 5000000000))
count=0

while [ $(date +%s%N) -lt $end ]; do
    ./sieve > /dev/null
    count=$((count + 1))
done

# Output the final result with actual timing
elapsed_ns=$(($(date +%s%N) - start))
elapsed_sec=$((elapsed_ns / 1000000000))
elapsed_frac=$(((elapsed_ns % 1000000000) / 1000))

printf "legen-holyc;%u;%u.%06u;1;algorithm=base,faithful=yes,bits=8\n" "$count" "$elapsed_sec" "$elapsed_frac"
