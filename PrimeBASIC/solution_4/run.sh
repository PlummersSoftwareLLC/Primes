#!/bin/bash

function run_sieve() {
    local program="$1"
    local author="$2"
    local suffix="$3"

    iterations=0
    runtime="5 second"
    endtime=$(($(date -d "$runtime" +%s%N)/1000000))
    starttime=$(($(date +%s%N)/1000000))
    currenttime=${starttime}

    while [[ $currenttime -le $endtime ]]
    do
        cbmbasic/cbmbasic "${program}"
        currenttime=$(($(date +%s%N)/1000000))
        ((iterations=iterations+1))
    done
    duration=$(echo "scale=3;($currenttime-$starttime)/1000" | bc)

    echo "${author};$iterations;$duration;1;algorithm=base,faithful=no${suffix}"
}

run_sieve primes.bas "davepl-rzuckerm-msbasic" ""
run_sieve primes2.bas "rzuckerm-msbasic-bit" ",bits=1"
