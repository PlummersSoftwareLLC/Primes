#!/bin/bash

iterations=0
runtime="5 second"
endtime=$(($(date -d "$runtime" +%s%N)/1000000))
starttime=$(($(date +%s%N)/1000000))
currenttime=${starttime}

while [[ $currenttime -le $endtime ]]
do
    cbmbasic primes.bas
    currenttime=$(($(date +%s%N)/1000000))
    ((iterations=iterations+1))
done
duration=$(echo "scale=3;($currenttime-$starttime)/1000" | bc)

echo "davepl-msbasic;$iterations;$duration;1;algorithm=base,faithful=no"