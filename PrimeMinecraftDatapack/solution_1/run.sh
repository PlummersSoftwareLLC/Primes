#!/bin/bash

# This script assumes that OpenJDK 17 (or newer), grep and awk are already installed
# somewhere in $PATH, and the build.sh script has been executed.

[ -d env ] && cd env

if [ ! -x playio ]; then
    echo "ERROR: playio not present. Maybe run build.sh?"
    exit 1
fi

./playio java -Xmx1024M -Xms1024M -jar server.jar nogui < runioscript.txt > output.txt

if [ ! -f output.txt ]; then
    echo "ERROR: no output found"
    exit 1
fi

if ! grep -Fq 'Storage minecraft:primes has the following contents: 78498' output.txt ; then
    echo "ERROR: prime count missing or invalid"
    rm output.txt
    exit 1
fi

awk '/starttime/ {STARTTIME=$2} /endtime/ {ENDTIME=$2} END {printf("RCoder01;1;%.6f;1;algorithm=base,faithful=no\n", (ENDTIME-STARTTIME)/1000000.0)}' output.txt

rm output.txt
