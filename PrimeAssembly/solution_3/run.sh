#!/bin/bash

# This script assumes that OpenJDK, grep and awk are already installed
# somewhere in $PATH, and the build.sh script has been executed.

if [ ! -x playio ]; then
    echo "ERROR: playio not present. Maybe run build.sh?"
    exit 1
fi

cd emuStudio
../playio ./emuStudio -cf config/MITSAltair8800.toml --input-file ../sievedp.asm auto --no-gui < ../runioscript.txt > ../output.txt

cd ..

if [ ! -f output.txt ]; then
    echo "ERROR: no output found"
    exit 1
fi

if ! grep -Fq 'Primes found: 1229' emuStudio/adm3A-terminal.out ; then
    echo "ERROR: prime count missing or invalid"
    rm output.txt
    exit 1
fi

awk '/starttime/ {STARTTIME=$2} /endtime/ {ENDTIME=$2} END {printf("mikedouglas-davepl;10;%.6f;1;algorithm=base,faithful=no,bits=8\n", (ENDTIME-STARTTIME)/1000000.0)}' output.txt

rm output.txt
