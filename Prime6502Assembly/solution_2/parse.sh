#/bin/bash
c1541 ./primes.d64 -read output,s ./output.txt > /dev/null

if [ ! -f output.txt ]; then
    echo "ERROR: no output found"
    exit 1
fi

if ! grep -Fq 'VALID Y' output.txt ; then
    echo "ERROR: prime count incorrect"
    rm output.txt
    exit 1
fi

mac2unix output.txt > /dev/null 2>&1

awk '/TIME/ {RUNTIME=$2} END {printf("rbergen-pet;1;%.3f;1;algorithm=base,faithful=no,bits=1\n", RUNTIME/60.0)}' output.txt

rm output.txt
