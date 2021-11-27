#!/bin/bash

c1541 -format primes,c1 d64 ./primes.d64 -write ./primes.prg > /dev/null
xpet -console -autostart ./primes.d64 +sound > /dev/null &

while : ; do
    sleep 10 
    c1541 -attach ./primes.d64 -list | grep -q output && \
        c1541 -attach ./primes.d64 -read output,s output.txt > /dev/null && \
        grep -q "TIME 0X[0-9A-F]\{6\}" output.txt && \
        break
done

kill %1

./parse.sh
