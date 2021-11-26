#!/bin/bash

c1541 -format primes,c1 d64 ./primes.d64 -write ./primes.prg
x128 -logfile /opt/vice.log -verbose -autostart -console ./primes.d64

ls /opt

exit 0

while : ; do
    sleep 10 
    c1541 -attach ./primes.d64 -list | grep -q output && \
        c1541 -attach ./primes.d64 -read output,s output.txt && \
        grep -q "TIME 0X[0-9A-F]{6}" output.txt && \
        break
done

kill %1

./parse.sh
