#!/bin/sh
export PATH=/bin/node/bin:$PATH
java -jar PrimeSieve-1.0-all.jar -t -if -i -t:m -if:m -i:m
node PrimeSieve.js -t -if -i
./PrimeSieve.kexe -t -if -i