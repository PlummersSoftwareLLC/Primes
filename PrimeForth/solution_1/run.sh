#!/bin/sh

if test -z "$GFORTH"
then
    export GFORTH=gforth-fast
fi

for src in prime-bitarray.fs prime-bytearray.fs
do
    "$GFORTH" $src -e "5 1000000 print-benchmark-results bye"
done
