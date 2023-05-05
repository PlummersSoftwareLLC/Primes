#!/bin/sh
for x in sieve_extend sieve_extend_epar; do
    for y in u32_v8 u64_v4 u64_v8; do
        ./$x-$y
    done
done
