#!/bin/bash

cd $(dirname "${BASH_SOURCE[0]}")

for file in prime_*.run; do
    if [ -x ${file} ]; then
        ./${file}
    fi
done

