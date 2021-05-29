#!/bin/bash

cd $(dirname "${BASH_SOURCE[0]}")

for file in prime_*[^.bas]; do
    if [ -x ${file} ]; then
        ./${file}
    fi
done

