#!/bin/bash
set -e

dub test --compiler=ldc2
dub build --compiler=ldc2 --build=release

./prime-sieve-d
