#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

IS_DARWIN=false
[[ "$(uname -s)" == "Darwin" ]] && IS_DARWIN=true

# Common vectorization flags for Darwin
DARWIN_EXTRA=()
$IS_DARWIN && DARWIN_EXTRA=(-fvectorize -fslp-vectorize)

src="PrimeC_extend.c"
bin="primes_c_extend"
cc="${CC:-clang}"
flags=(-march=native -mtune=native -pthread -O3 -ffast-math
       -fno-math-errno -fno-trapping-math -flto -std=c11
       "${DARWIN_EXTRA[@]}")

echo "Compiling $src -> $bin"
"$cc" "${flags[@]}" "$src" -o "$bin" -lm -lpthread

echo "Running $bin"
set +e; "./$bin"; s=$?; set -e
[[ $s -ne 0 ]] && echo "Note: exited with status $s (prime count mod 256)."
