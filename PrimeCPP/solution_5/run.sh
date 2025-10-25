#!/bin/bash
set -euo pipefail

# Ensure we operate relative to this script's directory
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

# Build PrimeCPP_array.cpp and run the binary with no arguments.
SRC="PrimeCPP_array.cpp"
BIN="primes_array"

CXX="${CXX:-clang++}"
CXXFLAGS=(-march=native -mtune=native -pthread -O3 -ffast-math -fno-math-errno -fno-trapping-math -flto -std=c++17)

if [[ "$(uname -s)" == "Darwin" ]]; then
    CXXFLAGS+=(-fvectorize -fslp-vectorize)
fi

echo "Compiling $SRC -> $BIN"
"$CXX" "${CXXFLAGS[@]}" "$SRC" -o "$BIN"

echo "Running $BIN"
set +e
"./$BIN"
status=$?
set -e
if [[ $status -ne 0 ]]; then
    echo "Note: binary exited with status $status (prime count modulo 256)."
fi
