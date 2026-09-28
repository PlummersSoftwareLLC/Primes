#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

CXX="${CXX:-g++}"
$CXX -O3 -march=native -mtune=native -pthread -std=c++17 PrimeCPP.cpp -o primes
./primes
