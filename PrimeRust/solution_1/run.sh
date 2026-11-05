#!/bin/sh
set -e

# 1. Mike Barber / GordonBGood entries
./prime-sieve-rust

# 2. CokieMiner Base Sieve
# 2.1 Single-threaded (1 thread)
./cokieminer-base-sieve

# 2.2 Physical cores (without SMT contention, maximizing throughput per core)
PHYSICAL_CORES=$(awk '
  /^physical id/ { p = $4 }
  /^core id/     { c = $4; if (!seen[p, c]++) count++ }
  END { print (count ? count : 0) }
' /proc/cpuinfo 2>/dev/null || echo 0)

LOGICAL_CORES=$(nproc)
if [ "$PHYSICAL_CORES" -le 0 ] || [ "$PHYSICAL_CORES" -gt "$LOGICAL_CORES" ]; then
    if [ "$LOGICAL_CORES" -gt 1 ]; then
        PHYSICAL_CORES=$((LOGICAL_CORES / 2))
    else
        PHYSICAL_CORES=1
    fi
fi

if [ "$PHYSICAL_CORES" -gt 1 ] && [ "$PHYSICAL_CORES" -lt "$LOGICAL_CORES" ]; then
    ./cokieminer-base-sieve --threads "$PHYSICAL_CORES"
fi

# 3. All logical threads (Hyper-Threading / SMT)
./cokieminer-base-sieve --threads auto
