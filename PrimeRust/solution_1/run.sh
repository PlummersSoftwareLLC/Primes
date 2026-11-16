#!/bin/sh
set -e

# Count the physical cores available to this process on Linux. The sysfs
# thread_siblings_list groups logical CPUs that share a physical core, which
# also covers ARM and hybrid CPUs without relying on /proc/cpuinfo fields.
physical_cores() {
    allowed=$(sed -n 's/^Cpus_allowed_list:[[:space:]]*//p' /proc/self/status 2>/dev/null) || allowed=
    [ -n "$allowed" ] || {
        echo 0
        return
    }

    printf '%s\n' "$allowed" |
    awk -F, '{
        for (i = 1; i <= NF; i++) {
            n = split($i, range, "-")
            if (n == 1) print range[1]
            else for (cpu = range[1]; cpu <= range[2]; cpu++) print cpu
        }
    }' |
    while read -r cpu; do
        cat "/sys/devices/system/cpu/cpu${cpu}/topology/thread_siblings_list" 2>/dev/null
    done |
    awk 'NF && !seen[$0]++ { n++ } END { print n + 0 }'
}

PHYSICAL_CORES=$(physical_cores)
if [ "${PHYSICAL_CORES:-0}" -le 0 ] 2>/dev/null; then
    # No topology information: all logical CPUs is the safe fallback, and on
    # machines without SMT that is also the physical-core count.
    PHYSICAL_CORES=$(nproc 2>/dev/null || getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)
fi

# Existing solution_1 entry, unchanged.
./prime-sieve-rust

# New entry: single-threaded, then at the physical-core count.
./cokieminer-base-sieve
if [ "$PHYSICAL_CORES" -gt 1 ]; then
    ./cokieminer-base-sieve --threads "$PHYSICAL_CORES"
fi
