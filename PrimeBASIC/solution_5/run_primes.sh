#!/bin/bash
SIEVE_SIZE="${1:-1000000}"
TIME_LIMIT="${2:-5}"
SHOW_RESULTS="${3:-0}"
printf "%d\n%d\n%d\n" "${SIEVE_SIZE}" "${TIME_LIMIT}" "${SHOW_RESULTS}" | python2.7 /opt/pitybas/pb.py primes.8xp.txt
