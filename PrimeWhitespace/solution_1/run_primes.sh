#!/bin/bash
./run_primes.py --bits-per-word=32 "$@"
./run_primes.py --bits-per-word=64 "$@"
