#!/bin/bash

crystal build primes.cr --release --no-debug --progress
./primes
