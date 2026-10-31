#!/bin/sh
set -e

./rust-wheel8-sieve
./cokieminer-sieve
./cokieminer-sieve --threads auto

