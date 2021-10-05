#!/bin/bash

if lscpu | grep -qi "aarch64"; then
  ghc Primes
else
  if lscpu | grep -qi "avx2"; then
    ghc Primes -DAVX2
  else
    ghc Primes -DAVX
  fi
fi
