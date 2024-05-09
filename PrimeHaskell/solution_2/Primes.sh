#!/bin/bash

if lscpu | grep -qi "aarch64"; then
  ghc Primes -threaded
else
  if lscpu | grep -qi "avx2"; then
    ghc Primes -DAVX2 -threaded
  else
    ghc Primes -DAVX -threaded
  fi
fi
