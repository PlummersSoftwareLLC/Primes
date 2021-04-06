#!/bin/bash

# Scheme program can be with minimal configuration run with two implementations: Guile and Chez Scheme
# Chez Scheme is faster


# uncomment for Guile Scheme
# guile --use-srfi=19,28 prime.scm

# uncomment for Chez Scheme
# echo "(define time-monotonic 'time-monotonic)"     | cat - prime.scm > prime-sieve.scm
# chezscheme --optimize-level 3 --script prime-sieve.scm
