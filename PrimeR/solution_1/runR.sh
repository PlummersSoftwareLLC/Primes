#!/bin/bash
 
# Workaround because R always prefixes [n] for each line
Rscript primes.R | sed  "s/\[[0-9].*\] //"
