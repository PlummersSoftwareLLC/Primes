#!/bin/sh

clojure -X sieve-8-bit/run :warm-up? true
clojure -X sieve-1-bit/run
