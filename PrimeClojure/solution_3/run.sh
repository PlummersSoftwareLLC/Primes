#!/bin/sh

clojure -X sieve/run :variant :boolean-array :warm-up? true
clojure -X sieve/run :variant :bitset :warm-up? false
clojure -X sieve/run :variant :boolean-array-futures :warm-up? true
