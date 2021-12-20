#!/bin/sh

clojure -X sieve/run :variant :bitset :warm-up? false
clojure -X sieve/run :variant :bitset-pre :warm-up? false
clojure -X sieve/run :variant :boolean-array :warm-up? false
clojure -X sieve/run :variant :boolean-array-pre :warm-up? true
clojure -X sieve/run :variant :boolean-array-pre-futures :warm-up? true
clojure -X sieve/run :variant :boolean-array-to-vector-futures :warm-up? true
