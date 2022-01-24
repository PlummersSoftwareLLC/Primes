#!/bin/sh

clojure -X sieve/run :variant :vector :warm-up? false
clojure -X sieve/run :variant :vector-transient :warm-up? false
clojure -X sieve/run :variant :bitset :warm-up? true
#clojure -X sieve/run :variant :bitset-all :warm-up? false
#clojure -X sieve/run :variant :bitset-pre :warm-up? false
clojure -X sieve/run :variant :boolean-array :warm-up? true
#clojure -X sieve/run :variant :boolean-array-all :warm-up? false
#clojure -X sieve/run :variant :boolean-array-pre :warm-up? false
#clojure -X sieve/run :variant :boolean-array-pre-futures :warm-up? false
#clojure -X sieve/run :variant :boolean-array-to-vector-futures :warm-up? false
