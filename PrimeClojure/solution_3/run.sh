#!/bin/bash

RUNS=$(if [ -z "$1" ]; then echo 1; else echo $1; fi)
for ((i=0;i<${RUNS};i++))
do
  echo $i
  #clojure -X sieve/run :variant :vector :warm-up? false
  #clojure -X sieve/run :variant :vector-transient :warm-up? false
  clojure -X sieve/run :variant :bitset :warm-up? true
  #clojure -X sieve/run :variant :bitset-all :warm-up? false
  #clojure -X sieve/run :variant :bitset-pre :warm-up? false
  clojure -X sieve/run :variant :boolean-array :warm-up? true
  sleep 20
  #clojure -X sieve/run :variant :boolean-array-all :warm-up? false
  #clojure -X sieve/run :variant :boolean-array-pre :warm-up? false
  #clojure -X sieve/run :variant :boolean-array-pre-futures :warm-up? false
  #clojure -X sieve/run :variant :boolean-array-to-vector-futures :warm-up? false
done
