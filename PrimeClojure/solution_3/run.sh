#!/bin/bash

RUNS=$(if [ -z "$1" ]; then echo 1; else echo $1; fi)
for ((i = 0; i < RUNS; i++))
do
  #echo $i
  java -jar target/primes.jar '{:variant :vector :warm-up? false}'
  java -jar target/primes.jar '{:variant :vector-transient :warm-up? false}'
  java -jar target/primes.jar '{:variant :bitset :warm-up? true}'
  #java -jar target/primes.jar '{:variant :bitset-shroedinger :warm-up? true}'
  #java -jar target/primes.jar '{:variant :bitset-all :warm-up? false}'
  #java -jar target/primes.jar '{:variant :bitset-pre :warm-up? false}'
  java -jar target/primes.jar '{:variant :boolean-array :warm-up? true}'
  #java -jar target/primes.jar '{:variant :boolean-array-all :warm-up? false}'
  #java -jar target/primes.jar '{:variant :boolean-array-pre :warm-up? false}'
  #java -jar target/primes.jar '{:variant :boolean-array-pre-futures :warm-up? false}'
  #java -jar target/primes.jar '{:variant :boolean-array-to-vector-futures :warm-up? false}'
done
