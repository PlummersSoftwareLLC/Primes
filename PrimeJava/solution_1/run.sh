#!/bin/bash

RUNS=$(if [ -z "$1" ]; then echo 1; else echo "$1"; fi)
for ((i = 0; i < RUNS; i++))
do
  #echo $i
  java -XX:+UseNUMA PrimeSieveJava -warmup
  java -XX:+UseNUMA PrimeSieveJava -variant bitset -warmup
done