#!/bin/bash

java PrimeSieveJava -warmup
numactl --cpunodebind=0 --membind=0 java PrimeSieveJava -variant bitset -warmup || java PrimeSieveJava -variant bitset -warmup