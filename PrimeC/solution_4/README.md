# C language solution by Rogier van Dam

![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This implementation uses the base algorithm with a slight extension.
It marks all the multiples of a prime in limited range - 2 times the product of all the primes before it (except for 2, which is not stored anyway), multiplied by the size of a word (32 bit, 4 bytes). For example, with prime 7, the range is 2 * (3 * 5 * 7) * 32. This range consists of two blocks of 3 * 5 * 7 * 32. The first block misses some multiples of 3, 5 and 7, because they where marked in the sieve as prime. The second block has all the multiples of 3, 5 and 7, starting at a 32 bit alignment and with a size of a multiple of 32. This block can be repeated multiple times.  

When the multiples of 7 in this second block are marked, copies of this second block are made to a large ranges (previous range * next prime (e.g. 11). When copying, the ranges for 3, 5 and 7 are enlarged simultaneously. This copy can be made efficiently by getting the original one time from memory and storing it multiple times. The range is widening out increasingly. By doing it this way, hopefully everything can be done in the CPU level 1 cache, therefore greatly optimizing performance.

So this algorithm is generic and doesn't store any primes up front. It is faithful to the original idea of the sieve, only optimizing CPU and memory performance. However, the rule for base is "*the algorithm clears all non-primes individually, increasing the number with 2 * factor on each cycle.*". We do clear the non-primes individually, but only up to a certain point. Thereafter, they are cleared simultaneously. So it is a slight alteration of the base model.

I added this because it is the fastest C algorithm on my machine.

## Run instructions
You can use the Dockerfile.

First compile the code using the **run.sh** script. Optimization parameters are at the beginning of the script. They are set up to use the gcc compiler.

Then run the code using the run.sh script, with the command:
./run.sh

## Output
On i7-8700 (WSL2, no docker):
```
rogiervandam;20513;5.000194;1;algorithm=other,faithful=yes,bits=1
```
