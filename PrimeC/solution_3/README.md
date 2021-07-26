# Implementation in C

![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is an implementation in C. The basic calculation is based on [solution_2/sieve_1of2.c](../solution_2/sieve_1of2.c) by Daniel Spångberg. However, this implementation has the segmented optimization built on top of that.

## The segmented algorithm

This algorithm is based on the fact that the cross-out pattern repeats at the product of the found primes. So for example:

If we have crossed out `3` then the words in the sieve will have the pattern below:

`WORD0,WORD1,WORD2,WORD3,WORD1,WORD2,WORD3,WORD1, etc`

After `5` is crossed out we get a similar repeating pattern, but this time after `3 * 5 = 15` words. Note that we can exclude the prime `2` because we are considering already only odd numbers in the sieve.

This algorithm makes use of this repeating pattern. If we know the repeating pattern then we can copy that pattern in one step until the end of the sieve size, instead of multiple bit operations on the same words to achieve the same result.

### Considerations

- It is only efficient if it affects every word. So only primes are considered that are  smaller than half the word size. This means that in the cross-out phase each step would always affect every word because the step size is smaller than one word.
- The copy of the pattern has to occur often enough to get benefit from the repeating pattern calculation. Therefor this code calculates a repeating pattern that is less than 1% of the total sieve size, so it can be copied 99 times. This results in a reduction in bit calculations for low primes with 99%.
- To make the calculation fair, no knowledge of what the prime numbers are is included in the algorithm. In other words, the repeating pattern and what primes to use for that pattern is calculated in each pass and depends on the word size and sieve size.

### Calculation steps

The algorithm consists of the following steps:

1. First calculate all the primes in the first half word.
2. Find the largest product of primes that is smaller than 1% of the sieve_size. Keep the largest prime in this product.
3. Run the sieve from `3` to and including the word that is equal to the product found in step 2. Stop after the largest prime of step 2 is processed.
4. Fill the rest of the array with copies of the words from word `1` to the product found in step 2.
5. Cross-out the remaining primes as usual, start with the largest prime from step 2 plus 2.

## Choice of Dockerfile

This solution uses Ubuntu 18.04 as the base image over alpine:3.13 because the `memcpy` function on alpine is slow due to the `must clib`. The speed in this solution comes from the use of  `memcpy`, so it is extra slow on alpine.

## Run instructions

### Build and run native

To run this solution you need the gcc compiler and dependencies.

```bash
cd path/to/sieve
./compile.sh
./run.sh
```

### Run with Docker

To run with Docker take the following steps:

1. Install Docker: <https://docs.docker.com/get-docker/>
2. Build the image:

    ```bash
    docker build --pull --rm -f "Dockerfile" -t c:latest "."
    ```

3. Run with Docker:

    ```bash
    docker run --rm -it  c:latest 
    ```

Or you can do step 2 and 3 with `go.sh`.

## Output

Below is an example of the output on my machine, running with Docker.

```bash
Passes: 9210, Time: 5.000185, Avg: 0.000543, Limit: 1000000, Count: 78498, Valid: True

fvbakel_Cwords;9210;5.000185;1;algorithm=other,faithful=yes,bits=1
```

These results are with the following conditions:

- Intel(R) Core(TM) i7-3520M CPU @ 2.90GHz, Lubuntu 21.04 64 bit
- GCC Compiler: gcc (Ubuntu 7.5.0-3ubuntu1~18.04) 7.5.0
- running in Docker container alpine:3.13
- Docker version 20.10.2, build 20.10.2-0ubuntu2
