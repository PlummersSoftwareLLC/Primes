# Implementation in C

![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is an implementation in C. The basic calculation is based on [solution_2/sieve_1of2.c](../solution_2/sieve_1of2.c) by Daniel Spångberg. However, this implementation has two solutions build on top of that.

1. `prime_words`: This solution makes use of the segmented algorithm and has a L1 cache optimization.
2. `primes_striped-block`: This solution makes use of the `striped-blocks` algorithm that is based on [Rust solution 1](../../PrimeRust/solution_1) by @mike-barber.

## The segmented algorithm

This algorithm is based on the fact that the cross-out pattern repeats at the product of the found primes. So for example:

If we have crossed out `3` then the words in the sieve will have the pattern below:

`WORD0,WORD1,WORD2,WORD3,WORD1,WORD2,WORD3,WORD1, etc`

After `5` is crossed out we get a similar repeating pattern, but this time after `3 * 5 = 15` words. Note that we can exclude the prime `2` because we are considering already only odd numbers in the sieve.

This algorithm makes use of this repeating pattern. If we know the repeating pattern then we can copy that pattern, instead of multiple bit operations on the same words to achieve the same result.

### Considerations

- It is only efficient if it affects every word. So only primes are considered that are  smaller than half the word size. This means that in the cross-out phase each step would always affect every word because the step size is smaller than one word.
- The copy of the pattern has to occur often enough to get benefit from the repeating pattern calculation. There for a check is done if the limit is at least 4 words.
- The copy makes only sense for prime number whose product is smaller than the sieve size.
- To make the calculation fair, no knowledge of what the prime numbers are is included in the algorithm. In other words, the repeating pattern and what primes to use for that pattern is calculated in each pass and depends on the word size and sieve size.

### Calculation steps

The algorithm consists of the following steps:

1. First calculate all the primes in the first word.
2. Find the range of product of primes that are larger than the word size and smaller than sieve size. Store these primes and product in an array. The last entry in the array has for prime the value `-1` and for product the number of words in the bit array.
3. Run the sieve from the first prime found in step 2 to and including the word that is equal to the first product found in step 2. Stop after the first prime of step 2 is processed.
4. Fill the array until and including the second product found in step 2 with copies of the words from word `1` to the product found in step 2.
5. Repeat step 3 and 4 foreach prime found in step 2 but this time start with the prime from the array. For the last prime use the number of words as the upper bound for the copy.
6. Cross-out the remaining primes as usual, start with the largest prime from step 2 plus 2.

## L1 cache optimization

A CPU has level one (L1) cache. This is the fastest memory the CPU can use, but it is limited in size. On my CPU it is 32kb. This is about half the size in case of one million as a limit. To have the sieve stay as long as possible in the L1 cache, the processing of the cross out is done in blocks. This has a big improvement of about 20% on my hardware. The optimal size of the blocks depends on the hardware, so this solution first searches for the best block size parameter for the hardware it is running on.

This optimization was inspired by the Cython solution 1 by ssolvest.

## Choice of Dockerfile

This solution uses Ubuntu 18.04 as the base image over alpine:3.13 because the `memcpy` function on alpine is slow due to the `musl libc`. The speed in this solution comes from the use of  `memcpy`, so it is extra slow on alpine.

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
Passes: 17338, Time: 5.000208, Avg: 0.000288, Limit: 1000000, Count: 78498, Valid: True

fvbakel_Cwords;17338;5.000208;1;algorithm=other,faithful=yes,bits=1
Passes: 6773, Time: 5.000336, Avg: 0.000738, Limit: 1000000, Count: 78498, Valid: True

fvbakel_Cstriped-block;6773;5.000336;1;algorithm=base,faithful=yes,bits=1
```

These results are with the following conditions:

- Intel(R) Core(TM) i7-3520M CPU @ 2.90GHz, Lubuntu 21.04 64 bit
- GCC Compiler: gcc (Ubuntu 7.5.0-3ubuntu1~18.04) 7.5.0
- running in Docker container Ubuntu:18.04
- Docker version 20.10.2, build 20.10.2-0ubuntu2
