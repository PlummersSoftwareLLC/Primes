# Implementation in C

![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is an implementation in C.
The algorithm is developed in NodeJS and C in parallel. 

## The extend algorithm
The extend algorithm marks all the multiples of a prime factor in the range of the product of the prime and all previous primes x2. E.g.: all multiples of 2,3 and 5 are marked until 2x (1x2x3x5) = 30. The range from 15-30 is a reoccuring pattern. So when we find 7, we can extend the pattern 15-30 until 7x15 = 105. Then we can mark all multiples of 7, and so on. So by gradually extending the seive by repeating the current pattern, we can have significant efficiency gains. 

For larger primes, the range will be so large that it can't be effiently handled by the L1 cache. Therefore, the sieve is divided in blocks, so that the multiples are handler per group. The blocks can be entirely independent (start at prime x and then use the extend algortim again), but a hybrid approach is faster: keep extending till the range for the first product of primes extends the sieve. Then, stripe of per block. 

A number of techniques have been used to enable bit-level patterns to be extended fast. 
Also all possible optimizations have been used to speed up the code in C.
On initalization, a small benchmark searches for the right settings for the hardware and OS environment. 
This defaults to a really small and short tuning. Using the command line, this can be intensified.

Inspired by: 
- nodeJS/solution_1 - rogiervandam-memcopy. This is the implementation in C, to see how much speed can be gained by moving from nodeJS to C. 
- PrimeC/solution_3 - fvbakel C-words. The segmented algorithm has similar concepts. But the extended algorithm implementations takes it further, by speed gains with a sub-byte (bit) level algorithms (race, pattern, small vs largestep optimizations).
- PrimeRust/solution_1 - Michael Barber. Inspired the manual loop unroll optimization
- PrimeC/solution_2 - danielspaangberg_1of2_epar. Inspired the multiprocessor versions. 

## Lessons learned
- 32 bit vectors can be faster than 64 bit
- Use #pragma ivdep to signal the compiler that it should not care about rereading memory in a loop.
- Use manual unroll for small sizes
- Small changes in code can have huge impact due to -Ofast of -O3 optimizations
- Using vector can greatly speed thing up, because of the sse/avx extensions
- __attribute__((always_inline)) can force inlining a function
- doing while (index<range_stop) and then if (index==range_stop) is faster than while (index<=range_stop)

Sources:
- https://www.agner.org/optimize/ - excellent manuals on optimization
- https://stackoverflow.com/questions/21681300/diferences-between-pragmas-simd-and-ivdep-vector-always
- https://stackoverflow.com/questions/25248766/emulating-shifts-on-32-bytes-with-avx
- https://stackoverflow.com/questions/3005564/gcc-recommendations-and-options-for-fastest-code
- https://github.com/simd-everywhere/simde
- https://www.cprogramming.com/tips/tip/common-optimization-tips
- https://codeforces.com/blog/entry/96344?locale=ru

## Run instructions


### Build and run native
To run this solution you need the gcc compiler and dependencies.

```bash
cd path/to/sieve
./compile.sh
./run.sh
```
or use the shortcut ./test.sh sieve_extend

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

Or do it all in one go:

```bash
docker build --pull --rm -f "Dockerfile" -t c:latest .; docker run c:latest 
```

Remember you can go in to the container like this:

```bash
docker run -it --entrypoint /bin/bash c:latest
```

### Command line options
```bash
Usage: ./sieve_extend [options] [maximum]
Options:
  --block <kilobyte> Set the block size to a specific <size> in kilobytes
  --check            Check the correctness of the algorithm
  --help             This help function
  --show  <maximum>  Show the primes found up to the maximum
  --time  <seconds>  The maximum time (in seconds) to run passes of the sieve algorithm
  --tune  <level>    find the best settings for the current os and hardware
                     1 - fast tuning
                     2 - refined tuning
                     3 - maximum tuning (takes long)
  --verbose <level>  Show more output to a certain level:
                     1 - show phase progress
                     2 - show general progress within the phase
                     3 - show actual work
```

## Output
The extension means the following:
```bash
u32v8b31t3
u32---------> unsigned 32 bit to store integers
   v8-------> vectorized extending and striping using 8 x u 32 (or otherwise specified above)
     b31----> block size of 31kB 
        t3--> threads = 3 
```

Below is an example of the output on my machine, running with Docker.
```bash
rogiervandam_extend-u64v2b31;62520;5.000061;1;algorithm=other,faithful=yes,bits=1
rogiervandam_extend-u32v4b32;62281;5.000014;1;algorithm=other,faithful=yes,bits=1
rogiervandam_extend-u32v8b32;68550;5.000067;1;algorithm=other,faithful=yes,bits=1
rogiervandam_extend-u64v4b32;72737;5.000043;1;algorithm=other,faithful=yes,bits=1
rogiervandam_extend-u64v8b31;37117;5.000108;1;algorithm=other,faithful=yes,bits=1
rogiervandam_extend_epar-u64v2b31t12;326098;5.000173;12;algorithm=other,faithful=yes,bits=1
rogiervandam_extend_epar-u64v2b31t6;291160;5.000140;6;algorithm=other,faithful=yes,bits=1
rogiervandam_extend_epar-u64v2b31t3;168067;5.000066;3;algorithm=other,faithful=yes,bits=1
rogiervandam_extend_epar-u64v2b31;60189;5.000004;1;algorithm=other,faithful=yes,bits=1
rogiervandam_extend_epar-u32v4b15t12;323704;5.000171;12;algorithm=other,faithful=yes,bits=1
rogiervandam_extend_epar-u32v4b15t6;270277;5.000113;6;algorithm=other,faithful=yes,bits=1
rogiervandam_extend_epar-u32v4b15t3;157055;5.000071;3;algorithm=other,faithful=yes,bits=1
rogiervandam_extend_epar-u32v4b15;55885;5.000028;1;algorithm=other,faithful=yes,bits=1
rogiervandam_extend_epar-u32v8b15t12;372283;5.000150;12;algorithm=other,faithful=yes,bits=1
rogiervandam_extend_epar-u32v8b15t6;296594;5.000084;6;algorithm=other,faithful=yes,bits=1
rogiervandam_extend_epar-u32v8b15t3;175971;5.000070;3;algorithm=other,faithful=yes,bits=1
rogiervandam_extend_epar-u32v8b15;62281;5.000028;1;algorithm=other,faithful=yes,bits=1
rogiervandam_extend_epar-u64v4b32t12;349984;5.000152;12;algorithm=other,faithful=yes,bits=1
rogiervandam_extend_epar-u64v4b32t6;302958;5.000100;6;algorithm=other,faithful=yes,bits=1
rogiervandam_extend_epar-u64v4b32t3;181114;5.000065;3;algorithm=other,faithful=yes,bits=1
rogiervandam_extend_epar-u64v4b32;64122;5.000054;1;algorithm=other,faithful=yes,bits=1
rogiervandam_extend_epar-u64v8b32t12;264962;5.000275;12;algorithm=other,faithful=yes,bits=1
rogiervandam_extend_epar-u64v8b32t6;172144;5.000174;6;algorithm=other,faithful=yes,bits=1
rogiervandam_extend_epar-u64v8b32t3;93330;5.000147;3;algorithm=other,faithful=yes,bits=1
rogiervandam_extend_epar-u64v8b32;32110;5.000124;1;algorithm=other,faithful=yes,bits=1
```
