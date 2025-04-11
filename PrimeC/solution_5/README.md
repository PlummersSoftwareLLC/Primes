# Implementation in C

![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Algorithm](https://img.shields.io/badge/Algorithm-base-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is an implementation in C.  
The algorithm was developed in parallel using NodeJS and C.

## The Extend Algorithm
The extend algorithm marks all the multiples of a prime factor in the range of the product of the prime and all previous primes multiplied by 2. For example, all multiples of 2, 3, and 5 are marked until `2 × (1 × 2 × 3 × 5) = 30`. The range from 15 to 30 forms a recurring pattern. When handling a new prime, such as 7, this pattern can be extended multiple times up to `7 × 15 = 105`. Then, all multiples of 7 in this extended pattern (15–105) are marked. This pattern can then be copied further, e.g., 11 times, and so on. By gradually extending the sieve by repeating the current pattern, significant efficiency gains can be achieved.

For larger primes, the range becomes too large to be efficiently handled by the L1/L2 cache. To address this, the sieve is divided into blocks, allowing multiples to be handled in groups. These blocks can either be processed independently (starting at a prime and using the extend algorithm again) or using a hybrid approach, which is faster. The hybrid approach extends the range until the first product of primes exceeds the sieve, then processes the sieve block by block.

The extend algorithm deviates from the base algorithm by marking multiple values at once using pattern copying. This allows the use of words and vectors to speed up the process. Vectors are filled with step patterns and applied at appropriate locations. These vectors can be rotated to fit the next position, unlocking significant speedup potential. Various techniques have been employed to enable fast bit-level pattern extensions, and all possible optimizations have been applied to maximize performance in C.

### Inspirations
- **NodeJS/solution_1 - rogiervandam-memcopy**: This implementation in C demonstrates the speed gains achieved by transitioning from NodeJS to C.
- **PrimeC/solution_3 - fvbakel C-words**: The segmented algorithm shares similar concepts, but the extended algorithm takes it further with sub-byte (bit-level) optimizations, including pairing, patterning, and small vs. large step optimizations.
- **PrimeRust/solution_1 - Michael Barber**: Inspired the manual loop unroll optimization.
 
### Reference Algorithms: Classic and Base
For comparison, the framework includes classic and base implementations:
- **Classic Implementation**: Based on @davepl's implementation, using the same defines as the extend version. It includes an 8-bit version and a 64-bit version. Surprisingly, the 8-bit version is two (icore) to four (apple m1) times faster than the 64-bit version on modern processors.
- **Base Implementation**: Uses functions that set multiples of primes individually and employs the block approach described by Michael Barber in PrimeRust/solution_1.


### Lessons Learned about Speed optimization in C
- **Data Types**:
  - 8-bit handling with shift and mask is faster than 32-bit or 64-bit handling.
  - Using 32-bit integers for counters is faster than 64-bit integers, especially on Apple M1 processors.
- **Compiler Optimizations**:
  - Use `#pragma GCC ivdep` to signal the compiler to ignore memory rereads in loops.
  - Manual unrolling for small sizes provides significant performance gains.
  - Combining manual unrolling with unroll hints in `applymask` yields large performance improvements.
- **Memory Alignment**:
  - Align data to cache lines for optimal performance.
  - Use a single `malloc` for both the sieve and storage to reduce overhead.
- **Vectorization**:
  - Leverage SSE/AVX extensions for vector operations to achieve significant speedups.
  - Pair vector manipulations to improve performance further.
- **Compiler Flags**:
  - Small code changes can have a huge impact when using `-Ofast` or `-O3` optimizations.
  - Link-time optimization (`-flto`) is beneficial.
  - Use `__attribute__((always_inline))` to force function inlining (using `inline` alone is insufficient).
- **Loop Optimization**:
  - Using `while (index < range_stop)` followed by `if (index == range_stop)` is faster than `while (index <= range_stop)`.
- **Memory Allocators**:
  - Alpine Docker images are slow due to the standard `malloc`. Integrating `jemalloc` or `mimalloc` improves performance.
- **Math Optimizations**:
  - Integrated the canonical "doom" fast square root for speed improvements (see `src/sieve/sieve_calc`).

### Sources about speed optimization
- https://www.agner.org/optimize/ - excellent manuals on optimization
- https://stackoverflow.com/questions/21681300/diferences-between-pragmas-simd-and-ivdep-vector-always
- https://stackoverflow.com/questions/25248766/emulating-shifts-on-32-bytes-with-avx
- https://stackoverflow.com/questions/3005564/gcc-recommendations-and-options-for-fastest-code
- https://clang.llvm.org/docs/UsersManual.html
- https://github.com/simd-everywhere/simde
- https://www.cprogramming.com/tips/tip/common-optimization-tips
- https://gcc.gnu.org/onlinedocs/gcc/Vector-Extensions.html
- https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html
- https://en.wikipedia.org/wiki/Fast_inverse_square_root

## Source code organization
During development, a framework was created for benchmarking sieve functions. This framework spans the following directories:

```none
src             Contains the basic high-level algorithm files. Each file should include all necessary dependencies 
                and contain a function named "shakeSieve(counter_t sieve_size)"
src/benchmark   Contains all functions for benchmarking algorithms.
src/bitstorage  Contains functions that operate on a bitmap level without knowledge of "primes."
src/generic     Contains types, helper functions, and utilities for creating different function versions.
src/sieve       Contains functions for creating, calculating, striping, and extending the sieve.
```

On initialization, a small benchmark determines the optimal settings for the hardware and OS environment. By default, this benchmark is quick and lightweight, but it can be intensified using command-line options.

## Build and run instructions

### Building & running with the sieve command gadget
The ./sieve command is a bash script for building and running the sieve application. It reads command-line arguments, sets the necessary defines, builds the appropriate version of the sieve app in the build directory, and runs it automatically. The script can be used as if it were the final program. Additional build options include:

```none
./sieve compileall                                - Compile all possible versions and place them in the ./build/ directory.
./sieve runall                                    - Run all possible versions. In the Primeview Docker container, verbosity is reduced.
./sieve docker <dockerfile extension> <arguments> - Run sieve in a Docker container with the specified arguments. 
                                                    Looks for Dockerfile_<extension> in ./dev/docker/.
./sieve docker all <arguments>                    - Run sieve in all available Docker containers.
./sieve docker <dockerfile extension> set         - Set the specified Dockerfile as the default in ./.
./sieve <arguments> gcc <arguments>               - Use GCC as the compiler (can appear anywhere in the argument list).
./sieve <arguments> clang <arguments>             - Use Clang as the compiler (can appear anywhere in the argument list).
./sieve <arguments> icx <arguments>               - Use Intel's ICX compiler (can appear anywhere in the argument list).
./sieve sieve_base                                - Compile the base algorithm variant.
./sieve sieve_classic                             - Compile the classic algorithm variant.
./sieve sieve_extend                              - Compile the extend algorithm variant (default).
```
Alternatively, you can use make for building.

### Command line options
```none
Usage: ./sieve [options] [maximum]
[options] is one or more of the following:
  --check                   Check the correctness of the algorithm.
                            0 - No check.
                            1 - Check prime count for the sieve size.
                            2 - Check prime count for every sieve size.
                            3 - Check prime count for every sieve size and block size.
                            4 - Check stripe algorithms for the sieve size.
                            5 - Check stripe algorithms for every sieve size.
                            6 - Check stripe algorithms for the sieve size and every block size.
                            7 - Check all and halt.
  --nocheck                 Skip correctness checks.
  --explain                 Explain the steps of the algorithm (requires compilation with explain support).
  --help                    Display this help message.
  --max <maximum>           Set the maximum prime to examine.
  --set <string>            Specify multiple settings connected by hyphens (e.g., s063-l128-b0262144-v256-a1).
        s<factor>           Set the cutoff prime for blockwise striping.
        l<bits>             Set the cutoff number of bits for vectorwise striping.
        b<bits>             Set the block size in bits.
        v<size>             Set the vector size in bits.
        a<algorithm>        Set the algorithm to use.
  --show <maximum>          Display primes found up to the specified maximum.
  --threads <maximum>       Set the maximum number of threads (requires OpenMP support).
                            Use 'all' for all available threads or 'half' for half the threads (e.g., to avoid hyperthreading).
  --time <seconds>          Set the maximum runtime in seconds for sieve passes.
  --timers                  Display timings for submodules (requires compilation with timers; slower).
  --tune <level>            Tune settings for the current OS and hardware.
                            0 - No tuning.
                            1 - Fast tuning.
                            2 - Refined tuning.
                            3 - Benchmark individual stripe functions.
                            4 - Benchmark iterative stripe functions.
  --verbose <level>         Set verbosity level:
                            0 - Only show result string.
                            1 - Include additional settings information.
                            2 - Show general phase progress.
                            3 - Show detailed progress within phases.
                            4 - Show actual work.
                            5 - Show high-level plan.
                            6 - Show detailed plan.
                            7 - Show more details.
                            8 - Show debug details.
                            9 - Show timing information.
[maximum] defaults to 1,000,000 if not specified.

More options are available in the ./src/benchmark/sieve_options.h file.
```

### Run with Docker

To run the application using Docker:

1. Install Docker: <https://docs.docker.com/get-docker/>
2. Build the image:

    ```bash
    docker build --pull --rm -f "Dockerfile" -t c:latest "."
    ```

3. Run with Docker:

    ```bash
    docker run --rm -it c:latest 
    ```

Or do it all in one go for testing single thread versions:

```bash
docker build --pull --rm -f "Dockerfile" -t c:latest .; docker run --rm --cpuset-cpus="0" --cpu-shares=1024 c:latest 
```

To access the container's shell:

```bash
docker run -it --entrypoint /bin/bash c:latest
```

Command to create a dockerfile
```bash
sed -i 's/\r$//' sieve;./sieve docker alpine_gcc_mimalloc set
```

Command to run the formal benchmark for the primeview results:
```bash
cd ../..; make DIRECTORY=PrimeC/solution_5; cd PrimeC/solution_5
```

If you encounter issues running the ./sieve file (e.g., due to CRLF/LF problems), use the following command:

```bash
sed -i 's/\r$//' sieve
```

## Output
The output at verbosity 1 and up has some extra settings information.
Before doing the benchmark, the program tunes some settings. Theses settings are in the output.
```none
Example:
s063-l128-b0262144-v256-a1
s063-----------------------> Use striping for the entire sieve up to this factor.
     l128------------------> Use large vector steps until this factor.
          b0262144---------> Use this block size above the "s" factor.
                   v256----> Use vectors of size 128, 256, or 512.
                        a1-> Algorithm 1 (extend and stripe the entire sieve, then process blockwise),
                             or Algorithm 2 (process entirely blockwise; see "./src/sieve_extend.c" for details).
```

Below is an example of the output on my machine, running with Docker.
```none
rogiervandam_extend;83501;5.000029;1;algorithm=other,faithful=yes,bits=1;s063-l128-b0262144-v256-a1 total 83501
rogiervandam_base;20651;5.000002;1;algorithm=base,faithful=yes,bits=1;s122-l236-b0262144-v256-a1 total 20651
rogiervandam_classic;8571;5.000516;1;algorithm=base,faithful=yes,bits=1;s064-l128-b0262144-v256-a1 total 8571
rogiervandam_extend_epar;449870;5.000058;12;algorithm=other,faithful=yes,bits=1;s067-l134-b0131072-v256-a1 total 449870
rogiervandam_extend_epar;311811;5.000044;6;algorithm=other,faithful=yes,bits=1;s079-l128-b0262144-v256-a2 total 311811
rogiervandam_extend_epar;255389;5.000096;4;algorithm=other,faithful=yes,bits=1;s368-l124-b0262144-v256-a2 total 255389
rogiervandam_extend_epar;154190;5.000031;2;algorithm=other,faithful=yes,bits=1;s060-l130-b0262144-v256-a1 total 154190
```
