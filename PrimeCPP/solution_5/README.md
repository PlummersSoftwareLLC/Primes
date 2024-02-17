# C++ constexpr solution by flo80

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Deviation](https://img.shields.io/badge/Deviation-compile%20time-blue)

This solution declares as much as possible as `constexpr`, allowing the compiler to calculate it during compile time.
Since the standard library does not provide required functions, sqrt and bitfield are custom versions.

*Deviation*: It does use the base algorithm but due to compile time optimization, a lot of the actual calculation does not happen at runtime

*Faithfulness*: Since the buffer size is fixed, it is [not considered faithful](https://github.com/PlummersSoftwareLLC/Primes/pull/274).

*Note*: this solution is limited to numbers up to around 50,000,000 (stack size limit on Mac OS it seems).

## Run instructions
### From CPP Binary

`./run.sh`, requires CLANG in a fairly recent version (supporting C++ 20)

### From Derived Assembly

The generated assembly code of this solution can be inspected by running the following command:

```shell
clang++ $CXX_ARGS -S -masm=intel PrimeCPP_CONSTEXPR.cpp -o PrimeAssembly.s
```

This code might be further optimised by a seasoned assembly developer. To generate the binary, run:

```shell
ASM_ARGS="-pthread -O3 -m64 -mtune=native"

clang++ $ASM_ARGS PrimeAssembly.s -o primes
```

and to run the solution simply execute the binary:

```shell
./primes
```

## Output

All on Dell M5510 (Intel Xeon E3-1505M CPU)

### Native performance

#### Single-threaded

| Index | Implementation | Solution | Label      | Passes    | Duration | Threads | Algorithm | Faithful | Bits | Passes/Second   |
|-------|----------------|----------|------------|-----------|----------|---------|-----------|----------|------|-----------------|
| 1     | cpp            | 5        | cosmic_dna | 139064742 | 5.00007  | 1       | base      | no       | 1    | 27812570.14905 |

##### Multi-threaded

| Index | Implementation | Solution | Label      | Passes    | Duration | Threads | Algorithm | Faithful | Bits | Passes/Second   |
|-------|----------------|----------|------------|-----------|----------|---------|-----------|----------|------|-----------------|
| 1     | cpp            | 5        | cosmic_dna | 486161732 | 5.00071  | 8       | base      | no       | 1    | 12152324.96125 |


### Compared to other C++ implementations:

#### Solution 3

##### Single-threaded

| Index | Implementation | Solution | Label               | Passes    | Duration | Threads | Algorithm | Faithful | Bits | Passes/Second   |
|-------|----------------|----------|---------------------|-----------|----------|---------|-----------|----------|------|-----------------|
| 1     | cpp            | 3        | flo80_pol_constexpr | 138251609 | 5.00049  | 1       | base      | no       | 1    | 27647584.68912 |

##### Multi-threaded

| Index | Implementation | Solution | Label               | Passes    | Duration | Threads | Algorithm | Faithful | Bits | Passes/Second   |
|-------|----------------|----------|---------------------|-----------|----------|---------|-----------|----------|------|-----------------|
| 1     | cpp            | 3        | flo80_pol_constexpr | 507404760 | 5.00087  | 8       | base      | no       | 1    | 12682899.49259 |
