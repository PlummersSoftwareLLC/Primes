# GNU Make solution by jastein693

![Algorithm](https://img.shields.io/badge/Algorithm-base-green) ![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen) ![Parallelism](https://img.shields.io/badge/Parallel-no-green) ![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This is a prime sieve solution implemented in GNU Make.

### Implementation

The sieve uses only GNU Make syntax and shell commands are only used to time runs and print the results.

GNU Make has no arithmetic functions. Instead, numbers are represented by lists of strings and manipulated with [GNU Make Standard Library](https://gmsl.jgc.org/) arithmetic functions. An explanation of the arithmetic functions can be found [here](https://www.cmcrossroads.com/article/learning-gnu-make-functions-arithmetic).

Bits are also represented by single character strings.

### Performance

This solution is very slow because of the lack of real arithmetic functionality. The use of large string lists seems to cause memory issues and it will crash long before it every reaches a limit of one million. For that reason, the default limit is set to 1000 and this solution is excluded from the automated benchmark.

## Run instructions

gmsl is included as a git submodule. Run the following command in the solution directory to initialize and fetch the library:
```
git submodule update --init gmsl
```

### GNU Make

- With defaults `make`
- With optional arguments `make sieve_size=10000 time_to_run=1 print_results=true`

### Docker

- Build `docker build -t primes_gnu_make .`
- Run `docker run --rm primes_gnu_make`
- Run with optional arguments `docker run --rm primes_gnu_make sieve_size=10000 time_to_run=1 print_results=true`

## Output

```
Passes: 22, Time: 6, Limit: 1000, Count: 168
jastein693;22;6;1;algorithm=base,faithful=no
```