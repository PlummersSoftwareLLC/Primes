# Prolog solution by [jimbxb](https://github.com/jimbxb)

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

A collection of solutions written in Prolog. 

### Solution 1.1 - Basic

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Uses Prolog's infinite-width integers to store the state of the sieve wrapped in the `bitvector/2` functor.

### Solution 1.2 - Dynamic

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

Uses a dynamically defined functor (`composite/2`) to track the state of the currently known composite numbers. 

### Solution 1.3 - C Foreign Interface

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Uses SWI Prolog's foreign interface with C to implement a more optimised version of the `bitvector/2` functor.

## Run

### Locally

```sh
$ cd path/to/prolog/solution
$ swipl-ld -cc-options,-w,-O3 -shared -o bitvector bitvector.c
$ ./run.sh
```

### Docker

```sh
$ cd path/to/dockerfile
$ docker build -t prolog-primes .
$ docker run --rm prolog-primes
```

## Output

```sh
$ ./run.sh
jimbxb-prolog-dynamic;4;5.586000;1;algorithm=base,faithful=no
jimbxb-prolog-basic;1;7.271000;1;algorithm=base,faithful=yes,bits=1
jimbxb-prolog-c;56;5.056000;1;algorithm=base,faithful=no,bits=1
```

## Acknowledgements

C Foreign Language Interface code is inspired by the SWI-PL examples [here](https://www.swi-prolog.org/pldoc/man?section=foreign).