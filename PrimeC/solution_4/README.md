# C solution by Nick Merriam and GLIWA team

![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Single threaded implementation in C of prime number sieve.

Several algorithm optimizations are applied to achieve a fast running time.

- Lazy wheel, see https://eprints.whiterose.ac.uk/3784/1/runcimanc1.pdf

## Run instructions

Compile with GCC optimizing for fast execution and then running the
compiled program by executing this script:

> $ ./run

Alternatively, you can use docker to run this code from a container.

```
$ docker build -t drag-race .
$ docker run drag-race
```

or as a one-liner:

```
$ docker run --rm $(docker build -q .)
merriam;14196;5.0;1
```

