# Plain Java solution by MansenC and PEZ

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)


Two faithful and straight-forward implementation of the sieve.

1. Creates an array of `boolean`s with all indexes `false`, then jogs through it, following the Eratosthenes algorithm, setting all non-primes to `true`.
2. Creates a `BitSet` array with all bits `true`, then jogs through it, following the Eratosthenes algorithm, setting all non-primes to `false`.

Run it like so:

```sh
$ javac PrimeSieveJava.java && java PrimeSieveJava [-limit <limit>] [-warmup]
$ javac PrimeSieveJava.java && java PrimeSieveJava [-limit <limit>] [-variant bitset|array] [-warmup]
```

Defaults to `limit` = 1000000, `variant = array`, and no warmup.

Run it with docker like so:

```sh
$ docker build -t pez-primes-java .
$ docker run --rm -it pez-primes-java
```


NB: There is something strange going on with the `BitSet` variant. It often looses half of its performance for yet unknown reasons. Only seen on Linux x64 + Docker so far. Here's the output from 5 runs:

```
Passes: 9614, Time: 5.000000, Avg: 0.000520, Limit: 1000000, Count: 78498, Valid: true
MansenC+pez-boolean-array;9614;5.000000;1;algorithm=base,faithful=yes,bits=8
Passes: 6045, Time: 5.000000, Avg: 0.000827, Limit: 1000000, Count: 78498, Valid: true
pez-bitset;6045;5.000000;1;algorithm=base,faithful=yes,bits=1
Passes: 9673, Time: 5.000000, Avg: 0.000517, Limit: 1000000, Count: 78498, Valid: true
MansenC+pez-boolean-array;9673;5.000000;1;algorithm=base,faithful=yes,bits=8
Passes: 5991, Time: 5.000000, Avg: 0.000835, Limit: 1000000, Count: 78498, Valid: true
pez-bitset;5991;5.000000;1;algorithm=base,faithful=yes,bits=1
Passes: 9759, Time: 5.000000, Avg: 0.000512, Limit: 1000000, Count: 78498, Valid: true
MansenC+pez-boolean-array;9759;5.000000;1;algorithm=base,faithful=yes,bits=8
Passes: 2758, Time: 5.001000, Avg: 0.001813, Limit: 1000000, Count: 78498, Valid: true
pez-bitset;2758;5.001000;1;algorithm=base,faithful=yes,bits=1
Passes: 9703, Time: 5.000000, Avg: 0.000515, Limit: 1000000, Count: 78498, Valid: true
MansenC+pez-boolean-array;9703;5.000000;1;algorithm=base,faithful=yes,bits=8
Passes: 6020, Time: 5.000000, Avg: 0.000831, Limit: 1000000, Count: 78498, Valid: true
pez-bitset;6020;5.000000;1;algorithm=base,faithful=yes,bits=1
Passes: 9753, Time: 5.000000, Avg: 0.000513, Limit: 1000000, Count: 78498, Valid: true
MansenC+pez-boolean-array;9753;5.000000;1;algorithm=base,faithful=yes,bits=8
Passes: 2755, Time: 5.001000, Avg: 0.001815, Limit: 1000000, Count: 78498, Valid: true
pez-bitset;2755;5.001000;1;algorithm=base,faithful=yes,bits=1
```