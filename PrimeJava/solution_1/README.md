# Plain Java solution by MansenC

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)


A faithful and straight-forward implementation of the sieve. Creates a `boolean` array with all indexes `true`, then jogs through it, following the Eratosthenes algorith, setting all non-primes to `false`. (Simplified and sped up by [PEZ](https://github.com/PEZ).)

Run it like so:

```sh
$ javac Sieve.java && java Sieve [-limit <limit>] [-warmup]
```

Defaults to `limit` = 1000000, and no warmup.

Run it with docker like so:

```sh
$ docker pull openjdk:17
$ docker build -t pez-primes-java .
$ docker run --rm -it pez-primes-java
```

Sample output:

```
Passes: 10324, Time: 5.000000, Avg: 0.000484, Limit: 1000000, Count: 78498, Valid: true
MansenC+pez;10324;5.000000;1;algorithm=base,faithful=yes,bits=8
```