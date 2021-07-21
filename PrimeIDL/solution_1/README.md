# IDL solution by KrizTioaN

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-green)

Interactive Data Language ([IDL](https://www.l3harrisgeospatial.com/Software-Technology/IDL)) implementations of the prime sieve algorithm. A free and open-source IDL compiler is available as [GDL](https://gnudatalanguage.github.io/index.html).

Two implementations are collected:

1. One has the least, in terms of speed, favorable [characteristics](https://github.com/PlummersSoftwareLLC/Primes/blob/drag-race/CONTRIBUTING.md#characteristics); loops are slow, really slow, in IDL ...
2. One has more favorable [characteristics](https://github.com/PlummersSoftwareLLC/Primes/blob/drag-race/CONTRIBUTING.md#characteristics) and does things more the 'IDL-way'.

## Run instructions

```shell
. run.sh
```

The script will check for and use [IDL](https://www.l3harrisgeospatial.com/Software-Technology/IDL) (license required) when available. Otherwise it will use free and open-source [GDL](https://gnudatalanguage.github.io/index.html).

### Docker

No IDL or GDL? Run it with Docker.

```shell
docker build -t primes .
docker run --rm primes
```

## Output

IDL 8.8:

```shell
kriztioan_1bit;7;5.553826;1;algorithm=base,faithful=yes,bits=1
kriztioan_idlway;717;5.005487;12;algorithm=base,faithful=yes,bits=8
```

GDL 1.0.0-rc.3 git:

```shell
kriztioan_1bit;2;5.370245;1;algorithm=base,faithful=yes,bits=1
kriztioan_idlway;363;5.010998;6;algorithm=base,faithful=yes,bits=8
```
