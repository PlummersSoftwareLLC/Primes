# IDL solution by KrizTioaN

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

Interactive Data Language ([IDL](https://www.l3harrisgeospatial.com/Software-Technology/IDL)) implementation of the prime sieve algorithm. A free and open-source IDL compiler is available as [GDL](https://gnudatalanguage.github.io/index.html).

This solution has more favorable [characteristics](https://github.com/PlummersSoftwareLLC/Primes/blob/drag-race/CONTRIBUTING.md#characteristics) compared to [solution 1](https://github.com/PlummersSoftwareLLC/Primes/tree/drag-race/PrimeIDL/solution_1) and does things more the 'IDL-way'.

## Run instructions

IDL (license required):

```shell
idl -quiet -e primeidl
```

GDL (free):

```shell
gdl -quiet -e primeidl
```

### Docker

No IDL or GDL? Just run it with Docker.

```shell
docker build -t primes .
docker run --rm primes 2>>/dev/null
```

## Output

IDL 8.8:

```shell
kriztioan;713;5.001624;12;algorithm=base;faithful=yes;bits=8
```

GDL 1.0.0-rc.3 git:

```shell
kriztioan;375;5.000420;6;algorithm=base;faithful=yes;bits=8
```
