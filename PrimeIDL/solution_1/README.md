# IDL solution by KrizTioaN

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

Interactive Data Language ([IDL](https://www.l3harrisgeospatial.com/Software-Technology/IDL)) implementation of the prime sieve algorithm. A free and open-source IDL compiler is available as [GDL](https://gnudatalanguage.github.io/index.html).

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

No IDL or GDL? Just run it in Docker.

```shell
docker build -t primes .
docker run --rm primes 2>>/dev/null
```

## Output

IDL 8.8:

```shell
kriztioan;23;5.196117;1;algorithm=base;faithful=yes;bits=unknown
```

GDL 1.0.0-rc.3 git:

```shell
kriztioan;7;5.121231;1;algorithm=base;faithful=yes;bits=unknown
```
