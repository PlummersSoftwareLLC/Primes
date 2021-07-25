# Elixir solution by Thomas9911

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

In Elixir it is possible to address a single bit in a binary (or what other programming languages call a string (technically not exactly the same) ). This is just a show case that it is possible, because it is not really fast and also not readable.

## Run instructions

Local:

```sh
MIX_ENV=prod mix run -e 'PrimeSieve.main'
```

Docker:

```sh
docker build -t primeelixir .
docker run primeelixir
```

## Output

Intel Core i7 4770k, Windows 10

```txt
thomas9911;34;5.732;1;algorithm=base,faithful=yes,bits=1
```

AMD Ryzen 7 5800H, Ubuntu 21.04 in Docker

```txt
thomas9911;48;5.156;1;algorithm=base,faithful=yes,bits=1
```
