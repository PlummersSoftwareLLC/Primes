# Elixir solution by cdesch

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This is an implementation in Elixir. This implementation uses recursion in place of a while loop since Elixir does not have a true while loop. Elixir is also immutable requiring the bit array in Dave's implementation to be transformed and returned.

## Run instructions

### Elixir

Execute the following commands from the implementation directory:

Setup

```sh
mix deps.get
```

Build

```sh
MIX_ENV=prod mix escript.build
```

Run

```sh
./prime_sieve
```

Test

```sh
mix test
```

Static Analysis

```sh
mix credo
```

### Docker

A Dockerfile has been provided.

Build Dockerfile

```sh
docker build -t elixir-prime .
```

Run

```sh
docker run --rm elixir-prime .
```

## Output

```sh
cdesch;1;32.212;1;algorithm=base,faithful=no
```
