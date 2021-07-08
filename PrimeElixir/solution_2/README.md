# Elixir solution by ityonemo

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-64-green)

This is an implementation in Elixir. This implementation uses :counters module
which provides a mutable array of counters.  These counters, are, unfortunately
64-bit integers:  http://erlang.org/doc/man/counters.html#description, which is
a part of the reason why they suck.

This will run several different thread counts to demonstrate how erlang processes
that are CPU-saturated will (or won't) scale.

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
ityonemo;207;5.02;1;algorithm=base,faithful=yes
ityonemo;357;5.024;2;algorithm=base,faithful=yes
ityonemo;427;5.015;3;algorithm=base,faithful=yes
ityonemo;510;5.02;4;algorithm=base,faithful=yes
ityonemo;507;5.032;5;algorithm=base,faithful=yes
ityonemo;543;5.042;6;algorithm=base,faithful=yes
ityonemo;551;5.049;7;algorithm=base,faithful=yes
ityonemo;567;5.048;8;algorithm=base,faithful=yes
```
