# Erlang solution by jesperes

Straight-forward single-threaded Erlang solution using `atomics` to
track the prime numbers.

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-64-yellowgreen)

## Run instructions

```shell
$ rebar3 escriptize && _build/default/bin/PrimeErlang
```

## Output

```shell
> rebar3 escriptize && _build/default/bin/PrimeErlang
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling PrimeErlang
===> Building escript for PrimeErlang...
jesperes;46;5.02126;1;algorithm=base,bits=64,faithful=yes
```
