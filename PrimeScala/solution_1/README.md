# Scala (JVM and Native) solution by rom1dep

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This is a Scala implementation of the "drag race" for Scala, targeting both Scala Native (see https://scala-native.org) and the JVM (see https://scala-lang.org)

# Run Instructions

You may compile this scala source targeting either native or the JVM:

- JVM

`./mill sieveJVM.run`

- Native

`./mill sieve.run`

Note: this may take a while with the default compile options (ReleaseFull) with optimizations

## Output

Should print something like:

`rom1dep;2424;5.002;1;algorithm=base,faithful=yes`
