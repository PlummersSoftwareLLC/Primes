# Scala (JVM and Native) solution by rom1dep

![Category](https://img.shields.io/badge/Category-faithful-green)

This is a Scala implementation of the "drag race" for Scala, targetting both Scala Native (see https://scala-native.org) and the JVM (see https://scala-lang.org)

# Run Instructions

You may compile this scala source targetting either native or the JVM:

- JVM

`./mill sieveJVM.run`

- Native

`./mill sieve.run`

Note: this may take a while with the default compile options (ReleaseFull) with optimizations

## Output

Should print something like:

`rom1dep;2424;5000;1`
