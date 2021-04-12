# PrimeSiveHaxe

An implementation of a Prime Sieve in Haxe.

## What's Haxe?

See: https://haxe.org/

Haxe is a programming language that while it does have it's own interpreter, can also be cross-compiled to other languages such as C++, Python, C# & more.

## Running the Prime Sieve

1. Make sure you have a working [Haxe](https://haxe.org/) installation.

### Haxe Interpreter

2. `haxe interp.hxml`

### Python

2. `haxe python.hxml`
3. `python3 bin/py.py`

### C++

2. `haxe cpp.hxml`
   1. If you get a `Error: Library hxcpp is not installed`, run `haxelib install hxcpp`
3. `./bin/cpp/Main`

## My personal results.

Take these with a grain of salt. My machine is a MacBook Pro 2020 (M1, 8GB RAM) running macOS Big Sur.

**Haxe Interpreter**: `Passes: 48, Time 10.0865981578826904, Avg: 0.21013746162255606, Limit: 1000000, Count: 78499, Valid: false`

**C++**: `Passes: 2071, Time 10.0035741329193, Avg: 0.00483031102506968, Limit: 1000000, Count: 78499, Valid: false`

**Python**: `Passes: 27, Time 10.352768182754517, Avg: 0.38343585862053764, Limit: 1000000, Count: 78499, Valid: false`
