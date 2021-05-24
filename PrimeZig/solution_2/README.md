# Zig solution by Isaac Yonemoto

![Category](https://img.shields.io/badge/Category-faithful-green)

An implementation shamelessly stolen from DevBlok's and made faster.  Gotta go fast!

* optimizations:
- doesn't bother to do even numbers.  If you want the "real deal" array, you can use the
  `Sieve.isPrime(self: Self, target: []u8)` function
- the sieve function itself doesn't do the array allocation.  Consider recycling your memory!
  You want it on the stack?  You got it on the stack.

## Run instructions

Build:

```
zig build -Drelease-fast=true
```

Test:
```
zig test src/prime.zig
```

Run:
```
./zig-cache/bin/PrimeZig
```

## Output

```
Passes: 7854, Time: 5.00034, Avg: 0.00064, Limit: 1000000

zig-ityonemo;7854;5.00034;1
```
