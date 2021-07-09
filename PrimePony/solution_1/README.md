# Pony implementation

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

## Run

### Run locally

Pony installation instructions are available at: https://github.com/ponylang/ponyc/blob/main/INSTALL.md

### Docker

```
docker build -t primes .
docker run --rm primes
```

## Benchmarks

**Machine:** MacBook Pro (13-inch, 2018, Four Thunderbolt 3 Ports)<br/>
**Processor:** 2.3 GHz Quad-Core Intel Core i5<br/>
**Memory:** 16GB 2133 MHz LPDDR3<br/>

```
marghidanu;2203;5;1;algorithm=base,faithful=yes,bits=1
marghidanu;2231;5;1;algorithm=base,faithful=yes,bits=1
marghidanu;2272;5;1;algorithm=base,faithful=yes,bits=1
marghidanu;2325;5;1;algorithm=base,faithful=yes,bits=1
marghidanu;2338;5;1;algorithm=base,faithful=yes,bits=1
marghidanu;2396;5;1;algorithm=base,faithful=yes,bits=1
marghidanu;1974;5;1;algorithm=base,faithful=yes,bits=1
marghidanu;2438;5;1;algorithm=base,faithful=yes,bits=1
marghidanu;2457;5;1;algorithm=base,faithful=yes,bits=1
marghidanu;2475;5;1;algorithm=base,faithful=yes,bits=1
```

## Notes

I couldn't install the compiler on macOS; the entire development has been done inside a Docker container.
Interesting language with exciting features, documentation could be better. I would love to see another take on this. 

More information at https://www.ponylang.io/

## Author

Tudor Marghidanu
https://marghidanu.com/