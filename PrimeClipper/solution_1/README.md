# Clipper Solution by Andy Radford

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

Clipper is a rather ancient derivitive of the even more ancient dbase/xbase languages. It's kind of like NCurses and SQL put into one language i.e. it's *very* business oriented.

I feel Andy's glare at me for calling these things ancient, as it has a transitive property towards calling *him* ancient.

We are using a project called [Harbour](https://harbour.github.io/) which is very technically its own language, but for the most part it's just Clipper with a few extras, so we're sticking with calling it Clipper.

Plus it's basically the only way to run Clipper in Docker.

This solution was developed using an original copy of the CA Blinker program for Clipper 5.2e, so this is a fully compatible
clipper implementation.

(Also try to find a gif or video of blinker - it literally blinks at you!)

Special thanks to the maintainers of this repository for spending some of their weekend helping us with making this solution possible to run in Docker.

## Implementation note

The original Clipper has a unique limitation in that, arrays have unlimited dimensions however each dimension can only hold 4096 values.

Thus, we need to spread out the sieve state across multiple 4096-sized dimensions.

## Running with Docker

```
docker build -t clipper
docker run --rm -t clipper
```

## Running without Docker

Install Harbour then run:

```
hbmk2 -gtstd -optim -cflag=-O3 -cflag=-mtune=native ./sieve.prg
./sieve
```

## Example output
TODO