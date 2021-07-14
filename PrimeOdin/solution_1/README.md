# Odin solution by omgitsmoe

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

Faithful [Odin](https://odin-lang.org/) implementation of the base algorithm ([based on davepl's
C++ version](https://github.com/PlummersSoftwareLLC/Primes/tree/drag-race/PrimeCPP/solution_1))
with 1- and 8-bit versions.

## Run instructions

### Locally

See the [Odin wiki](https://github.com/odin-lang/Odin/wiki#getting-started-with-odin) on how to
setup a dev environment for your system.

Then run:
```
odin run main.odin -opt:3 -no-bounds-check
```

### Docker

Navigate to this solution's directory and run the following commands:

```
docker build -t odin-primes .
docker run --rm odin-primes
```

## Output

Both the 1-bit as well as the 8-bit storage version will be run.

On a Ryzen 1600X using Odin version `dev-2021-07`:
```
odin_bit_moe;8482;5.001;1;algorithm=base,faithful=yes,bits=1
odin_byte_moe;9775;5.000;1;algorithm=base,faithful=yes,bits=8
```
