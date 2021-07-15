# Odin solution by [omgitsmoe](https://github.com/omgitsmoe), [Ginger Bill](https://github.com/gingerBill) and [Kelimion](https://github.com/Kelimion)

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

Faithful [Odin](https://odin-lang.org/) implementation of the base algorithm ([based on davepl's
C++ version](https://github.com/PlummersSoftwareLLC/Primes/tree/drag-race/PrimeCPP/solution_1))
with 1- and 8-bit versions, which will be run in both single and multiple threads.

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

Thanks to rbergen for helping me with the Docker image.

## Output

Both the 1-bit as well as the 8-bit storage version will be run.

On a Ryzen 1600X using Odin version `dev-2021-07`:
```
odin_bit_moe;8985;5.000;1;algorithm=base,faithful=yes,bits=1
odin_byte_moe;14818;5.000;1;algorithm=base,faithful=yes,bits=8
odin_bit_threaded_moe;57279;5.005;12;algorithm=base,faithful=yes,bits=1
odin_byte_threaded_moe;77369;5.038;12;algorithm=base,faithful=yes,bits=8
```
