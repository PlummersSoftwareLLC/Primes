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
odin run . -o:speed -no-bounds-check
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

On a Ryzen 9 9950X3D using Odin version `dev-2025-12`:
```
odin_bit_moe;20782;5.0001343379999996;1;algorithm=base,faithful=yes,bits=1
odin_byte_moe;29614;5.000120271;1;algorithm=base,faithful=yes,bits=8
odin_bit_threaded_moe;325264;5.0082490579999996;32;algorithm=base,faithful=yes,bits=1
odin_byte_threaded_moe;538477;5.007647265;32;algorithm=base,faithful=yes,bits=8
```
