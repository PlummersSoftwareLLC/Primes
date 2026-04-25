# cwager_x64ff_mt

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

`cwager_x64ff_mt` is a multithreaded x86-64 NASM implementation of the base Sieve of Eratosthenes.

The main source file is `cwager_x64ff_mt.asm`.

Each worker thread repeatedly:

- allocates a fresh sieve buffer dynamically at runtime
- initializes that buffer from scratch
- runs a faithful base-algorithm sieve over odd candidates only
- discards the sieve unless it is the final completed pass for that worker

The implementation keeps the benchmark state in a dynamically allocated benchmark structure and gives each worker its own dynamically allocated worker state structure.

Each worker state structure contains the runtime sieve metadata and the sieve buffer pointer:

- sieve size
- derived bit count
- derived word count
- pass count
- sieve buffer pointer

This is intended to be the assembly equivalent of the "class containing the full state of the sieve" required by the current drag-race faithfulness rules. The sieve size and corresponding buffer are established dynamically at runtime for each worker, and each timed pass recreates a fresh sieve buffer from scratch before running the base algorithm.

## Run instructions

### NASM/GCC

From this directory:

```bash
./build.sh
./run.sh
```

This produces the executable `cwager_x64ff_mt` from `cwager_x64ff_mt.asm`.

### Docker

```bash
docker build -t cwager-x64ff-mt .
docker run --rm cwager-x64ff-mt
```

## Output

Example output from this machine:

```text
cwager_x64ff_mt;128068;5.001;16;algorithm=base,faithful=yes,bits=1
cwager_x64ff_mt;128397;5.000;16;algorithm=base,faithful=yes,bits=1
```

## Notes

This implementation targets amd64/x86-64 Linux, links against pthreads, and reports using the label `cwager_x64ff_mt`.
