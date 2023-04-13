# Euphoria solution by rzuckerm

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-1-green)

* `primes.ex` uses `integer` for each sieve item
* `primes_bit.ex` packs 32 sieve items in an `integer`. The bit manipulation functions are inlined
  in the sieve to improve performance.

## Run instructions

Build the docker image with this:

```bash
./build.sh
```

You should only need to do this once. Run the docker image:

```bash
./run.sh
```

## Output

On an Intel(R) Core(TM) i7-8700 CPU @ 3.20GHz with 32 GB of memory on a Windows 10 desktop running
a Ubuntu 22.04 VM in VirtualBox 6.1:

```
Passes: 388, Time: 5.00, Avg: 0.01288660, Limit: 1000000, Count: 78498, Valid: true

rzuckerm-bool;388;5.00;1;algorithm=base,faithful=yes

Passes: 3035, Time: 5.00, Avg: 0.00164745, Limit: 1000000, Count: 78498, Valid: true

rzuckerm-bit;3035;5.00;1;algorithm=base,faithful=yes,bits=1
```
