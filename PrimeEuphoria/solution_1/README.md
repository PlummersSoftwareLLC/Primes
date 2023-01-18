# Euphoria solution by rzuckerm

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-1-green)

* `primes.ex` uses `integer` for each sieve item
* `primes_bit.ex` packs 64 sieve items in an `atom`

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
Passes: 400, Time: 5.01, Avg: 0.01252500, Limit: 1000000, Count: 78498, Valid: true

rzuckerm;400;5.01;1;algorithm=base,faithful=yes

Passes: 1409, Time: 5.00, Avg: 0.00354862, Limit: 1000000, Count: 78498, Valid: true

rzuckerm;1409;5.00;1;algorithm=base,faithful=yes,bits=1
```
