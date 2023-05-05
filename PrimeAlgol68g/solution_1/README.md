# Algol 68g solution by rzuckerm

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-1-green)

* `primes.a68` uses `BOOL` for each sieve item
* `primes_bit.a68` uses `BITS` to contain a machine word worth of sieve items,
  where a machine word is defined by the `a68g` system variable `bits width`
  (typically 32 or 64 -- 32 on my system for some reason)

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
Passes: 153, Time: 5.00621800, Avg: .03272038, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true

rzuckerm;153;5.00621800;1;algorithm=base,faithful=yes

Passes: 23, Time: 5.09794500, Avg: .22164978, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true

rzuckerm;23;5.09794500;1;algorithm=base,faithful=yes,bits=1
```

