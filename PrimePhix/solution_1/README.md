# Euphoria solution by rzuckerm

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This implementation is almost identical to [Euphoria solution 1](../../PrimeEuphoria/solution_1).
[Phix](http://phix.x10.mx/) is a newer language based on
[Euphoria](https://en.wikipedia.org/wiki/Euphoria_(programming_language)). The performance
of the `integer` sieve solution is about twice the speed in Phix. However, there are a number
of issues with Phix. I was hoping to use the Phix compiler to further improve the performance,
bit I could not get it to work. Also, I wanted to implement the single-bit algorithm, but
the bitwise functions do not seem to work consistently.

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
Passes: 611, Time: 5.00348985, Avg: 0.00818902, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true

rzuckerm;611;5.00348985;1;algorithm=base,faithful=yes
```
