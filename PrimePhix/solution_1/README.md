# Phix solution by rzuckerm

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This implementation is almost identical to [Euphoria solution 1](../../PrimeEuphoria/solution_1).
[Phix](http://phix.x10.mx/) is a newer language based on
[Euphoria](https://en.wikipedia.org/wiki/Euphoria_(programming_language)). The performance
of the `integer` sieve solution is about 2.4 times faster in Phix, but the 1-bit sieve
solution is about 6.7 times slower. There are a number of problems with Phix.
The interpreter needs to be run through `ld-linux-x86-64.so` or it crashes with a SEGFAULT.
The compiler (which I hoped would greatly improve performance) crashes with a SEGFAULT
regardless of whether it runs through `ld-linux-x86-64.so` or not.

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
Passes: 953, Time: 5.00248559, Avg: 0.00524920, Limit: 1000000, Count: 78498, Valid: true

rzuckerm;953;5.00248559;1;algorithm=base,faithful=yes

Passes: 209, Time: 5.02, Avg: 0.02401389, Limit: 1000000, Count: 78498, Valid: true

rzuckerm;209;5.02;1;algorithm=base,faithful=yes,bits=1
```

