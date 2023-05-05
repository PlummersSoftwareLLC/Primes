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
solution is about 6.7 times slower. Sadly, the compiled executable was not significantly
faster than the interpreted version.

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
Passes: 1006, Time: 5.00226523, Avg: 0.00497243, Limit: 1000000, Count: 78498, Valid: true

rzuckerm;1006;5.00226523;1;algorithm=base,faithful=yes

Passes: 210, Time: 5.00272310, Avg: 0.02382249, Limit: 1000000, Count: 78498, Valid: true

rzuckerm;210;5.00272310;1;algorithm=base,faithful=yes,bits=1
```
