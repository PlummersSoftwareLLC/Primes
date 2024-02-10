# Beef solution by rzuckerm

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

* `Primes/src/Primes.bf` uses 1 bit for each sieve item

## Caveats

The docker image only works on an AMD64 architecture. I was not able to get the
[Beef docker image](https://hub.docker.com/r/rzuckerm/beef) to build using ARM64.

## Run instructions

Build the docker image with this:

```bash
./build.sh
```

You should only need to do this once. Run the docker image:

```bash
./run.sh [<options>]
```

where `<options>` are optional command-line arguments:

* `--limit=<limit>` - Upper limit for calculating prime. Default value is 1000000
* `--time=<time>` - Time limit in seconds. Default value is 5
* `--show` - Print found prime numbers
* `--help` - Show help

## Output

On a 12th Gen Intel(R) Core(TM) i7-12850HX 2.10 GHz with 32 GB of memory on a Windows 10
laptop running a Ubuntu 22.04 VM in VirtualBox 7.0.6:

```
Passes: 11905, Time: 5000ms, Avg: 0.4199916002ms, Limit: 1000000, Count: 78498, Valid: True
rzuckerm;11905;5;1;algorithm=base,faithful=yes,bits=1
```
