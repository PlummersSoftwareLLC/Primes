# Pyret solution by rzuckerm

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

* `primes.arr` uses `Boolean` for each sieve item

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
* `-s` - Print found prime numbers
* `-h` - Show help

## Output

On an Intel(R) Core(TM) i7-8700 CPU @ 3.20GHz with 32 GB of memory on a Windows 10 desktop running
a Ubuntu 22.04 VM in VirtualBox 6.1:

```
```
