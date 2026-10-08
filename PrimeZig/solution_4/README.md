# Zig solution by crishoj

![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Faithful 210-wheel sieve. Each pass allocates a fresh buffer with one bit per
integer coprime to 210, so the working set for a limit of 1,000,000 is about
38 KiB and stays in L1. Multiples of 11 through 112 are applied as repeating
word masks. Larger factors are struck with strided single-bit ORs. A second
line runs one independent sieve per CPU.

## Run instructions

Zig 0.17:

```sh
zig build -Doptimize=ReleaseFast
./zig-out/bin/prime-zig
```

Or Docker:

```sh
docker build -t prime-zig .
docker run --rm prime-zig
```

The program checks the sieve against a reference count before timing. Each
reported line then runs for five seconds. `SIEVE_SECONDS` can shorten a local
timing run; the default is 5.

## Output

On a 4-core Intel Xeon at 2.4 GHz (KVM, 48 KiB L1 data per core), Zig 0.17.0,
ReleaseFast:

```text
crishoj_wheel;133432;5.00000;1;algorithm=wheel,faithful=yes,bits=1
crishoj_wheel_mt;529563;5.00072;4;algorithm=wheel,faithful=yes,bits=1
```

Three native runs on that machine had medians of about 26,686 passes/sec
(1 thread) and 105,897 passes/sec (4 threads). The Docker image printed:

```text
crishoj_wheel;135279;5.00004;1;algorithm=wheel,faithful=yes,bits=1
crishoj_wheel_mt;540795;5.00096;4;algorithm=wheel,faithful=yes,bits=1
```
