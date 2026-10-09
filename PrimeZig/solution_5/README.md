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

On a 4-core Intel Xeon at 2.4 GHz (KVM, 48 KiB L1 data per core), Zig 0.17.0
ReleaseFast. Three native 5-second runs had medians of about 27,009 passes/sec
(1 thread) and 109,002 passes/sec (4 threads):

```text
crishoj_wheel;135045;5.00003;1;algorithm=wheel,faithful=yes,bits=1
crishoj_wheel_mt;545507;5.00457;4;algorithm=wheel,faithful=yes,bits=1
```

On an Apple M5 Pro MacBook, native Zig 0.17.0 ReleaseFast:

```text
crishoj_wheel;201602;5.00002;1;algorithm=wheel,faithful=yes,bits=1
crishoj_wheel_mt;2657099;5.00601;18;algorithm=wheel,faithful=yes,bits=1
```

About 40,320 passes/sec on 1 thread and 530,800 passes/sec on 18 threads.
A second run gave `198917;5.00001;1` and `2637560;5.00878;18`.
