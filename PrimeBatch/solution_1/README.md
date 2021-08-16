# Batch solution by Sxxov

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)![Parallelism](https://img.shields.io/badge/Parallel-no-green)![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

A Windows Batch file implementation of the prime sieve.

It takes advantage of the worker model to imitate classes, spawning a new `cmd` window per pass. This also means multithreading is supported, but is disabled by default.

> Note: This implementation runs really *really* slowly & is thus skipped from the automated benchmark. Check the [output](#output) section for reference of speed, & check [run instructions](#run-instructions) for methods to run it manually.



## Run instructions

These instructions assumes that you're in the directory of `PrimeBatch/solution_1` & have all the dependencies installed as stated in `BENCHMARK.md`.

Both arguments `workers` & `sieveSize` can be omitted, they will default to the values shown in the examples below.

> Note: The examples & the defaults use the prime count size based on the rules. If you'd like to actually run these, it's probably better you decrease them. I found a sieve size of ~1000 gives the implementation a much better chance of going through more than 1 pass per 5 seconds. Example to run on Windows:
>
> ```
> main.bat /workers:%NUMBER_OF_PROCESSORS% /sieveSize:1000
> ```

### Natively on Windows

This uses `cmd.exe`, which is included by default in all versions of Windows.

```batch
main.bat /workers:1 /sieveSize:1000000
```

### Via Wine on Unix-based Systems

This uses `Wine`, which you may need to install. Refer [here](https://www.tecmint.com/install-wine-in-ubuntu/).

```bash
wine cmd /c main.bat /workers:1 /sieveSize:1000000
```

### Via Docker on Unix-based Systems

This uses `Docker`, which you should have already installed if coming from `BENCHMARK.md`.

```bash
docker build --build-arg workers=1 --build-arg sieveSize=1000000 -t primebatch . && docker run -d primebatch
```

## Output

This output was gotten from a sieve of 1,000,000 natively on Windows, without using Docker or Wine. 

```
elapsed: 10:31:55.85 (37915.85s total)

---
batch;1;37915.85;1;algorithm=base,faithful=yes,bits=unknown
```

As you can see, this is kind of a dumb amount of time. Full machine specifications for completeness:

* AMD Ryzen 9 4900HS @ 3.0GHz
* 16GB 3200MHz DDR4
* Windows 10 21H1