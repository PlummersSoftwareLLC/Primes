# Rust solution(s) by Sycration

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This implementation is a port of the original C++ implementation but it uses rust's const generics and const functions to run the sieve at compile time. This is unfair and does not set the size of the array at runtime, so it is unfaithful. One runs on one core, the other on 8. 

## Run instructions

1. Install rust nightly
2. run `rustc +nightly src/singlethreaded.rs -Copt-level=z -C target-cpu=native`
3. run `rustc +nightly src/multithreaded.rs -Copt-level=z -C target-cpu=native`
4. run `run-by-docker.sh`

## Output

```
running on one core
Passes: 180893, Time: 5.0000133, Avg: 0.000027640722968826874, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
SycrationSinglethreaded;180893;5.0000133;1;algorithm=base,faithful=no

running on eight cores
Passes: 903785, Time: 5.0000348, Avg: 0.0000055323277106834034, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
SycrationMultithreaded;903785;5.0000348;8;algorithm=base,faithful=no
```

on an i7-9700k running Windows 11

At least on my computer, it thoroughly beats the similar C++ constexpr implementation. I don't know why.
(`flo80_constexpr;118136;5.000162;8;algorithm=base,faithful=no,bits=1`)
