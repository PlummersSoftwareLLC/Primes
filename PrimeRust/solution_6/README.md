# Rust solution by Sycration

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This implementation is a port of the original C++ implementation but it uses rust's const generics and const functions to run the sieve at compile time. This is unfair and does not set the size of the array at runtime, so it is unfaithful.

## Run instructions

1. Install rust nightly
2. run `cargo +nightly run --release`

## Output

```
Passes: 139108, Time: 5.0000218, Avg: 0.00003594345256922679, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Sycration;139108;5.0000218;1;algorithm=base,faithful=no
```

on an i7-9700k running Windows 11
