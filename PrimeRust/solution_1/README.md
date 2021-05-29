# Rust solution by @mike-barber

![Category](https://img.shields.io/badge/Category-faithful-green)
![Category](https://img.shields.io/badge/Category-unfaithful-yellowgreen)

Contributors:
- Michael Barber @mike-barber https://www.github.com/mike-barber

This is a somewhat Rust-idiomatic version that has the storage of prime flags abstracted out, with the prime sieve algorithm generic over the storage. Two kinds of storage are implemented:
    
- `bit storage` is equivalent to the above C++ `vector<bool>` case and uses individual bits within a byte to store true/false
- `byte storage` has a simple vector of bytes, and just has `0 == false` and `1 == true`. It's a lot faster. But only for small datasets, since it uses more memory.
- this version still intends to be similar to the C++ implementations so it's easier to compare; it's not fully idiomatic Rust, and is not intended to be.
- it runs both single-thread and multi-thread tests.

Note that C++'s `vector<bool>` is a controversial specialisation of `vector` that `*may*` use more efficient storage than using a whole byte for 1 or 0. It's up to the compiler vendor, and is not standardised. Typically it's using individual bits for storage, within bytes or words. So the `bit storage` I have implemented closely resembles this. Rust has no such thing built in, although there are several crates available. For clarity, it's implemented manually in this solution.

## Run instructions

- using Docker is the easiest way to get started without installing Rust:
    - `./build-docker.sh`
    - `./run-docker.sh`
    - `./run-docker.sh --help` for help with command line options
- `cargo run --release` if you've got Rust installed

There are more notes for getting started with Rust at the bottom, under `Quick start for those interested in Rust`

## Output

Tested on a Ryzen 3900X, Rust 1.51, running on WSL2.

This is as reported on `stdout`:
```
mike-barber_byte-storage;18177;5.0000672340;1
mike-barber_bit-storage;11954;5.0003018379;1
mike-barber_byte-storage;169690;5.0010256767;24
mike-barber_bit-storage;151787;5.0007596016;24
```

We report more informative metrics to `stderr` too, but these don't go into the report, as recorded below.

## Performance - **Ryzen 3900X**

Tested on a Ryzen 3900X, Rust 1.51, running on WSL2.
- Examples are run with `cargo run --release -- --repetitions=3` to get 3 repetitions.

```
Computing primes to 1000000 on 1 thread for 5 seconds.
byte-storage    Passes: 18194, Threads: 1, Time: 5.0000767708, Average: 0.0002748201, Limit: 1000000, Counts: 78498, Valid: Pass
byte-storage    Passes: 18175, Threads: 1, Time: 5.0002570152, Average: 0.0002751173, Limit: 1000000, Counts: 78498, Valid: Pass
byte-storage    Passes: 18050, Threads: 1, Time: 5.0000729561, Average: 0.0002770124, Limit: 1000000, Counts: 78498, Valid: Pass

Computing primes to 1000000 on 1 thread for 5 seconds.
bit-storage     Passes: 11917, Threads: 1, Time: 5.0003094673, Average: 0.0004195946, Limit: 1000000, Counts: 78498, Valid: Pass
bit-storage     Passes: 11941, Threads: 1, Time: 5.0002613068, Average: 0.0004187473, Limit: 1000000, Counts: 78498, Valid: Pass
bit-storage     Passes: 11961, Threads: 1, Time: 5.0001654625, Average: 0.0004180391, Limit: 1000000, Counts: 78498, Valid: Pass

Computing primes to 1000000 on 24 threads for 5 seconds.
byte-storage    Passes: 169821, Threads: 24, Time: 5.0006589890, Average: 0.0000294466, Limit: 1000000, Counts: 78498, Valid: Pass
byte-storage    Passes: 169607, Threads: 24, Time: 5.0007286072, Average: 0.0000294842, Limit: 1000000, Counts: 78498, Valid: Pass
byte-storage    Passes: 169333, Threads: 24, Time: 5.0009894371, Average: 0.0000295335, Limit: 1000000, Counts: 78498, Valid: Pass

Computing primes to 1000000 on 24 threads for 5 seconds.
bit-storage     Passes: 151804, Threads: 24, Time: 5.0008311272, Average: 0.0000329427, Limit: 1000000, Counts: 78498, Valid: Pass
bit-storage     Passes: 151710, Threads: 24, Time: 5.0007929802, Average: 0.0000329628, Limit: 1000000, Counts: 78498, Valid: Pass
bit-storage     Passes: 151644, Threads: 24, Time: 5.0008230209, Average: 0.0000329774, Limit: 1000000, Counts: 78498, Valid: Pass
```

## Performance - **Raspberri Pi 4**

Tested on a **Raspberry Pi 4**, standard clock with active cooling, running 64-bit Ubuntu Server 20.04.
- Examples are run with `cargo run --release -- --repetitions=3` to get 3 repetitions.
- Docker works on the Pi too :)

```
Computing primes to 1000000 on 1 thread for 5 seconds.
byte-storage    Passes: 2325, Threads: 1, Time: 5.0005192757, Average: 0.0021507610, Limit: 1000000, Counts: 78498, Valid: Pass
byte-storage    Passes: 2332, Threads: 1, Time: 5.0009851456, Average: 0.0021445048, Limit: 1000000, Counts: 78498, Valid: Pass
byte-storage    Passes: 2334, Threads: 1, Time: 5.0021271706, Average: 0.0021431565, Limit: 1000000, Counts: 78498, Valid: Pass

Computing primes to 1000000 on 1 thread for 5 seconds.
bit-storage     Passes: 2434, Threads: 1, Time: 5.0012273788, Average: 0.0020547360, Limit: 1000000, Counts: 78498, Valid: Pass
bit-storage     Passes: 2433, Threads: 1, Time: 5.0001516342, Average: 0.0020551383, Limit: 1000000, Counts: 78498, Valid: Pass
bit-storage     Passes: 2434, Threads: 1, Time: 5.0013856888, Average: 0.0020548010, Limit: 1000000, Counts: 78498, Valid: Pass

Computing primes to 1000000 on 4 threads for 5 seconds.
byte-storage    Passes: 393, Threads: 4, Time: 5.0251092911, Average: 0.0127865374, Limit: 1000000, Counts: 78498, Valid: Pass
byte-storage    Passes: 398, Threads: 4, Time: 5.0315208435, Average: 0.0126420120, Limit: 1000000, Counts: 78498, Valid: Pass
byte-storage    Passes: 397, Threads: 4, Time: 5.0280776024, Average: 0.0126651833, Limit: 1000000, Counts: 78498, Valid: Pass

Computing primes to 1000000 on 4 threads for 5 seconds.
bit-storage     Passes: 8083, Threads: 4, Time: 5.0011849403, Average: 0.0006187288, Limit: 1000000, Counts: 78498, Valid: Pass
bit-storage     Passes: 8062, Threads: 4, Time: 5.0017576218, Average: 0.0006204115, Limit: 1000000, Counts: 78498, Valid: Pass
bit-storage     Passes: 8358, Threads: 4, Time: 5.0018110275, Average: 0.0005984459, Limit: 1000000, Counts: 78498, Valid: Pass
```

Some interesting things to note here. The little Pi only has 1MB of cache, total. We're blowing that cache immediately using `byte` level storage when running on multiple threads: it's just thrashing the cache with a large dataset. However, the `bit` level storage is more efficient and data for all threads fits in the cache in this case and we get a decent performance uplift.

I'm expecting the Apple M1 to do a lot better given it has 16MB of cache, but I can't test this. Fundamentally, you'll see the same thing on any processor once the dataset doesn't fit inside the cache, essentially where `sieve_size * threads > cache`. So for small datasets, `byte` level storage may be faster given there's less work to do. For larger datasets, you need the memory efficiency instead.

# Configuration

I've enabled all the optimisation I'm aware of, including:
- setting `target-cpu=native`
- link time optimisation and `codegen-units=1`
- pay attention to [.cargo/config](.cargo/config) if you want to run it on, say, Mac. I don't have one.

# Quick start for those interested in Rust

Install Rust. It's really easy: https://www.rust-lang.org/learn/get-started. On Linux, it's just `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`; probably the same or similar on Mac. On Windows, just grab the installer from the link.

There is a convenient [run.sh](run.sh) file that builds and runs the solution.

Alternately, in the directory containing this README file, 
- run tests: `cargo test` 
- build: `cargo build --release`
- run: `target/release/prime-sieve-rust` or `cargo run --release`
    - for help with command line parameters: `target/release/prime-sieve-rust --help`
    - this allows you to specify sieve size, threads, etc.

It'll take a little while to compile the first time. Because Rust. It's doing a fair bit of work :)

To play with the code, the simplest approach is to use *Visual Studio Code* and install the `rust-analyzer` plugin.

And if you want to learn more, there are tons of great resources, including a good introductory book, on https://www.rust-lang.org/learn/get-started

## Docker

You can also run the solution using Docker, without installing Rust.

```
./build-docker.sh
./run-docker.sh
```

For available command line options, just do `./run-docker.sh --help`
