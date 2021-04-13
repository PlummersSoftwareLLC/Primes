# Rust solution by @mike-barber

`# category badges will go here`

Contributors:
- Michael Barber @mike-barber https://www.github.com/mike-barber

A somewhat Rust-idiomatic version that has the storage of prime flags abstracted out, with the prime sieve algorithm generic over the storage. Two kinds of storage are implemented:
    - `bit storage` is equivalent to the above C++ `vector<bool>` case and uses individual bits within a byte to store true/false
    - `byte storage` has a simple vector of bytes, and just has `0 == false` and `1 == true`. It's a lot faster. But only for small datasets, since it uses more memory.
    - this version still intends to be similar to the C++ implementations so it's easier to compare; it's not fully idiomatic Rust, and is not intended to be. 
    - it runs both single-thread and multi-thread tests.

Note that `vector<bool>` in C++ is a strange specialisation of `vector` that **may** use more efficient storage than using a whole byte for 1 or 0. It's up to the compiler vendor, and is not standardised. Typically it's using individual bits for storage, within bytes or words. So the `bit storage` I have implemented closely resembles this. Rust has no such thing built in, although there are several crates available.

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
byte-storage;16849;5.0003237724;1
bit-storage;11171;5.0002226830;1
byte-storage;157714;5.0008201599;24
bit-storage;126534;5.0010280609;24
```

We report more informative metrics to `stderr` too, but these don't go into the report, as recorded below.

## Performance - **Ryzen 3900X**

Tested on a Ryzen 3900X, Rust 1.51, running on WSL2.
- Examples are run with `cargo run --release -- --repetitions=3` to get 3 repetitions.

```
Computing primes to 1000000 on 1 thread for 5 seconds.
Byte storage    Passes: 17015, Threads: 1, Time: 5.0002589226, Average: 0.0002938736, Limit: 1000000, Counts: 78498, Valid: Pass
Byte storage    Passes: 16997, Threads: 1, Time: 5.0003304482, Average: 0.0002941890, Limit: 1000000, Counts: 78498, Valid: Pass
Byte storage    Passes: 16975, Threads: 1, Time: 5.0001964569, Average: 0.0002945624, Limit: 1000000, Counts: 78498, Valid: Pass

Computing primes to 1000000 on 1 thread for 5 seconds.
Bit storage     Passes: 11220, Threads: 1, Time: 5.0004730225, Average: 0.0004456750, Limit: 1000000, Counts: 78498, Valid: Pass
Bit storage     Passes: 11222, Threads: 1, Time: 5.0004014969, Average: 0.0004455892, Limit: 1000000, Counts: 78498, Valid: Pass
Bit storage     Passes: 11223, Threads: 1, Time: 5.0003871918, Average: 0.0004455482, Limit: 1000000, Counts: 78498, Valid: Pass

Computing primes to 1000000 on 24 threads for 5 seconds.
Byte storage    Passes: 157425, Threads: 24, Time: 5.0010108948, Average: 0.0000317676, Limit: 1000000, Counts: 78498, Valid: Pass
Byte storage    Passes: 157288, Threads: 24, Time: 5.0008053780, Average: 0.0000317939, Limit: 1000000, Counts: 78498, Valid: Pass
Byte storage    Passes: 157691, Threads: 24, Time: 5.0009226799, Average: 0.0000317134, Limit: 1000000, Counts: 78498, Valid: Pass

Computing primes to 1000000 on 24 threads for 5 seconds.
Bit storage     Passes: 141430, Threads: 24, Time: 5.0008363724, Average: 0.0000353591, Limit: 1000000, Counts: 78498, Valid: Pass
Bit storage     Passes: 141365, Threads: 24, Time: 5.0008583069, Average: 0.0000353755, Limit: 1000000, Counts: 78498, Valid: Pass
Bit storage     Passes: 141206, Threads: 24, Time: 5.0008640289, Average: 0.0000354154, Limit: 1000000, Counts: 78498, Valid: Pass
```

## Performance - **Raspberri Pi 4**

Tested on a **Raspberry Pi 4**, standard clock with active cooling, running 64-bit Ubuntu Server 20.04.
- Examples are run with `cargo run --release -- --repetitions=3` to get 3 repetitions.
- Docker works on the Pi too :)

```
Computing primes to 1000000 on 1 thread for 5 seconds.
Byte storage    Passes: 2030, Threads: 1, Time: 5.0023941994, Average: 0.0024642337, Limit: 1000000, Counts: 78498, Valid: Pass
Byte storage    Passes: 2026, Threads: 1, Time: 5.0019612312, Average: 0.0024688852, Limit: 1000000, Counts: 78498, Valid: Pass
Byte storage    Passes: 2036, Threads: 1, Time: 5.0005850792, Average: 0.0024560830, Limit: 1000000, Counts: 78498, Valid: Pass

Computing primes to 1000000 on 1 thread for 5 seconds.
Bit storage     Passes: 2234, Threads: 1, Time: 5.0007052422, Average: 0.0022384536, Limit: 1000000, Counts: 78498, Valid: Pass
Bit storage     Passes: 2230, Threads: 1, Time: 5.0005149841, Average: 0.0022423833, Limit: 1000000, Counts: 78498, Valid: Pass
Bit storage     Passes: 2232, Threads: 1, Time: 5.0008111000, Average: 0.0022405067, Limit: 1000000, Counts: 78498, Valid: Pass

Computing primes to 1000000 on 4 threads for 5 seconds.
Byte storage    Passes: 343, Threads: 4, Time: 5.0172901154, Average: 0.0146276681, Limit: 1000000, Counts: 78498, Valid: Pass
Byte storage    Passes: 341, Threads: 4, Time: 5.0041513443, Average: 0.0146749308, Limit: 1000000, Counts: 78498, Valid: Pass
Byte storage    Passes: 345, Threads: 4, Time: 5.0423941612, Average: 0.0146156354, Limit: 1000000, Counts: 78498, Valid: Pass

Computing primes to 1000000 on 4 threads for 5 seconds.
Bit storage     Passes: 6923, Threads: 4, Time: 5.0020055771, Average: 0.0007225199, Limit: 1000000, Counts: 78498, Valid: Pass
Bit storage     Passes: 6921, Threads: 4, Time: 5.0014953613, Average: 0.0007226550, Limit: 1000000, Counts: 78498, Valid: Pass
Bit storage     Passes: 6806, Threads: 4, Time: 5.0029740334, Average: 0.0007350829, Limit: 1000000, Counts: 78498, Valid: Pass
```

Some interesting things to note here. The little Pi only has 1MB of cache, total. We're blowing that cache immediately using `byte` level storage when running on multiple threads: it's just thrashing the cache with a large dataset. However, the `bit` level storage is more efficient and data for all threads fits in the cache in this case and we get a decent performance uplift.

I'm expecting the Apple M1 to do a lot better given it has 16MB of cache, but I can't test this. Fundamentally, you'll see the same thing on any processor once the dataset doesn't fit inside the cache. So for small datasets, `byte` level storage will be faster given there's less work to do. For larger datasets, you need the memory efficiency instead.

# Configuration

I've enabled all the optimisation I'm aware of, including:
- setting `target-cpu=native`
- link time optimisation and `codegen-units=1`
- pay attention to [.cargo/config](.cargo/config) if you want to run it on, say, Mac. 

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
