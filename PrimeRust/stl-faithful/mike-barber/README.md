# Rust implementation

Contributors:

- Michael Barber @mike-barber https://www.github.com/mike-barber

There are two Rust ports:
- a very directly-translated version similar to the C++ version that is very easy to compare with it, called `simple-bits`.
- a slightly more Rust-idiomatic port called `abstracted` that has the storage of prime flags abstracted out, with the prime sieve algorithm generic over the storage. Two kinds of storage are implemented:
    - `bit storage` is equivalent to the above `simple-bits` case and uses individual bits within a byte to store true/false
    - `byte storage` has a simple vector of bytes, and just has `0 == false` and `1 == true`. It's a lot faster.
    - this port still intends to be similar to the C++ implementations so it's easier to compare; it's not fully idiomatic Rust, and is not intended to be.
    - this version also contains a parallel implementation, similar to the newer C++ version Dave's been working on
    - it runs both single-thread and multi-thread tests.

The motivation for the second port, supporting bytes/bools, is that many other PR's are doing the same thing. And Dave mentioned in the Pi vs Apple M1 vs ThreadRipper video that he'd probably be testing the same in C++. It's also a nice way to demo some simple traits and generics for those interested in Rust. And also fun because these kinds of abstractions don't cost us anything.


## Performance - **Ryzen 3900X**

Tested on a Ryzen 3900X, Rust 1.51, running on WSL2.

### Results for the `abstracted` implementation

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

### Results for the `simple-bits` implementation 

5 second run: 
```
Passes: 10285, Time: 5.0003653, Avg: 0.0004861804, Limit: 1000000, Count: 78498, Valid: true
Passes: 10306, Time: 5.00034, Avg: 0.00048518728, Limit: 1000000, Count: 78498, Valid: true
Passes: 10260, Time: 5.0000286, Avg: 0.00048733223, Limit: 1000000, Count: 78498, Valid: true
```

## Performance - **Raspberri Pi 4**

Tested on a **Raspberry Pi 4**, standard clock with active cooling, running 64-bit Ubuntu Server 20.04. 

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

# Quick start for non-Rust people

Install Rust. It's really easy: https://www.rust-lang.org/learn/get-started. On Linux, it's just `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`; probably the same or similar on Mac. On Windows, just grab the installer from the link.

There is a convenient [run.sh](run.sh) file that runs both variants with defaults.

Alternately, in the directory containing this README file, 
- run tests: `cargo test` 
- build: `cargo build --release`
- run the abstracted version: `target/release/abstracted`
    - for help with command line parameters: `target/release/abstracted --help`
    - this allows you to specify sieve size, threads, etc.
- run the simple-bits version: `target/release/simple-bits`

It'll take a little while to compile the first time.

To play with the code, the simplest approach is to use *Visual Studio Code* and install the `rust-analyzer` plugin.

## Docker

You can also run the solution using Docker, without installing Rust. This will run the `abstracted` variant.

```
./build-docker.sh
./run-docker.sh
```

For available command line options, just do `./run-docker.sh --help`
