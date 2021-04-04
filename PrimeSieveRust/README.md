# Rust implementation

There are two Rust ports:
- a very directly-translated version similar to the C++ version that is very easy to compare with it, called `simple-bits`.
- a slightly more Rust-idiomatic port called `abstracted` that has the storage of prime flags abstracted out, with the prime sieve algorithm generic over the storage. Two kinds of storage are implemented:
    - `bit storage` is equivalent to the above `simple-bits` case and uses individual bits within a byte to store true/false
    - `byte storage` has a simple vector of bytes, and just has `0 == false` and `1 == true`. It's a lot faster.

The motivation for the second port, supporting bytes/bools, is that many other PR's are doing the same thing. And Dave mentioned in the Pi vs Apple M1 vs ThreadRipper video that he'd probably be testing the same in C++. It's also a nice way to demo some simple traits and generics for those interested in Rust. And also fun because these kinds of abstractions don't cost us anything.

## Performance - Ryzen 3900X 

Tested on a Ryzen 3900X, Rust 1.50, running on WSL2.

### Results for the `abstracted` implementation

```
Bit storage:   Passes: 22392, Time: 10.000229, Avg: 0.00044659828, Limit: 1000000, Count: 78498, Valid: true
Byte storage:  Passes: 33980, Time: 10.000248, Avg: 0.00029429805, Limit: 1000000, Count: 78498, Valid: true
```

### Results for the `simple-bits` implementation 

```
Windows C++:  Passes: 15391, Time: 10.000000, Avg: 0.000650, Limit: 1000000, Count: 78498, Valid: 1
Linux clang:  Passes: 15687, Time: 10.000000, Avg: 0.000637, Limit: 1000000, Count: 78498, Valid: 1
Windows Rust: Passes: 20542, Time: 10.000122, Avg: 0.00048681346, Limit: 1000000, Count: 78498, Valid: true
Linux Rust:   Passes: 21170, Time: 10.000431, Avg: 0.00047238692, Limit: 1000000, Count: 78498, Valid: true
```

## Performance - Raspberri Pi 4

Tested on a Raspberry Pi 4, mild overclock with active cooling, running 64-bit Ubuntu Server 20.04. 

```
Bit storage:   Passes: 4862, Time: 10.000785, Avg: 0.0020569281, Limit: 1000000, Count: 78498, Valid: true
Byte storage:  Passes: 5041, Time: 10.001149, Avg: 0.0019839613, Limit: 1000000, Count: 78498, Valid: true
```
This is quite a bit faster than Dave's C++ results for the Pi, so I reckon there's some more performance we can squeeze out C++ on the Pi (and the M1 too probably). 

### Configuration

I've enabled all the optimisation I'm aware of, including:
- setting `target-cpu=native`
- link time optimisation and `codegen-units=1`
- pay attention to [.cargo/config](.cargo/config) if you want to run it on, say, Mac. 

## Quick start for non-Rust people

Install Rust. It's really easy: https://www.rust-lang.org/learn/get-started. On Linux, it's just `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`; probably the same or similar on Mac. On Windows, just grab the installer from the link.

Then in the `PrimeSieveRust` directory, 

- run tests: `cargo test` 
- run the abstracted version: `cargo run --release --bin abstracted`
- run the simple-bits version: `cargo run --release --bin simple-bits`

It'll take a little while to compile the first time.

To play with the code, the simplest approach is to use *Visual Studio Code* and install the `rust-analyzer` plugin.