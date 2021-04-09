# Rust implementation

Contributors:

- Michael Barber @mike-barber https://www.github.com/mike-barber

There are two Rust ports:
- a very directly-translated version similar to the C++ version that is very easy to compare with it, called `simple-bits`.
- a slightly more Rust-idiomatic port called `abstracted` that has the storage of prime flags abstracted out, with the prime sieve algorithm generic over the storage. Two kinds of storage are implemented:
    - `bit storage` is equivalent to the above `simple-bits` case and uses individual bits within a byte to store true/false
    - `byte storage` has a simple vector of bytes, and just has `0 == false` and `1 == true`. It's a lot faster.
    - this port still intends to be similar to the C++ implementations so it's easier to compare; it's not fully idiomatic Rust, and is not intended to be. 

The motivation for the second port, supporting bytes/bools, is that many other PR's are doing the same thing. And Dave mentioned in the Pi vs Apple M1 vs ThreadRipper video that he'd probably be testing the same in C++. It's also a nice way to demo some simple traits and generics for those interested in Rust. And also fun because these kinds of abstractions don't cost us anything.

## Performance - Ryzen 3900X 

Tested on a Ryzen 3900X, Rust 1.50, running on WSL2.

### Results for the `abstracted` implementation

5 second run:
```
Byte storage:  Passes: 16887, Time: 5.000172, Avg: 0.00029609594, Limit: 1000000, Count: 78498, Valid: true
Bit storage:   Passes: 11125, Time: 5.000176, Avg: 0.000449454, Limit: 1000000, Count: 78498, Valid: true
```

10 second run:
```
Byte storage:  Passes: 33705, Time: 10.000176, Avg: 0.0002966971, Limit: 1000000, Count: 78498, Valid: true
Bit storage:   Passes: 22351, Time: 10.000236, Avg: 0.0004474178, Limit: 1000000, Count: 78498, Valid: true
```

### Results for the `simple-bits` implementation 

5 second run: 
```
Passes: 10229, Time: 5.0004015, Avg: 0.0004888456, Limit: 1000000, Count: 78498, Valid: true
```

10 second run:
```
Passes: 20476, Time: 10.000134, Avg: 0.00048838323, Limit: 1000000, Count: 78498, Valid: true
```

## Performance - Raspberri Pi 4

Tested on a Raspberry Pi 4, standard clock with active cooling, running 64-bit Ubuntu Server 20.04. 

5 second run:
```
Byte storage:  Passes: 2128, Time: 5.001707, Avg: 0.0023504263, Limit: 1000000, Count: 78498, Valid: true
Bit storage:   Passes: 2097, Time: 5.001482, Avg: 0.0023850654, Limit: 1000000, Count: 78498, Valid: true
```

10 second run:
```
Byte storage:  Passes: 4136, Time: 10.000821, Avg: 0.0024179935, Limit: 1000000, Count: 78498, Valid: true
Bit storage:   Passes: 4164, Time: 10.000468, Avg: 0.0024016495, Limit: 1000000, Count: 78498, Valid: true
```

### Configuration

I've enabled all the optimisation I'm aware of, including:
- setting `target-cpu=native`
- link time optimisation and `codegen-units=1`
- pay attention to [.cargo/config](.cargo/config) if you want to run it on, say, Mac. 

## Quick start for non-Rust people

Install Rust. It's really easy: https://www.rust-lang.org/learn/get-started. On Linux, it's just `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`; probably the same or similar on Mac. On Windows, just grab the installer from the link.

There is a convenient [run.sh](run.sh) file.

Alternately, in the directory containing this README file, 
- run tests: `cargo test` 
- run the abstracted version: `cargo run --release --bin abstracted`
- run the simple-bits version: `cargo run --release --bin simple-bits`

It'll take a little while to compile the first time.

To play with the code, the simplest approach is to use *Visual Studio Code* and install the `rust-analyzer` plugin.
