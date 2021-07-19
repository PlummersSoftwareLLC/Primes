# Rust solution by @Kulasko

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

Contributors:
- Kai Rese @Kulasko https://git.h3n.eu/Kulasko

This solution aims to explore the effects of different multithreading approaches to an optimized version of the C++ algorithm, inspired by the solution of Mike Barber (solution_1). Currently, there are three algorithms:

- Stock standard single threaded. Mostly useful for a baseline compared to the others.
- Streaming. After finding a new prime number, all threads work together on a flag unset pass. This takes no advantage of data locality whatsoever, also threads can steal memory from each other's caches between passes, so it should be bottlenecked by cache bandwidth and crosstalk latency. Also, this isn't expected to scale well in any way.
- Tiled. After a single thread fetches all the primes before the square root of the total number, a number of memory blocks receive the list of primes and each apply them on their memory. This has very good data locality, but as thread don't move to where the action happens, they are bound to run dry. If there are no other bottlenecks, this should approach around 50% of CPU core scaling.

Each algorithm is run on a bit-based and a bool-based data set.

## Run instructions

- With no local Rust installation, the Docker scripts can be used:
    - `./build-docker.sh`
    - `./run-docker.sh --set-size 16`
- With a local Rust installation, `cargo run --release -- --set-size 16` does the trick.

The `set-size` argument sets the working set size for the tiling algorithm in kibibytes.
You should set it to the amount of cache each of your threads has, preferably to the L1 data cache size.
16 kB is the optimal size for most processors, so if you don't specify a size, it defaults to that value.
There are some noteworthy exceptions:
- Modern x86 CPUs without SMT, such as the Ryzen 7 4700U, for example. They can use the full 32 kB of the core.
- Raspberry Pi 4 (32 kB)
- Intel processors with Ice Lake or newer architecture (for example the 11900k, 24 kB or 48 kB without SMT)
- Apple M1 (64 kB for the smaller cores, 128 kB for the big ones, I couldn't test which the best setting is since I don't own Apple hardware)

## Output

This is the output on `stdout` from my old trusty (slow) AMD FX 8350 running at 4.0 Ghz on Manjaro and Rust 1.53:

```
kulasko-rust-serial-bit;4094;5.000878526;1;algorithm=base,faithful=yes,bits=1
kulasko-rust-serial-bool;4191;5.000533138;1;algorithm=base,faithful=yes,bits=8
kulasko-rust-streamed-bit;1249;5.000794346;8;algorithm=base,faithful=yes,bits=1
kulasko-rust-streamed-bool;1041;5.003056427;8;algorithm=base,faithful=yes,bits=8
kulasko-rust-tiled-bit;16146;5.000183582;8;algorithm=base,faithful=yes,bits=1
kulasko-rust-tiled-bool;2945;5.000977906;8;algorithm=base,faithful=yes,bits=8
```
