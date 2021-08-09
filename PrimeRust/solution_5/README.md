# Rust solution by @Kulasko

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

Contributors:
- Kai Rese @Kulasko https://git.h3n.eu/Kulasko

This solution aims to be an extension of `solution_01` by Michael Barker by multiple multithreaded algorithms. Contrary to most approaches that just run independent sieves in parallel, this solution provides algorithms that work on the same sieve. It really is a shame there is no differentiation for this competition.

Currently, there are two algorithms:
- Streaming. After finding a new prime number, all threads work together on a flag unset pass. This takes no advantage of data locality whatsoever, also threads can steal memory from each other's caches between passes, so it should be bottlenecked by cache bandwidth and crosstalk latency. Also, this isn't expected to scale well in any way.
- Tiled. After a single thread fetches all the primes up to the square root of the total number, a number of threads receive the list of primes and each apply them on their part of the sieve. This has very good data locality, but as threads don't move to where the action happens, they are bound to run dry. If there are no other bottlenecks, this should approach around 50% of CPU core scaling.

Each algorithm is run on each combination of flag handling and internal data primitive that makes sense to test. Others (such as boolean with u32 elements) are tested, but not run.

## Run instructions

- With no local Rust installation, the Docker scripts can be used:
    - `./build-docker.sh`
    - `./run-docker.sh`
- With a local Rust installation, `cargo run --release` does the trick.

The `set-size` argument sets the working set size for the tiling algorithm in kibibytes.
You should set it to the amount of cache each of your threads has, preferably to the L1 data cache size.
16 kB is the optimal size for most processors, so if you don't specify a size, it defaults to that value.
There are some noteworthy exceptions:
- Modern x86 CPUs without SMT, such as the Ryzen 7 4700U, for example. They can use the full 32 kB of the core.
- Raspberry Pi 4 (32 kB)
- Intel processors with Ice Lake or newer architecture (for example the 11900k, 24 kB with or 48 kB without SMT)
- Apple M1 (64 kB for the smaller cores, 128 kB for the big ones, I couldn't test which the best setting is since I don't own Apple hardware)
You can just supply the argument to the docker script. For cargo, you have to tell it to treat the argument as a program argument like so:
`cargo run --release -- --set-size 16`

This solution features extensive documentation (WIP). To take a look at a compiled version, simply run `cargo doc --document-private-items --open`.

## Output

This is the output on `stdout` from my old trusty (slow) AMD FX 8350 running at 4.0 Ghz on Manjaro and Rust 1.53:

```
kulasko-rust-serial-bool-u8;3551;5.000967992;1;algorithm=base,faithful=yes,bits=8
kulasko-rust-serial-bit-u32;4385;5.000471221;1;algorithm=base,faithful=yes,bits=1
kulasko-rust-stream-bool-u8;901;5.000556986;8;algorithm=base,faithful=yes,bits=8
kulasko-rust-stream-bit-u8;1180;5.002886401;8;algorithm=base,faithful=yes,bits=1
kulasko-rust-stream-bit-u32;1167;5.004061427;8;algorithm=base,faithful=yes,bits=1
kulasko-rust-tile-bool-u8;7761;5.000411964;8;algorithm=base,faithful=yes,bits=8
kulasko-rust-tile-bit-u8;14717;5.000355348;8;algorithm=base,faithful=yes,bits=1
kulasko-rust-tile-bit-u32;14439;5.000358052;8;algorithm=base,faithful=yes,bits=1
```
