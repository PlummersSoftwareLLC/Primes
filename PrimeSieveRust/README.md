# Rust implementation

A not-very-idiomatic port of the Dave's C++ version to Rust. There are even a couple of `unsafe` methods, but they don't make a massive difference to performance, and could be removed for a slight penalty.

Rust performs reasonably well in this case, on my machine (AMD Ryzen 3900X).
- `Windows C++:  Passes: 15391, Time: 10.000000, Avg: 0.000650, Limit: 1000000, Count: 78498, Valid: 1`
- `Linux clang:  Passes: 15687, Time: 10.000000, Avg: 0.000637, Limit: 1000000, Count: 78498, Valid: 1`
- `Windows Rust: Passes: 20542, Time: 10.000122, Avg: 0.00048681346, Limit: 1000000, Count: 78498, Valid: true`
- `Linux Rust:   Passes: 21170, Time: 10.000431, Avg: 0.00047238692, Limit: 1000000, Count: 78498, Valid: true`

I've enabled all the optimisation I'm aware of, including:
- setting `target-cpu=native`
- link time optimisation and `codegen-units=1`

## Quick start for non-Rust people

Install Rust -- it's really easy: https://www.rust-lang.org/learn/get-started

Then in the `PrimeSieveRust` directory, 

- test: `cargo test` 
- run: `cargo run --release`

For some disassembly fun

```
cargo install cargo-asm
cargo asm prime_sieve_rust::main
```