# Rust solution by CokieMiner

Wheel-30 Sieve of Eratosthenes with one bit per candidate and zero dependencies.
Each pass creates a new `PrimeSieve`, allocates its bitmap, and generates seed
patterns at runtime. At the standard limit, seeds cover primes through 89;
remaining factors are found in the sieve. Parallel workers use `std::thread`
and compute independent complete sieves.

## Run instructions

From this directory, using Docker:

```sh
docker build -t cokieminer-sieve .
docker run --rm cokieminer-sieve
docker run --rm cokieminer-sieve --threads auto
```

Or build locally with Rust 1.99 or newer:

```sh
cargo build --release --offline --locked
./target/release/cokieminer-sieve
./target/release/cokieminer-sieve --threads auto
```

The default is one worker, limit **1,000,000**, and at least **5 seconds**.
`--threads auto` uses the available CPU count; `--threads N` sets a worker budget.
Each worker's final sieve is validated after timing against **78,498 primes**.
Run `--help` for options, or `--check --limit 1000000` for one untimed count.
For a local CPU-specific build, set `RUSTFLAGS="-C target-cpu=native"` on the
Cargo build command.

## Output

Docker runs (default and `--threads auto`) on Linux, AMD Ryzen AI 7 350, Rust 1.99:

```text
CokieMiner_rust_w30;199391;5.000019767;1;algorithm=wheel,faithful=yes,bits=1
CokieMiner_rust_w30;1359830;5.000430594;16;algorithm=wheel,faithful=yes,bits=1
```

The race record goes to stdout; diagnostics and validation go to stderr.
Tags follow the [Primes contribution guidelines](https://github.com/PlummersSoftwareLLC/Primes/blob/drag-race/CONTRIBUTING.md).
Licensed under [MIT](LICENSE).
