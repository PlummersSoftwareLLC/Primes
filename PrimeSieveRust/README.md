# WHAT IS IT?
Primes based on the code from Dave Plummer Primes repository.

A Rust copy of the algorithm presented in PrimeGo.

# PERFORMANCE

AMD Ryzen Threadripper 3970X 3.7GHz

```
Passes: 5718, Time: 10.000857, Avg: 0.0017490132, Limit: 1000000, Count: 78498, Valid: true
```

# HOW TO RUN

[Install Rust 1.51.0](https://www.rust-lang.org/tools/install)

```bash 
$ cargo run --release
```

# HOW TO TEST

```bash
$ cargo test
```

```
running 8 tests
test prime_sieve::test::under_one_hundred_yields_valid_results ... ok
test prime_sieve::test::under_one_thousand_yields_valid_results ... ok
test prime_sieve::test::under_ten_yields_valid_results ... ok
test prime_sieve::test::under_ten_thousand_yields_valid_results ... ok
test prime_sieve::test::under_one_hundred_thousand_yields_valid_results ... ok
test prime_sieve::test::under_one_million_yields_valid_results ... ok
test prime_sieve::test::under_ten_million_yields_valid_results ... ok
test prime_sieve::test::under_one_hundred_million_yields_valid_results ... ok

test result: ok. 8 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 14.43s
```