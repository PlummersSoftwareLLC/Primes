# WHAT IS IT?
Primes based on the code from Dave Plummer Primes repository.

A Rustlang copy of the algorithm presented in PrimeCPP.

# PERFORMANCE

Intel i7 8700 base clock of 3.19 GHz but the tests were run at a boost clock of 4.28 GHz

In Rust:
```
Passes: 5973, Time: 5.0006636s, Avg: 837.211µs, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
```

Reference CPP run on the same machine:
```
Passes: 8529, Time: 5.000000, Avg: 0.000586, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
```



# HOW TO BUILD

Install Rust

then:
./run.cmd (Windows)
./run.sh (*nix)
