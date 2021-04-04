# Primes | A Software Drag Race

Source code to [Dave's Garage](https://www.youtube.com/c/DavesGarage/featured) video
benchmarking the same prime number sieve in Python, C#, and C++; forked to add Rust to the benchmark.

[![Software Drag Racing | Dave's Garage](https://img.youtube.com/vi/D3h62rgewZM/0.jpg)](https://youtu.be/D3h62rgewZM)

---

# Results

In my AMD 3900XT machine here are the results:

```
make run
g++ -O3 -flto PrimeCPP/PrimeCPP.cpp && ./a.out
Passes: 9686, Time: 5.000000, Avg: 0.000516, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
cd PrimeCS && dotnet run --configuration Release
Passes: 3942, Time: 10.0004223, Avg: 0.002536890487062405, Limit: 1000000, Count: 78498, Valid: True
cd PrimeRs && cargo run -q --release
Passes: 15607, Time: 5.0002155, Avg: 0.00032038288, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
python3 PrimeSievePY/PrimePY.py
Passes: 64, Time: 10.103218182004639, Avg: 0.15786278409382248, Limit: 1000000, Count: 78498, Valid: True
```


```
make print_versions 
g++ --version
g++ (Ubuntu 10.2.0-13ubuntu1) 10.2.0
Copyright (C) 2020 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

dotnet --version
5.0.201
rustc --version
rustc 1.51.0 (2fd73fabe 2021-03-23)
python3 --version
Python 3.8.6
```