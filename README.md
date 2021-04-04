# Primes | A Software Drag Race

Source code to [Dave's Garage](https://www.youtube.com/c/DavesGarage/featured) video
benchmarking the same prime number sieve in Python, C#, and C++; forked to add Rust to the benchmark.

[![Software Drag Racing | Dave's Garage](https://img.youtube.com/vi/D3h62rgewZM/0.jpg)](https://youtu.be/D3h62rgewZM)

---

# Results

In my AMD 3900XT machine here are the results:

```
make run
echo "C++:"
C++:
g++ -O3 -flto -march=native PrimeCPP/PrimeCPP.cpp && ./a.out
Passes: 9506, Time: 5.000000, Avg: 0.000526, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
echo "C#:"
C#:
cd PrimeCS && dotnet run --configuration Release
Passes: 4105, Time: 10.0015522, Avg: 0.0024364317174177834, Limit: 1000000, Count: 78498, Valid: True
echo "Rust:"
Rust:
cd PrimeRs && cargo run -q --release -- -C target-cpu=native
Passes: 15560, Time: 5.000209, Avg: 0.00032135018, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
echo "Python:"
Python:
python3 PrimeSievePY/PrimePY.py
Passes: 67, Time: 10.142712171000312, Avg: 0.15138376374627333, Limit: 1000000, Count: 78498, Valid: True
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