# Primes | A Software Drag Race


Originally forked from [Dave's Garage](https://www.youtube.com/c/DavesGarage/featured) video
benchmarking the same prime number sieve in Python, C#, and C++; forked to add Rust, Nim, D etc to the benchmark.

[![Software Drag Racing | Dave's Garage](https://img.youtube.com/vi/D3h62rgewZM/0.jpg)](https://youtu.be/D3h62rgewZM)

---

# Results

In my AMD 3900XT machine here are the results:

```
$ make run
touch result.json
make cpp
make[1]: Entering directory '/home/bruce3434/proj/gh/Primes'
clang++ -O3 -flto -march=native PrimeCPP/PrimeCPP.cpp && ./a.out 
Passes: 922, Time: 5.000000, Avg: 0.005423, Limit: 10000000, Count1: 664579, Count2: 664579, Valid: 1
make[1]: Leaving directory '/home/bruce3434/proj/gh/Primes'
make rust
make[1]: Entering directory '/home/bruce3434/proj/gh/Primes'
cd PrimeRs && cargo run -q --release -- -C target-cpu=native 
Passes: 1093, Time: 5.000063, Avg: 0.004574623, Limit: 10000000, Count1: 664579, Count2: 664579, Valid: true
make[1]: Leaving directory '/home/bruce3434/proj/gh/Primes'
make nim
make[1]: Entering directory '/home/bruce3434/proj/gh/Primes'
cd PrimeNim && nimble build --d:danger --passC:-flto --passC:-march=native --silent && ./PrimeNim 
Passes: 974, Time: 5 seconds, 1 millisecond, 154 microseconds, and 997 nanoseconds, Avg: 5134656.0, Limit: 10000000, Count1: 664579, Count2: 664579, Valid: true
make[1]: Leaving directory '/home/bruce3434/proj/gh/Primes'
make d
make[1]: Entering directory '/home/bruce3434/proj/gh/Primes'
cd PrimeD && dub build -b release-nobounds -q --compiler=ldc2 && ./primed 
Passes: 764, Time: 5000, Avg: 6.5445, Limit: 10000000, Count1: 664579, Count2: 664579, Valid: true
make[1]: Leaving directory '/home/bruce3434/proj/gh/Primes'
make dotnet
make[1]: Entering directory '/home/bruce3434/proj/gh/Primes'
cd PrimeCS && dotnet run --configuration Release
Passes: 182, Time: 5.0035962, Avg: 0.02749228681318681, Limit: 10000000, Count: 664579, Valid: True
make[1]: Leaving directory '/home/bruce3434/proj/gh/Primes'
make python
make[1]: Entering directory '/home/bruce3434/proj/gh/Primes'
python3 PrimeSievePY/PrimePY.py
Passes: 3, Time: 5.586517693000133, Avg: 1.8621725643333775, Limit: 10000000, Count: 664579, Valid: True
make[1]: Leaving directory '/home/bruce3434/proj/gh/Primes'
```


```
make print_versions 
g++ --version
g++ (Ubuntu 10.2.0-13ubuntu1) 10.2.0
Copyright (C) 2020 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

dotnet --version
5.0.202
rustc --version
rustc 1.51.0 (2fd73fabe 2021-03-23)
python3 --version
Python 3.8.6
nim --version
Nim Compiler Version 1.4.4 [Linux: amd64]
Compiled at 2021-02-23
Copyright (c) 2006-2020 by Andreas Rumpf

git hash: 2ff517462bf8609b30e6134c9665
active boot switches: -d:release
```