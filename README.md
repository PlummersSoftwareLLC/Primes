# Primes | A Software Drag Race

Source code to [Dave's Garage](https://www.youtube.com/c/DavesGarage/featured) video
benchmarking the same prime number sieve in Python, C#, and C++; forked to add Rust to the benchmark.

[![Software Drag Racing | Dave's Garage](https://img.youtube.com/vi/D3h62rgewZM/0.jpg)](https://youtu.be/D3h62rgewZM)

---

# Results

In my AMD 3900XT machine here are the results:

- C++:
	
	- Compile command: `g++ -O3 -flto`
	- GCC version: 10.2
	- Result: `Passes: 10430, Time: 5.000000, Avg: 0.000479, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1`
	
- Rust:

	- Compile command: `cargo build --release`
	- Rust version: 1.51
	- Result: `Passes: 14768, Time: 5.0001297, Avg: 0.00033857865, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true`
	
	
---

- C# Dotnet Core
	
	- Run command: `dotnet run --configuration Release`
	- dotnet version: 5.0.201
	- Result: `Passes: 3812, Time: 10.0004937, Avg: 0.0026234243704092337, Limit: 1000000, Count: 78498, Valid: True`

- Python:

	- Run command: `python3 PrimePY.py`
	- Python version: Python 3.8.6
	- Result: `Passes: 68, Time: 10.042399984988151, Avg: 0.147682352720414, Limit: 1000000, Count: 78498, Valid: True`

