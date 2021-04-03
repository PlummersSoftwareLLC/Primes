# Primes | A Software Drag Race

Source code to [Dave's Garage](https://www.youtube.com/c/DavesGarage/featured) video
benchmarking the same prime number sieve in Python, C#, and C++.

[![Software Drag Racing | Dave's Garage](https://img.youtube.com/vi/D3h62rgewZM/0.jpg)](https://youtu.be/D3h62rgewZM)

## Results table of the original implementation

| System                      | Test                  | Results                                                               |
| --------------------------- | --------------------- | --------------------------------------------------------------------- |
| Apple MacBook Pro M1 (2020) | CPP                   | Passes: 8886, Time: 10.0000, Avg: **0.001125**, Limit: 1000000        |
| Apple MacBook Pro M1 (2020) | Python2.7.16 (python) | Passes: 10, Time: 10.311948061, Avg: **1.0311948061**, Limit: 1000000 |
| Apple MacBook Pro M1 (2020) | Python3.9.2 (python3) | Passes: 78, Time: 10.079185375, Avg: **0.1292203253**, Limit: 1000000 |
