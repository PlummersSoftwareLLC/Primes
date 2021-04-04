# Swift implementation of Dave's prime sieve

Source code to [Dave's Garage](https://www.youtube.com/c/DavesGarage/featured) video
benchmarking the same prime number sieve in Python, C#, Swift and C++.

This branch adds a Swift implementation that stays close to Dave's original C++ version that used a byte buffer to store the bits rather than std::vector\<bool\>.

The Swift version uses an UnsafeRawMutableBuffer for storage, which gives better performance than Array\<UInt8\> at the expense of safety.

Build with: `swiftc -Ounchecked -o prime primes.swift`

My results for 5 second runs on a MacBook Pro i7-7820HQ 2.9GHz running Big Sur 11.2.3.

| Language    | Passes |Build                                       |Run               |
| ----------- | ------ |--------------------------------------------|------------------|
| C++         | 6,100  |clang++ -O3 -std=c++17 -o prime PrimeCPP.cpp|./prime           |
| Python      | 21     |N/A                                         |python3 PrimePY.py|
| C# via Mono | 622    |mcs -out:prime -optimize PrimeCS.cs         |mono prime        |
| Swift       | 1,252  |swiftc -Ounchecked -o prime primes.swift    |./prime           |

This comes with the proviso that I don't know what I'm doing with C#!

[![Software Drag Racing | Dave's Garage](https://img.youtube.com/vi/D3h62rgewZM/0.jpg)](https://youtu.be/D3h62rgewZM)
