# Swift implementation of Dave's prime sieve

Source code to [Dave's Garage](https://www.youtube.com/c/DavesGarage/featured) video
benchmarking the same prime number sieve in Python, C#, Swift and C++.

This branch adds a Swift implementation that stays close to Dave's original C++ version that used a byte buffer to store the bits rather than std::vector<bool>.

The Swift version uses an UnsafeRawMutableBuffer for storage, which gives better performance than Array<UInt8> at the expense of safety.

Build with: `swiftc -Ounchecked -o prime primes.swift`

My results for 5 second runs on a MacBook Pro i7-7820HQ 2.9GHz running Big Sur 11.2.3.

| Language    | Passes |
| ----------- | ------ |
| C++         | 6,100  |
| Python      | 21     |
| C# via Mono | 622    |
| Swift       | 1,252  |

This comes with the proviso that I don't know what I'm doing with C#! For what it's worth I built the C# version with:  
`mcs -out:prime -optimize PrimeCS.cs`   
and ran as:  
`mono prime`


[![Software Drag Racing | Dave's Garage](https://img.youtube.com/vi/D3h62rgewZM/0.jpg)](https://youtu.be/D3h62rgewZM)
