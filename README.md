# Primes | A Software Drag Race


Originally forked from [Dave's Garage](https://www.youtube.com/c/DavesGarage/featured) video
benchmarking the same prime number sieve in Python, C#, and C++; forked to add Rust, Nim, D etc to the benchmark.

---

# Results

In my AMD 3900XT machine here are the results:

![Chart](primes.png)

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