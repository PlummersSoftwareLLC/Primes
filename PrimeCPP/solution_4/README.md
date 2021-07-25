# C++ comprehensive solution by BlackMark

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-16-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-32-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-64-yellowgreen)
![Deviation](https://img.shields.io/badge/Deviation-compile%20time-blue)

This solution implements a generic compile-time configurable algorithm that can be used to generate all combinations of optimizations to exhaustively search for the optimal solution. All of the generated implementations are faithful. 

Additionally, a non-faithful solution is also included, which further improves upon the `constexpr` solution by flo80, by computing the complete sieve at compile-time and only copying the pre-generated sieve at run-time.

## Optimization configurations

The following optimizations are supported:

### Algorithms

- Wheel algorithm
    - Supports arbitrary wheel size and generates wheel at compile-time
    - Wheel sizes `0` (1 of 1) and `1` (1 of 2) are considered to be the base algorithm
- Non-faithful pre-generation algorithm
    - Uses compile-time 1 of 1 base algorithm and stores 1 bit in inverted `uint32_t` array

### Storages

- Vector storage, using `std::vector`, indicated by `vec<type>`
    - Can store any of the types `bool`, `uint8_t`, `uint16_t`, `uint32_t`, `uint64_t`
    - Can store bits inverted, indicated by `inv` prefix
- Array storage, using raw array, indicated by `arr<type>`
    - Can store any of the types `uint8_t`, `uint16_t`, `uint32_t`, `uint64_t`
    - Can store bits inverted, indicated by `inv` prefix
- Bit storage, using bits within an integer array, indicated by `bits<type>`
    - Can store any of `uint8_t`, `uint16_t`, `uint32_t`, `uint64_t`
    - Can store bits inverted, indicated by `inv` prefix
- Masked bit storage, same as bit storage, but pre-computes lookup table for masking single bit, indicated by `maskedbits<type>`
    - Can store any of `uint8_t`, `uint16_t`, `uint32_t`, `uint64_t`
    - Can store bits inverted, indicated by `inv` prefix
- Strided bit storage, same as bit storage, but puts consecutive bits a stride of `type` width apart, indicated by `stridedbits<type>`
    - Can store any of `uint8_t`, `uint16_t`, `uint32_t`, `uint64_t`
    - Can store bits inverted, indicated by `inv` prefix

### Half storage

- Not storing flags for even numbers is indicated by `hs` (half storage)
- Storing flags for all numbers is indicated by `fs` (full storage)

### Strides

- Dynamic stride uses the wheel to advance the loop(s)
    - Outer dynamic stride only uses the wheel for the outer loop, indicated by `os` (outer stride)
    - Both dynamic stride uses the wheel for the outer and inner loop, indicated by `bs` (both strides)
- Not using the wheel at all and advancing the loops by a constant stride is indicated by `cs` (constant stride)
    - Constant stride of `1` is used for half storage and/or wheel size `0`
    - Otherwise constant stride of `2` is used

### Parallelism

The implementation only supports running in an embarrassingly parallel configuration, as this will always yield better performance than anything that introduces dependencies.

### Compiler

The compiler also plays a significant role in performance, which is why both `gcc` and `clang` are used.

## Building and running

The solution uses a number of C++20 features and generally pushes C++ to its limit, which is why it requires `clang-12` and/or `gcc-11`, as earlier versions don't work due to compiler bugs. 

The provided makefile expects the environment variables `CXX_GCC` and `CXX_CLANG` to be set to the corresponding compiler. The following make run targets are supported:

- `make run_all` runs all of the following run targets
- `make run_base` runs the benchmark using the base algorithm
- `make run_wheel` runs the benchmark using the wheel algorithm
- `make run_pregen` runs the benchmark using the non-faithful pre-generated algorithm
- `make run_tests` runs all possible optimization configurations with sieve sizes from `0` to `50,000` and compares all computes primes to ones computed by a reference implementation using a naive `isPrime` test
- `make run_suite` runs all possible optimization configurations up to wheel size `6`, this takes a long time to compile and even longer to run
- `make run_bench` runs the base, wheel, and pregen run targets and is also the default configured for the docker benchmark

For each run target there is an equivalent build target with the same name, but without the `run_` prefix. 

These targets generate executables with both `gcc` and `clang`. 

Parallel runs will be performed using the number of threads reported by `std::thread::hardware_concurrency()`.

### Docker

The provided dockerfile runs the `make run_bench` target.

## Results

Using the `run_suite` target on both an Intel i9-9900K and AMD Ryzen 9 3950X yielded the following optimal configurations:

### Intel

#### Wheel 1-bit

- Wheel size 6 (5760 of 30030)
- Outer stride
- Half storage
- Inverted bits in `uint32_t` array
- Compiled with clang

#### Wheel 8-bit

- Wheel size 6 (5760 of 30030)
- Outer stride
- Half storage
- Inverted bits in `std::vector<uint8_t>` (using 8-bits per flag)
- Compiled with gcc

#### Base 1-bit

- Wheel size 1 (1 of 2)
- Constant stride
- Half storage
- Inverted and strided bits in `uint8_t` array
- Compiled with gcc

#### Base 8-bit

- Wheel size 1 (1 of 2)
- Both stride
- Half storage
- Inverted bits in `std::vector<uint8_t>` (using 8-bits per flag)
- Compiled with gcc

### AMD

#### Wheel 1-bit

- Wheel size 6 (5760 of 30030)
- Outer stride
- Half storage
- Inverted and masked bits in `uint32_t` array
- Compiled with gcc

#### Wheel 8-bit

- Wheel size 6 (5760 of 30030)
- Outer stride
- Half storage
- Inverted bits in `std::vector<uint8_t>` (using 8-bits per flag)
- Compiled with gcc

#### Base 1-bit

- Wheel size 1 (1 of 2)
- Constant stride
- Half storage
- Inverted and strided bits in `uint8_t` array
- Compiled with gcc

#### Base 8-bit

- Wheel size 1 (1 of 2)
- Constant stride
- Half storage
- Inverted bits in `bool` array (using one flag per bool)
- Compiled with clang

Using these results the `run_bench` target was configured to include these optimal configurations.

## Ranking

The following measurements were taken on an Intel i9-9900K and AMD Ryzen 9 3950X running each benchmark 30 times to account for scheduling noise. Only faithful solutions were used for the comparison, and only the fastest solutions are shown.

![intel_base_1](https://raw.githubusercontent.com/BlackMark29A/Primes/6300dee30609f93e1b8a137658635bb2ec0b12da/PrimeCPP/solution_4/images/intel_base_1-bit.png)

![intel_base_8](https://raw.githubusercontent.com/BlackMark29A/Primes/6300dee30609f93e1b8a137658635bb2ec0b12da/PrimeCPP/solution_4/images/intel_base_8-bit.png)

![intel_wheel_1](https://raw.githubusercontent.com/BlackMark29A/Primes/6300dee30609f93e1b8a137658635bb2ec0b12da/PrimeCPP/solution_4/images/intel_wheel_1-bit.png)

![intel_wheel_8](https://raw.githubusercontent.com/BlackMark29A/Primes/6300dee30609f93e1b8a137658635bb2ec0b12da/PrimeCPP/solution_4/images/intel_wheel_8-bit.png)

![amd_base_1](https://raw.githubusercontent.com/BlackMark29A/Primes/6300dee30609f93e1b8a137658635bb2ec0b12da/PrimeCPP/solution_4/images/amd_base_1-bit.png)

![amd_base_8](https://raw.githubusercontent.com/BlackMark29A/Primes/6300dee30609f93e1b8a137658635bb2ec0b12da/PrimeCPP/solution_4/images/amd_base_8-bit.png)

![amd_wheel_1](https://raw.githubusercontent.com/BlackMark29A/Primes/6300dee30609f93e1b8a137658635bb2ec0b12da/PrimeCPP/solution_4/images/amd_wheel_1-bit.png)

![amd_wheel_8](https://raw.githubusercontent.com/BlackMark29A/Primes/6300dee30609f93e1b8a137658635bb2ec0b12da/PrimeCPP/solution_4/images/amd_wheel_8-bit.png)
