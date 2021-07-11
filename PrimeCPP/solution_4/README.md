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
- Bit storage, using bits within an integer array, indicated by `bits<type>`
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

The provided makefile expects the environment variables `CXX_GCC` and `CXX_CLANG` to be set to the corresponding compiler. The following make targets are supported:

- `make run` runs the faithful benchmark configured to use the optimal configuration
- `make run_pregen` runs the non-faithful pre-generated algorithm
- `make run_bench` runs both faithful and non-faithful benchmark
- `make run_suite` runs all possible optimization configurations up to wheel size `7`, this takes a long time to compile and an even longer to run
- `make run_tests` runs all possible optimization configurations with sieve sizes from `0` to `50,000` and compares all computes primes to ones computed by a reference implementation using a naive `isPrime` test

These targets generate executables with both `gcc` and `clang`.

Parallel runs will be performed using the number of threads reported by `std::thread::hardware_concurrency()`, down to `1` thread, halving the number of threads each time.

### Docker

The provided dockerfile runs the `make run_bench` target.

## Results

Using the `run_suite` target on both an Intel i9-9900K and AMD Ryzen 9 3950X yielded the following optimal configurations:

### Intel

#### Wheel

- Wheel size 7 (92160 of 510510)
- Outer stride
- Half storage
- Inverted bits in `uint32_t` array
- Compiled with clang

#### Base

- Wheel size 1 (1 of 2)
- Outer stride
- Full storage
- Inverted bits in `uint32_t` array
- Compiled with clang

### AMD

#### Wheel

- Wheel size 7 (92160 of 510510)
- Outer stride
- Half storage
- Inverted bits in `uint8_t` vector (using 8-bits per flag)
- Compiled with gcc

#### Base

- Wheel size 1 (1 of 2)
- Constant stride
- Half storage
- Inverted bits in `uint8_t` vector (using 8-bits per flag)
- Compiled with gcc

Using these results the `run_bench` target was configured to include these optimal configurations (also including wheel size 6 for better comparability with existing solutions), which yields the following results:

<details>
<summary>Intel i9-9900K</summary>

```
                                                                             Single-threaded
┌───────┬────────────────┬──────────┬───────────────────────────────────────────────────────┬─────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label                                                 │ Passes  │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼───────────────────────────────────────────────────────┼─────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-clang            │ 1950811 │ 5.00000  │    1    │   base    │    no    │ 1    │ 390162.20000  │
│   2   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-gcc              │ 925443  │ 5.00000  │    1    │   base    │    no    │ 1    │ 185088.60000  │
│   3   │ cpp            │ 3        │ flo80_constexpr                                       │  90211  │ 5.00002  │    1    │   base    │    no    │ 1    │  18042.13866  │
│   4   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-clang     │  30245  │ 5.00002  │    1    │   wheel   │   yes    │ 1    │  6048.97580   │
│   5   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-clang       │  28089  │ 5.00016  │    1    │   wheel   │   yes    │ 1    │  5617.62024   │
│   6   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-gcc       │  24149  │ 5.00000  │    1    │   wheel   │   yes    │ 1    │  4829.80000   │
│   7   │ zig            │ 3        │ ManDeJan&ityonemo-zig-single-bitSieve-u8-5760of30030  │  23548  │ 5.00021  │    1    │   wheel   │   yes    │ 1    │  4709.40221   │
│   8   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-gcc         │  22447  │ 5.00018  │    1    │   wheel   │   yes    │ 1    │  4489.23839   │
│   9   │ c              │ 2        │ danielspaangberg_5760of30030_owrb                     │  21935  │ 5.00015  │    1    │   wheel   │   yes    │ 1    │  4386.86664   │
│  10   │ zig            │ 3        │ ManDeJan&ityonemo-zig-single-bitSieve-u8-480of2310    │  21447  │ 5.00014  │    1    │   wheel   │   yes    │ 1    │  4289.27990   │
│  11   │ c              │ 2        │ danielspaangberg_48of210                              │  21191  │ 5.00014  │    1    │   wheel   │   yes    │ 1    │  4238.08557   │
│  12   │ zig            │ 3        │ ManDeJan&ityonemo-zig-single-bitSieve-u64-480of2310   │  21098  │ 5.00010  │    1    │   wheel   │   yes    │ 1    │  4219.51561   │
│  13   │ zig            │ 3        │ ManDeJan&ityonemo-zig-single-bitSieve-u64-5760of30030 │  20120  │ 5.00001  │    1    │   wheel   │   yes    │ 1    │  4023.99195   │
│  14   │ c              │ 2        │ danielspaangberg_480of2310_owrb                       │  18900  │ 5.00002  │    1    │   wheel   │   yes    │ 1    │  3779.98564   │
│  15   │ c              │ 2        │ danielspaangberg_8of30                                │  18271  │ 5.00003  │    1    │   wheel   │   yes    │ 1    │  3654.17661   │
│  16   │ c              │ 2        │ danielspaangberg_48of210_owrb                         │  15717  │ 5.00020  │    1    │   wheel   │   yes    │ 1    │  3143.27427   │
│  17   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-gcc         │  12837  │ 5.00019  │    1    │   wheel   │   yes    │ 8    │  2567.30244   │
│  18   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-clang              │  12565  │ 5.00031  │    1    │   base    │   yes    │ 1    │  2512.84420   │
│  19   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-gcc           │  12501  │ 5.00039  │    1    │   wheel   │   yes    │ 8    │  2500.00500   │
│  20   │ c              │ 2        │ danielspaangberg_8of30_owrb                           │  11921  │ 5.00049  │    1    │   wheel   │   yes    │ 1    │  2383.96399   │
│  21   │ c              │ 2        │ danielspaangberg_1of2                                 │  11812  │ 5.00012  │    1    │   base    │   yes    │ 1    │  2362.34425   │
│  22   │ zig            │ 2        │ ManDeJan&ityonemo-zig-bit-sieve                       │  11509  │ 5.00038  │    1    │   base    │   yes    │ 1    │  2301.62508   │
│  23   │ zig            │ 3        │ ManDeJan&ityonemo-zig-single-bitSieve-u8              │  11483  │ 5.00041  │    1    │   base    │   yes    │ 1    │  2296.41169   │
│  24   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-gcc                │  10389  │ 5.00010  │    1    │   base    │   yes    │ 1    │  2077.75844   │
│  25   │ rust           │ 1        │ mike-barber_byte-storage                              │  9750   │ 5.00021  │    1    │   base    │   yes    │ 8    │  1949.92004   │
│  26   │ zig            │ 2        │ ManDeJan&ityonemo-zig-byte-sieve-type-bool            │  9624   │ 5.00015  │    1    │   base    │   yes    │ 8    │  1924.74226   │
│  27   │ zig            │ 3        │ ManDeJan&ityonemo-zig-single-sieve-u8                 │  9618   │ 5.00028  │    1    │   base    │   yes    │ 8    │  1923.49228   │
│  28   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-gcc                  │  9567   │ 5.00036  │    1    │   base    │   yes    │ 8    │  1913.26225   │
│  29   │ rust           │ 1        │ mike-barber_bit-storage-rotate                        │  9329   │ 5.00014  │    1    │   base    │   yes    │ 1    │  1865.74715   │
│  30   │ zig            │ 3        │ ManDeJan&ityonemo-zig-single-sieve-u8-5760of30030     │  9167   │ 5.00014  │    1    │   wheel   │   yes    │ 8    │  1833.34867   │
│  31   │ rust           │ 1        │ mike-barber_bit-storage                               │  9165   │ 5.00015  │    1    │   base    │   yes    │ 1    │  1832.94371   │
│  32   │ haskell        │ 1        │ fatho/bitset_unchecked                                │  9087   │ 5.00038  │    1    │   base    │    no    │ 1    │  1817.26152   │
│  33   │ haskell        │ 1        │ fatho/bitset                                          │  8714   │ 5.00018  │    1    │   base    │    no    │ 1    │  1742.73726   │
│  34   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-clang       │  8694   │ 5.00009  │    1    │   wheel   │   yes    │ 8    │  1738.76870   │
│  35   │ zig            │ 3        │ ManDeJan&ityonemo-zig-single-sieve-bool               │  8644   │ 5.00051  │    1    │   base    │   yes    │ 8    │  1728.62368   │
│  36   │ cpp            │ 1        │ davepl                                                │  8527   │ 5.00028  │    1    │   base    │   yes    │ 1    │  1705.30314   │
│  37   │ zig            │ 3        │ ManDeJan&ityonemo-zig-single-sieve-u8-480of2310       │  8517   │ 5.00048  │    1    │   wheel   │   yes    │ 8    │  1703.23649   │
│  38   │ fortran        │ 1        │ johandweber_fortran                                   │  8469   │ 5.00000  │    1    │   base    │    no    │ 1    │  1693.80000   │
│  39   │ c              │ 1        │ mckoss-c830                                           │  8358   │ 5.00000  │    1    │   wheel   │   yes    │ 1    │  1671.60000   │
│  40   │ assembly       │ 1        │ rbergen_x64uff_byte                                   │  8342   │ 5.00000  │    1    │   base    │    no    │ 8    │  1668.40000   │
│  41   │ v              │ 1        │ marghidanu                                            │  8197   │ 5.00000  │    1    │   base    │   yes    │      │  1639.40000   │
│  42   │ assembly       │ 1        │ rbergen_x64ff_byte                                    │  8172   │ 5.00000  │    1    │   base    │   yes    │ 8    │  1634.40000   │
│  43   │ go             │ 2        │ ssovest-go                                            │  8073   │ 5.00119  │    1    │   base    │   yes    │ 1    │  1614.21562   │
│  44   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-clang         │  8065   │ 5.00040  │    1    │   wheel   │   yes    │ 8    │  1612.87097   │
│  45   │ csharp         │ 1        │ kinematics_pool30                                     │  8045   │ 5.00029  │    1    │   wheel   │   yes    │ 1    │  1608.90668   │
│  46   │ ada            │ 1        │ BoopBeepBoopBeep                                      │  7927   │ 5.00028  │    1    │   base    │    no    │      │  1585.31110   │
│  47   │ julia          │ 2        │ epithet-jl                                            │  7827   │ 5.00059  │    1    │   base    │   yes    │ 1    │  1565.21655   │
│  48   │ crystal        │ 1        │ marghidanu                                            │  7718   │ 5.00003  │    1    │   base    │   yes    │ 1    │  1543.59012   │
│  49   │ zig            │ 1        │ devblok                                               │  7557   │ 5.00053  │    1    │   base    │   yes    │ 8    │  1511.23981   │
│  50   │ assembly       │ 1        │ rbergen_x64uff_bitshift                               │  7506   │ 5.00000  │    1    │   base    │    no    │ 1    │  1501.20000   │
│  51   │ haskell        │ 1        │ fatho/vector_unchecked                                │  7371   │ 5.00031  │    1    │   base    │    no    │ 8    │  1474.10831   │
│  52   │ haskell        │ 1        │ fatho/vector                                          │  6941   │ 5.00048  │    1    │   base    │    no    │ 8    │  1388.06730   │
│  53   │ csharp         │ 1        │ kinematics_pool30m                                    │  6802   │ 5.00039  │    1    │   wheel   │   yes    │ 1    │  1360.29390   │
│  54   │ d              │ 2        │ BradleyChatha                                         │  6706   │ 5.00054  │    1    │   base    │   yes    │ 1    │  1341.05517   │
│  55   │ csharp         │ 1        │ kinematics_dbool                                      │  6220   │ 5.00074  │    1    │   base    │   yes    │      │  1243.81592   │
│  56   │ fsharp         │ 2        │ dmannock_fsharp_port                                  │  6135   │ 5.00037  │    1    │   base    │   yes    │      │  1226.90847   │
│  57   │ csharp         │ 3        │ tannergooding                                         │  6024   │ 5.00009  │    1    │   base    │   yes    │ 1    │  1204.77834   │
│  58   │ csharp         │ 1        │ kinematics_pool2of6                                   │  6014   │ 5.00065  │    1    │   wheel   │   yes    │ 1    │  1202.64366   │
│  59   │ nodejs         │ 1        │ rogiervandam                                          │  5937   │ 5.00069  │    1    │   base    │   yes    │ 1    │  1187.23661   │
│  60   │ java           │ 1        │ MansenC                                               │  11840  │ 10.00000 │    1    │   base    │   yes    │      │  1184.00000   │
│  61   │ rust           │ 2        │ Azgrom                                                │  5771   │ 5.00027  │    1    │   base    │   yes    │      │  1154.13791   │
│  62   │ csharp         │ 1        │ kinematics_raw6                                       │  5668   │ 5.00049  │    1    │   wheel   │   yes    │ 1    │  1133.48892   │
│  63   │ fsharp         │ 3        │ dmannock_fsharp_recursion                             │  5630   │ 5.00016  │    1    │   base    │   yes    │      │  1125.96284   │
│  64   │ csharp         │ 1        │ kinematics_raw32                                      │  5454   │ 5.00060  │    1    │   base    │   yes    │ 1    │  1090.66912   │
│  65   │ assembly       │ 1        │ rbergen_x64ff_bitshift                                │  5427   │ 5.00000  │    1    │   base    │   yes    │ 1    │  1085.40000   │
│  66   │ rust           │ 4        │ joshallen64                                           │  5419   │ 5.00065  │    1    │   base    │   yes    │      │  1083.65923   │
│  67   │ csharp         │ 1        │ kinematics_raw                                        │  4999   │ 5.00015  │    1    │   base    │   yes    │ 1    │   999.77001   │
│  68   │ typescript     │ 1        │ marghidanu                                            │  4928   │ 5.00000  │    1    │   base    │   yes    │      │   985.60000   │
│  69   │ assembly       │ 1        │ rbergen_x64uff_bitbtr                                 │  4914   │ 5.00000  │    1    │   base    │    no    │ 1    │   982.80000   │
│  70   │ assembly       │ 1        │ rbergen_x64ff_bitbtr                                  │  4809   │ 5.00000  │    1    │   base    │   yes    │ 1    │   961.80000   │
│  71   │ csharp         │ 1        │ kinematics_rawd                                       │  4654   │ 5.00039  │    1    │   base    │   yes    │ 1    │   930.72740   │
│  72   │ lisp           │ 2        │ mayerrobert-cl                                        │  4142   │ 5.00000  │    1    │   base    │    no    │ 1    │   828.40000   │
│  73   │ dart           │ 1        │ eagerestwolf&mmcdon20                                 │  3985   │ 5.00067  │    1    │   base    │   yes    │ 8    │   796.89354   │
│  74   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-clang                │  3862   │ 5.00063  │    1    │   base    │   yes    │ 8    │   772.30269   │
│  75   │ nim            │ 2        │ beef331                                               │  3829   │ 5.00059  │    1    │   base    │   yes    │ 1    │   765.71000   │
│  76   │ nim            │ 1        │ marghidanu                                            │  3781   │ 5.00028  │    1    │   base    │   yes    │ 8    │   756.15749   │
│  77   │ lua            │ 2        │ ben1jen_luajit1                                       │  3710   │ 5.00000  │    1    │   base    │    no    │ 1    │   742.00000   │
│  78   │ d              │ 1        │ eagerestwolf                                          │  3387   │ 5.00000  │    1    │   base    │   yes    │ 8    │   677.40000   │
│  79   │ csharp         │ 1        │ kinematics_ibool                                      │  3277   │ 5.00154  │    1    │   base    │   yes    │      │   655.19820   │
│  80   │ basic          │ 1        │ rbergen_8of30                                         │  3240   │ 5.00100  │    1    │   wheel   │   yes    │ 1    │   647.87043   │
│  81   │ csharp         │ 1        │ kinematics_pool                                       │  3227   │ 5.00094  │    1    │   base    │   yes    │ 1    │   645.27869   │
│  82   │ cython         │ 1        │ rpkak                                                 │  3175   │ 5.00095  │    1    │   base    │   yes    │ 8    │   634.87883   │
│  83   │ go             │ 1        │ bundgaard                                             │  2930   │ 5.00090  │    1    │   base    │   yes    │      │   585.89489   │
│  84   │ csharp         │ 1        │ kinematics_bool                                       │  2905   │ 5.00051  │    1    │   base    │   yes    │      │   580.94074   │
│  85   │ rust           │ 3        │ Blui42                                                │  2177   │ 5.00003  │    1    │   base    │   yes    │      │   435.39763   │
│  86   │ python         │ 2        │ ssovest                                               │  1999   │ 5.00238  │    1    │   base    │   yes    │ 8    │   399.60990   │
│  87   │ fsharp         │ 1        │ rbergen                                               │  1905   │ 5.00035  │    1    │   base    │   yes    │ 1    │   380.97369   │
│  88   │ csharp         │ 1        │ kinematics_standard                                   │  1888   │ 5.00040  │    1    │   base    │   yes    │ 1    │   377.56979   │
│  89   │ basic          │ 2        │ rbergen_vb                                            │  1825   │ 5.00130  │    1    │   base    │   yes    │ 1    │   364.90520   │
│  90   │ scala          │ 1        │ rom1dep                                               │  1539   │ 5.00000  │    1    │   base    │   yes    │      │   307.80000   │
│  91   │ csharp         │ 1        │ kinematics_original                                   │  1495   │ 5.00095  │    1    │   base    │   yes    │ 1    │   298.94320   │
│  92   │ assemblyscript │ 1        │ donmahallem                                           │  2957   │ 10.00000 │    1    │   base    │   yes    │      │   295.70000   │
│  93   │ csharp         │ 2        │ davepl                                                │  2925   │ 10.00200 │    1    │   base    │   yes    │ 1    │   292.44151   │
│  94   │ pascal         │ 1        │ rbergen                                               │  1461   │ 5.00000  │    1    │   base    │   yes    │      │   292.20000   │
│  95   │ pony           │ 1        │ marghidanu                                            │  1375   │ 5.00000  │    1    │   base    │   yes    │ 1    │   275.00000   │
│  96   │ cobol          │ 1        │ fvbakel_Cobol                                         │  1370   │ 5.00000  │    1    │   base    │    no    │ 8    │   274.00000   │
│  97   │ octave         │ 1        │ octave                                                │  1255   │ 5.00171  │    1    │   base    │    no    │      │   250.91419   │
│  98   │ basic          │ 1        │ rbergen_boolean                                       │   989   │ 5.00300  │    1    │   base    │   yes    │      │   197.68139   │
│  99   │ ocaml          │ 2        │ gkpotter-unfaithful                                   │   762   │ 5.00632  │    1    │   base    │    no    │      │   152.20749   │
│  100  │ ocaml          │ 1        │ gkpotter-faithful                                     │   668   │ 5.00436  │    1    │   base    │   yes    │      │   133.48374   │
│  101  │ r              │ 1        │ fvbakel_R                                             │   652   │ 5.00500  │    1    │   base    │   yes    │ 32   │   130.26973   │
│  102  │ basic          │ 1        │ rbergen_bit64                                         │   592   │ 5.00000  │    1    │   base    │   yes    │ 1    │   118.40000   │
│  103  │ julia          │ 1        │ dcbi                                                  │   582   │ 5.00821  │    1    │   base    │   yes    │ 1    │   116.20921   │
│  104  │ basic          │ 1        │ rbergen_bit32                                         │   581   │ 5.00500  │    1    │   base    │   yes    │ 1    │   116.08392   │
│  105  │ swift          │ 1        │ j-f1                                                  │  1149   │ 10.00598 │    1    │   base    │   yes    │      │   114.83133   │
│  106  │ haxe           │ 1        │ TayIorRobinson_Haxe_C++                               │   872   │ 10.00731 │    1    │   base    │   yes    │      │   87.13628    │
│  107  │ postscript     │ 1        │ epithet-ps                                            │   86    │ 5.02300  │    1    │   base    │    no    │ 8    │   17.12124    │
│  108  │ php            │ 1        │ DennisdeBest                                          │   154   │ 10.04162 │    1    │   base    │   yes    │      │   15.33617    │
│  109  │ ruby           │ 1        │ rbergen                                               │   68    │ 5.05300  │    1    │   base    │   yes    │      │   13.45735    │
│  110  │ lisp           │ 1        │ mikehw                                                │   97    │ 10.10000 │    1    │   base    │    no    │ 1    │    9.60396    │
│  111  │ wren           │ 1        │ marghidanu                                            │   42    │ 5.05107  │    1    │   base    │   yes    │      │    8.31506    │
│  112  │ smalltalk      │ 1        │ fvbakel_smalltalk                                     │   28    │ 5.04900  │    1    │   base    │   yes    │ 1    │    5.54565    │
│  113  │ mixal          │ 1        │ rbergen                                               │   30    │ 5.44000  │    1    │   base    │    no    │ 1    │    5.51471    │
│  114  │ perl           │ 1        │ marghidanu                                            │   22    │ 5.09874  │    1    │   base    │   yes    │      │    4.31479    │
│  115  │ haxe           │ 1        │ TayIorRobinson_Haxe_HaxeEval                          │   42    │ 10.00906 │    1    │   base    │   yes    │      │    4.19620    │
│  116  │ python         │ 1        │ davepl                                                │   32    │ 10.21840 │    1    │   base    │   yes    │      │    3.13161    │
│  117  │ powershell     │ 2        │ crowbar27_ps2                                         │    7    │ 5.45838  │    1    │   base    │   yes    │ 1    │    1.28243    │
│  118  │ sql            │ 2        │ fvbakel_MariaDB3                                      │    6    │ 5.14800  │    1    │   other   │    no    │ 32   │    1.16550    │
│  119  │ tcl            │ 1        │ fvbakeltcl                                            │    6    │ 5.61700  │    1    │   base    │   yes    │ 1    │    1.06819    │
│  120  │ tcl            │ 2        │ fvbakel_ootcl                                         │    5    │ 5.39900  │    1    │   base    │   yes    │ 1    │    0.92610    │
│  121  │ sql            │ 1        │ fvbakel_sqlite                                        │    5    │ 5.90071  │    1    │   other   │    no    │ 8    │    0.84736    │
│  122  │ sql            │ 2        │ fvbakel_MariaDB2                                      │    4    │ 5.14300  │    1    │   other   │    no    │ 32   │    0.77776    │
│  123  │ haxe           │ 1        │ TayIorRobinson_Haxe_Python                            │    7    │ 10.42407 │    1    │   base    │   yes    │      │    0.67152    │
│  124  │ sql            │ 2        │ fvbakel_MariaDB1                                      │    1    │ 7.07600  │    1    │   base    │    no    │ 32   │    0.14132    │
│  125  │ latex          │ 1        │ tjol                                                  │    2    │ 17.58553 │    1    │   base    │    no    │ 32   │    0.11373    │
│  126  │ prolog         │ 1        │ jimbxb_prolog                                         │    1    │ 9.08500  │    1    │   base    │   yes    │ 1    │    0.11007    │
│  127  │ bash           │ 1        │ bash                                                  │    1    │ 14.21945 │    1    │   base    │    no    │      │    0.07033    │
│  128  │ elixir         │ 1        │ cdesch                                                │    1    │ 15.07000 │    1    │   base    │    no    │      │    0.06636    │
│  129  │ powershell     │ 1        │ crowbar27_ps1                                         │    1    │ 65.10530 │    1    │   base    │   yes    │ 1    │    0.01536    │
│  130  │ lua            │ 1        │ lua                                                   │    1    │ 68.00000 │    1    │   base    │    no    │ 64   │    0.01471    │
└───────┴────────────────┴──────────┴───────────────────────────────────────────────────────┴─────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
                                                                                    Multi-threaded
┌───────┬────────────────┬──────────┬───────────────────────────────────────────────────────────────────┬──────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label                                                             │  Passes  │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼───────────────────────────────────────────────────────────────────┼──────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-clang                        │ 3541825  │ 5.00000  │    2    │   base    │    no    │ 1    │ 354182.50000  │
│   2   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-clang                        │ 6842764  │ 5.00000  │    4    │   base    │    no    │ 1    │ 342138.20000  │
│   3   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-clang                        │ 11349792 │ 5.00000  │    8    │   base    │    no    │ 1    │ 283744.80000  │
│   4   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-clang                        │ 16132504 │ 5.00000  │   16    │   base    │    no    │ 1    │ 201656.30000  │
│   5   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-gcc                          │ 1884968  │ 5.00000  │    2    │   base    │    no    │ 1    │ 188496.80000  │
│   6   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-gcc                          │ 3272869  │ 5.00000  │    4    │   base    │    no    │ 1    │ 163643.45000  │
│   7   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-gcc                          │ 4784369  │ 5.00000  │    8    │   base    │    no    │ 1    │ 119609.22500  │
│   8   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-gcc                          │ 4687904  │ 5.00020  │   16    │   base    │    no    │ 1    │  58596.45614  │
│   9   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-clang                 │  58998   │ 5.00016  │    2    │   wheel   │   yes    │ 1    │  5899.61121   │
│  10   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-clang                   │  56021   │ 5.00007  │    2    │   wheel   │   yes    │ 1    │  5602.02157   │
│  11   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-clang                 │  103581  │ 5.00014  │    4    │   wheel   │   yes    │ 1    │  5178.90499   │
│  12   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-gcc                   │  48347   │ 5.00015  │    2    │   wheel   │   yes    │ 1    │  4834.55496   │
│  13   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-clang                   │  93475   │ 5.00019  │    4    │   wheel   │   yes    │ 1    │  4673.57240   │
│  14   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-clang                 │  174823  │ 5.00016  │    8    │   wheel   │   yes    │ 1    │  4370.43515   │
│  15   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-gcc                   │  84696   │ 5.00033  │    4    │   wheel   │   yes    │ 1    │  4234.52052   │
│  16   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-clang                   │  164493  │ 5.00032  │    8    │   wheel   │   yes    │ 1    │  4112.06183   │
│  17   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-gcc                     │  40069   │ 5.00009  │    2    │   wheel   │   yes    │ 1    │  4006.82788   │
│  18   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-gcc                     │  78319   │ 5.00016  │    4    │   wheel   │   yes    │ 1    │  3915.82469   │
│  19   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-gcc                   │  142094  │ 5.00020  │    8    │   wheel   │   yes    │ 1    │  3552.20791   │
│  20   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-gcc                     │  130505  │ 5.00033  │    8    │   wheel   │   yes    │ 1    │  3262.40968   │
│  21   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-clang                 │  236970  │ 5.00071  │   16    │   wheel   │   yes    │ 1    │  2961.70444   │
│  22   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-clang                   │  219234  │ 5.00086  │   16    │   wheel   │   yes    │ 1    │  2739.95373   │
│  23   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-gcc                       │  24805   │ 5.00012  │    2    │   wheel   │   yes    │ 8    │  2480.44047   │
│  24   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-clang                          │  23990   │ 5.00038  │    2    │   base    │   yes    │ 1    │  2398.81769   │
│  25   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-gcc                     │  23985   │ 5.00051  │    2    │   wheel   │   yes    │ 8    │  2398.25538   │
│  26   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-gcc                   │  190769  │ 5.00044  │   16    │   wheel   │   yes    │ 1    │  2384.40267   │
│  27   │ zig            │ 3        │ ManDeJan&ityonemo-zig-parallel-gustafson-bitSieve-u8-5760of30030  │  184871  │ 5.00002  │   16    │   wheel   │   yes    │ 1    │  2310.87826   │
│  28   │ zig            │ 3        │ ManDeJan&ityonemo-zig-parallel-gustafson-bitSieve-u64-5760of30030 │  176220  │ 5.00002  │   16    │   wheel   │   yes    │ 1    │  2202.74119   │
│  29   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-clang                          │  43362   │ 5.00043  │    4    │   base    │   yes    │ 1    │  2167.91356   │
│  30   │ c              │ 2        │ danielspaangberg_5760of30030_epar                                 │  172366  │ 5.00295  │   16    │   wheel   │   yes    │ 1    │  2153.30670   │
│  31   │ zig            │ 3        │ ManDeJan&ityonemo-zig-parallel-gustafson-bitSieve-u64-480of2310   │  171102  │ 5.00056  │   16    │   wheel   │   yes    │ 1    │  2138.53548   │
│  32   │ zig            │ 3        │ ManDeJan&ityonemo-zig-parallel-gustafson-bitSieve-u8-480of2310    │  168058  │ 5.00024  │   16    │   wheel   │   yes    │ 1    │  2100.62417   │
│  33   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-gcc                     │  41784   │ 5.00023  │    4    │   wheel   │   yes    │ 8    │  2089.10390   │
│  34   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-gcc                            │  20449   │ 5.00009  │    2    │   base    │   yes    │ 1    │  2044.86319   │
│  35   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-gcc                     │  163275  │ 5.00185  │   16    │   wheel   │   yes    │ 1    │  2040.18263   │
│  36   │ c              │ 2        │ danielspaangberg_480of2310_epar                                   │  162431  │ 5.00278  │   16    │   wheel   │   yes    │ 1    │  2029.25883   │
│  37   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-gcc                       │  40522   │ 5.00036  │    4    │   wheel   │   yes    │ 8    │  2025.95413   │
│  38   │ c              │ 2        │ danielspaangberg_48of210_epar                                     │  150286  │ 5.00377  │   16    │   wheel   │   yes    │ 1    │  1877.15850   │
│  39   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-gcc                              │  18539   │ 5.00032  │    2    │   base    │   yes    │ 8    │  1853.78136   │
│  40   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-clang                          │  73863   │ 5.00071  │    8    │   base    │   yes    │ 1    │  1846.31282   │
│  41   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-gcc                            │  35919   │ 5.00034  │    4    │   base    │   yes    │ 1    │  1795.82788   │
│  42   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-clang                   │  17303   │ 5.00040  │    2    │   wheel   │   yes    │ 8    │  1730.16159   │
│  43   │ cpp            │ 3        │ flo80_constexpr                                                   │  135664  │ 5.00045  │   16    │   base    │    no    │ 1    │  1695.64807   │
│  44   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-clang                   │  32362   │ 5.00047  │    4    │   wheel   │   yes    │ 8    │  1617.94791   │
│  45   │ c              │ 2        │ danielspaangberg_8of30_epar                                       │  125473  │ 5.00260  │   16    │   wheel   │   yes    │ 1    │  1567.59704   │
│  46   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-clang                     │  15561   │ 5.00035  │    2    │   wheel   │   yes    │ 8    │  1555.99108   │
│  47   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-gcc                              │  31020   │ 5.00094  │    4    │   base    │   yes    │ 8    │  1550.70847   │
│  48   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-gcc                            │  60440   │ 5.00094  │    8    │   base    │   yes    │ 1    │  1510.71599   │
│  49   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-clang                     │  29752   │ 5.00045  │    4    │   wheel   │   yes    │ 8    │  1487.46613   │
│  50   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-gcc                       │  56266   │ 5.00075  │    8    │   wheel   │   yes    │ 8    │  1406.43903   │
│  51   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-gcc                     │  54825   │ 5.00044  │    8    │   wheel   │   yes    │ 8    │  1370.50440   │
│  52   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-clang                   │  50845   │ 5.00066  │    8    │   wheel   │   yes    │ 8    │  1270.95723   │
│  53   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-clang                          │  101248  │ 5.00073  │   16    │   base    │   yes    │ 1    │  1265.41525   │
│  54   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-clang                     │  48056   │ 5.00076  │    8    │   wheel   │   yes    │ 8    │  1201.21741   │
│  55   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-gcc                              │  45704   │ 5.00059  │    8    │   base    │   yes    │ 8    │  1142.46519   │
│  56   │ zig            │ 3        │ ManDeJan&ityonemo-zig-parallel-gustafson-bitSieve-u8              │  89098   │ 5.00019  │   16    │   base    │   yes    │ 1    │  1113.68268   │
│  57   │ c              │ 2        │ danielspaangberg_1of2_epar                                        │  87791   │ 5.00270  │   16    │   base    │   yes    │ 1    │  1096.79413   │
│  58   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-gcc                            │  77445   │ 5.00205  │   16    │   base    │   yes    │ 1    │   967.66576   │
│  59   │ zig            │ 3        │ ManDeJan&ityonemo-zig-parallel-gustafson-bitSieve-u64             │  74380   │ 5.00065  │   16    │   base    │   yes    │ 1    │   929.62915   │
│  60   │ rust           │ 1        │ mike-barber_bit-storage                                           │  74106   │ 5.00080  │   16    │   base    │   yes    │ 1    │   926.17732   │
│  61   │ rust           │ 1        │ mike-barber_bit-storage-rotate                                    │  73742   │ 5.00079  │   16    │   base    │   yes    │ 1    │   921.62927   │
│  62   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-clang                            │   7726   │ 5.00067  │    2    │   base    │   yes    │ 8    │   772.49649   │
│  63   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-clang                            │  14463   │ 5.00140  │    4    │   base    │   yes    │ 8    │   722.94757   │
│  64   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-gcc                     │  54459   │ 5.00094  │   16    │   wheel   │   yes    │ 8    │   680.60955   │
│  65   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-clang                            │  26969   │ 5.00086  │    8    │   base    │   yes    │ 8    │   674.10905   │
│  66   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-gcc                       │  53053   │ 5.00089  │   16    │   wheel   │   yes    │ 8    │   663.04448   │
│  67   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-clang                   │  52517   │ 5.00110  │   16    │   wheel   │   yes    │ 8    │   656.31811   │
│  68   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-clang                     │  52039   │ 5.00082  │   16    │   wheel   │   yes    │ 8    │   650.38084   │
│  69   │ c              │ 2        │ danielspaangberg_5760of30030_par                                  │  12885   │ 5.00007  │    4    │   wheel   │   yes    │ 1    │   644.24072   │
│  70   │ c              │ 2        │ danielspaangberg_480of2310_par                                    │  12579   │ 5.00021  │    4    │   wheel   │   yes    │ 1    │   628.92421   │
│  71   │ c              │ 2        │ danielspaangberg_48of210_par                                      │  12432   │ 5.00038  │    4    │   wheel   │   yes    │ 1    │   621.55239   │
│  72   │ c              │ 2        │ danielspaangberg_8of30_par                                        │  12063   │ 5.00039  │    4    │   wheel   │   yes    │ 1    │   603.10248   │
│  73   │ d              │ 2        │ BradleyChatha-Multi                                               │  46705   │ 5.00139  │   16    │   base    │   yes    │ 1    │   583.65025   │
│  74   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-gcc                              │  46570   │ 5.00238  │   16    │   base    │   yes    │ 8    │   581.84804   │
│  75   │ zig            │ 3        │ ManDeJan&ityonemo-zig-parallel-gustafson-sieve-u8                 │  46529   │ 5.00010  │   16    │   base    │   yes    │ 8    │   581.60087   │
│  76   │ rust           │ 1        │ mike-barber_byte-storage                                          │  45985   │ 5.00126  │   16    │   base    │   yes    │ 8    │   574.66787   │
│  77   │ c              │ 2        │ danielspaangberg_1of2_par                                         │  10293   │ 5.00039  │    4    │   base    │   yes    │ 1    │   514.61027   │
│  78   │ cpp            │ 2        │ davepl_par                                                        │  41040   │ 5.00163  │   16    │   base    │   yes    │ 1    │   512.83282   │
│  79   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-clang                            │  39484   │ 5.00160  │   16    │   base    │   yes    │ 8    │   493.39211   │
│  80   │ zig            │ 3        │ ManDeJan&ityonemo-zig-parallel-amdahl-sieve-u8                    │  14878   │ 5.00002  │   16    │   base    │   yes    │ 8    │   185.97426   │
│  81   │ csharp         │ 1        │ kinematics_pool6p                                                 │   2850   │ 5.00006  │   16    │   wheel   │   yes    │ 1    │   35.62457    │
│  82   │ csharp         │ 1        │ kinematics_rawp                                                   │   2770   │ 5.00021  │   16    │   base    │   yes    │ 1    │   34.62355    │
└───────┴────────────────┴──────────┴───────────────────────────────────────────────────────────────────┴──────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
```

</details>

<details>
<summary>AMD Ryzen 9 3950X</summary>

```
                                                                             Single-threaded
┌───────┬────────────────┬──────────┬───────────────────────────────────────────────────────┬─────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label                                                 │ Passes  │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼───────────────────────────────────────────────────────┼─────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-clang            │ 2425985 │ 5.00000  │    1    │   base    │    no    │ 1    │ 485197.00000  │
│   2   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-gcc              │ 1168431 │ 5.00000  │    1    │   base    │    no    │ 1    │ 233686.20000  │
│   3   │ cpp            │ 3        │ flo80_constexpr                                       │ 151532  │ 5.00001  │    1    │   base    │    no    │ 1    │  30306.33333  │
│   4   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-gcc         │  30124  │ 5.00006  │    1    │   wheel   │   yes    │ 8    │  6024.72770   │
│   5   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-gcc           │  29174  │ 5.00002  │    1    │   wheel   │   yes    │ 8    │  5834.77666   │
│   6   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-clang       │  28692  │ 5.00014  │    1    │   wheel   │   yes    │ 1    │  5738.23933   │
│   7   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-gcc       │  28105  │ 5.00018  │    1    │   wheel   │   yes    │ 1    │  5620.79765   │
│   8   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-clang     │  27805  │ 5.00018  │    1    │   wheel   │   yes    │ 1    │  5560.79981   │
│   9   │ zig            │ 3        │ ManDeJan&ityonemo-zig-single-bitSieve-u8-5760of30030  │  27555  │ 5.00003  │    1    │   wheel   │   yes    │ 1    │  5510.96693   │
│  10   │ zig            │ 3        │ ManDeJan&ityonemo-zig-single-bitSieve-u8-480of2310    │  25533  │ 5.00017  │    1    │   wheel   │   yes    │ 1    │  5106.42638   │
│  11   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-gcc         │  25511  │ 5.00009  │    1    │   wheel   │   yes    │ 1    │  5102.10816   │
│  12   │ zig            │ 3        │ ManDeJan&ityonemo-zig-single-bitSieve-u64-5760of30030 │  25010  │ 5.00012  │    1    │   wheel   │   yes    │ 1    │  5001.87995   │
│  13   │ zig            │ 3        │ ManDeJan&ityonemo-zig-single-bitSieve-u64-480of2310   │  23204  │ 5.00007  │    1    │   wheel   │   yes    │ 1    │  4640.73503   │
│  14   │ c              │ 2        │ danielspaangberg_48of210                              │  22199  │ 5.00014  │    1    │   wheel   │   yes    │ 1    │  4439.67569   │
│  15   │ c              │ 2        │ danielspaangberg_5760of30030_owrb                     │  19032  │ 5.00019  │    1    │   wheel   │   yes    │ 1    │  3806.25765   │
│  16   │ c              │ 2        │ danielspaangberg_8of30                                │  19026  │ 5.00017  │    1    │   wheel   │   yes    │ 1    │  3805.07139   │
│  17   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-gcc                  │  17758  │ 5.00010  │    1    │   base    │   yes    │ 8    │  3551.52897   │
│  18   │ rust           │ 1        │ mike-barber_byte-storage                              │  17719  │ 5.00016  │    1    │   base    │   yes    │ 8    │  3543.68983   │
│  19   │ zig            │ 2        │ ManDeJan&ityonemo-zig-byte-sieve-type-bool            │  17684  │ 5.00012  │    1    │   base    │   yes    │ 8    │  3536.71512   │
│  20   │ assembly       │ 1        │ rbergen_x64uff_byte                                   │  16465  │ 5.00000  │    1    │   base    │    no    │ 8    │  3293.00000   │
│  21   │ c              │ 2        │ danielspaangberg_480of2310_owrb                       │  16363  │ 5.00020  │    1    │   wheel   │   yes    │ 1    │  3272.46714   │
│  22   │ v              │ 1        │ marghidanu                                            │  15058  │ 5.00000  │    1    │   base    │   yes    │      │  3011.60000   │
│  23   │ ada            │ 1        │ BoopBeepBoopBeep                                      │  14653  │ 5.00006  │    1    │   base    │    no    │      │  2930.56414   │
│  24   │ haskell        │ 1        │ fatho/vector_unchecked                                │  14509  │ 5.00030  │    1    │   base    │    no    │ 8    │  2901.62474   │
│  25   │ c              │ 2        │ danielspaangberg_48of210_owrb                         │  14031  │ 5.00028  │    1    │   wheel   │   yes    │ 1    │  2806.04398   │
│  26   │ zig            │ 3        │ ManDeJan&ityonemo-zig-single-bitSieve-u8              │  13936  │ 5.00013  │    1    │   base    │   yes    │ 1    │  2787.12753   │
│  27   │ zig            │ 1        │ devblok                                               │  13685  │ 5.00017  │    1    │   base    │   yes    │ 8    │  2736.90695   │
│  28   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-clang              │  13176  │ 5.00003  │    1    │   base    │   yes    │ 1    │  2635.18419   │
│  29   │ haskell        │ 1        │ fatho/vector                                          │  12835  │ 5.00035  │    1    │   base    │    no    │ 8    │  2566.82186   │
│  30   │ c              │ 2        │ danielspaangberg_1of2                                 │  12578  │ 5.00040  │    1    │   base    │   yes    │ 1    │  2515.40078   │
│  31   │ c              │ 1        │ mckoss-c830                                           │  12479  │ 5.00000  │    1    │   wheel   │   yes    │ 1    │  2495.80000   │
│  32   │ assembly       │ 1        │ rbergen_x64ff_byte                                    │  12125  │ 5.00000  │    1    │   base    │   yes    │ 8    │  2425.00000   │
│  33   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-gcc                │  11876  │ 5.00042  │    1    │   base    │   yes    │ 1    │  2375.00050   │
│  34   │ rust           │ 1        │ mike-barber_bit-storage-rotate                        │  11836  │ 5.00036  │    1    │   base    │   yes    │ 1    │  2367.02912   │
│  35   │ rust           │ 1        │ mike-barber_bit-storage                               │  11645  │ 5.00043  │    1    │   base    │   yes    │ 1    │  2328.79856   │
│  36   │ haskell        │ 1        │ fatho/bitset_unchecked                                │  11342  │ 5.00038  │    1    │   base    │    no    │ 1    │  2268.22761   │
│  37   │ zig            │ 2        │ ManDeJan&ityonemo-zig-bit-sieve                       │  10925  │ 5.00002  │    1    │   base    │   yes    │ 1    │  2184.99126   │
│  38   │ fortran        │ 1        │ johandweber_fortran                                   │  10894  │ 5.00000  │    1    │   base    │    no    │ 1    │  2178.80000   │
│  39   │ zig            │ 3        │ ManDeJan&ityonemo-zig-single-sieve-u8                 │  10450  │ 5.00020  │    1    │   base    │   yes    │ 8    │  2089.91640   │
│  40   │ c              │ 2        │ danielspaangberg_8of30_owrb                           │  10330  │ 5.00042  │    1    │   wheel   │   yes    │ 1    │  2065.82812   │
│  41   │ rust           │ 2        │ Azgrom                                                │  10313  │ 5.00016  │    1    │   base    │   yes    │      │  2062.53507   │
│  42   │ haskell        │ 1        │ fatho/bitset                                          │  10213  │ 5.00001  │    1    │   base    │    no    │ 1    │  2042.59551   │
│  43   │ cpp            │ 1        │ davepl                                                │  9973   │ 5.00025  │    1    │   base    │   yes    │ 1    │  1994.50067   │
│  44   │ crystal        │ 1        │ marghidanu                                            │  9665   │ 5.00047  │    1    │   base    │   yes    │ 1    │  1932.81832   │
│  45   │ fsharp         │ 3        │ dmannock_fsharp_recursion                             │  9599   │ 5.00046  │    1    │   base    │   yes    │      │  1919.62339   │
│  46   │ fsharp         │ 2        │ dmannock_fsharp_port                                  │  9595   │ 5.00021  │    1    │   base    │   yes    │      │  1918.92094   │
│  47   │ csharp         │ 1        │ kinematics_dbool                                      │  8868   │ 5.00034  │    1    │   base    │   yes    │      │  1773.47940   │
│  48   │ rust           │ 4        │ joshallen64                                           │  8469   │ 5.00051  │    1    │   base    │   yes    │      │  1693.62847   │
│  49   │ zig            │ 3        │ ManDeJan&ityonemo-zig-single-sieve-bool               │  8441   │ 5.00030  │    1    │   base    │   yes    │ 8    │  1688.09871   │
│  50   │ java           │ 1        │ MansenC                                               │  16552  │ 10.00000 │    1    │   base    │   yes    │      │  1655.20000   │
│  51   │ julia          │ 2        │ epithet-jl                                            │  7911   │ 5.00055  │    1    │   base    │   yes    │ 1    │  1582.02597   │
│  52   │ zig            │ 3        │ ManDeJan&ityonemo-zig-single-sieve-u8-480of2310       │  7757   │ 5.00048  │    1    │   wheel   │   yes    │ 8    │  1551.25108   │
│  53   │ zig            │ 3        │ ManDeJan&ityonemo-zig-single-sieve-u8-5760of30030     │  7741   │ 5.00019  │    1    │   wheel   │   yes    │ 8    │  1548.14117   │
│  54   │ csharp         │ 3        │ tannergooding                                         │  7596   │ 5.00025  │    1    │   base    │   yes    │ 1    │  1519.12435   │
│  55   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-clang       │  6941   │ 5.00014  │    1    │   wheel   │   yes    │ 8    │  1388.16113   │
│  56   │ assembly       │ 1        │ rbergen_x64uff_bitbtr                                 │  6783   │ 5.00000  │    1    │   base    │    no    │ 1    │  1356.60000   │
│  57   │ assembly       │ 1        │ rbergen_x64uff_bitshift                               │  6775   │ 5.00000  │    1    │   base    │    no    │ 1    │  1355.00000   │
│  58   │ assembly       │ 1        │ rbergen_x64ff_bitshift                                │  6624   │ 5.00000  │    1    │   base    │   yes    │ 1    │  1324.80000   │
│  59   │ assembly       │ 1        │ rbergen_x64ff_bitbtr                                  │  6596   │ 5.00000  │    1    │   base    │   yes    │ 1    │  1319.20000   │
│  60   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-clang         │  6579   │ 5.00039  │    1    │   wheel   │   yes    │ 8    │  1315.69738   │
│  61   │ nodejs         │ 1        │ rogiervandam                                          │  6485   │ 5.00066  │    1    │   base    │   yes    │ 1    │  1296.82774   │
│  62   │ go             │ 2        │ ssovest-go                                            │  6457   │ 5.00080  │    1    │   base    │   yes    │ 1    │  1291.19391   │
│  63   │ csharp         │ 1        │ kinematics_raw32                                      │  6382   │ 5.00042  │    1    │   base    │   yes    │ 1    │  1276.29279   │
│  64   │ csharp         │ 1        │ kinematics_rawd                                       │  6316   │ 5.00056  │    1    │   base    │   yes    │ 1    │  1263.05854   │
│  65   │ csharp         │ 1        │ kinematics_pool30m                                    │  6212   │ 5.00015  │    1    │   wheel   │   yes    │ 1    │  1242.36273   │
│  66   │ csharp         │ 1        │ kinematics_raw6                                       │  6204   │ 5.00052  │    1    │   wheel   │   yes    │ 1    │  1240.67097   │
│  67   │ csharp         │ 1        │ kinematics_pool30                                     │  6196   │ 5.00038  │    1    │   wheel   │   yes    │ 1    │  1239.10583   │
│  68   │ csharp         │ 1        │ kinematics_raw                                        │  6168   │ 5.00004  │    1    │   base    │   yes    │ 1    │  1233.59013   │
│  69   │ dart           │ 1        │ eagerestwolf&mmcdon20                                 │  5906   │ 5.00049  │    1    │   base    │   yes    │ 8    │  1181.08355   │
│  70   │ typescript     │ 1        │ marghidanu                                            │  5867   │ 5.00000  │    1    │   base    │   yes    │      │  1173.40000   │
│  71   │ lisp           │ 2        │ mayerrobert-cl                                        │  5713   │ 5.00394  │    1    │   base    │    no    │ 1    │  1141.70080   │
│  72   │ d              │ 2        │ BradleyChatha                                         │  5553   │ 5.00082  │    1    │   base    │   yes    │ 1    │  1110.41789   │
│  73   │ csharp         │ 1        │ kinematics_pool2of6                                   │  5361   │ 5.00044  │    1    │   wheel   │   yes    │ 1    │  1072.10565   │
│  74   │ rust           │ 3        │ Blui42                                                │  4148   │ 5.00006  │    1    │   base    │   yes    │      │   829.59001   │
│  75   │ lua            │ 2        │ ben1jen_luajit1                                       │  3570   │ 5.00000  │    1    │   base    │    no    │ 1    │   714.00000   │
│  76   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-clang                │  3307   │ 5.00066  │    1    │   base    │   yes    │ 8    │   661.31271   │
│  77   │ nim            │ 2        │ beef331                                               │  3206   │ 5.00147  │    1    │   base    │   yes    │ 1    │   641.01104   │
│  78   │ d              │ 1        │ eagerestwolf                                          │  3131   │ 5.00000  │    1    │   base    │   yes    │ 8    │   626.20000   │
│  79   │ csharp         │ 1        │ kinematics_ibool                                      │  3124   │ 5.00070  │    1    │   base    │   yes    │      │   624.71254   │
│  80   │ cython         │ 1        │ rpkak                                                 │  2977   │ 5.00054  │    1    │   base    │   yes    │ 8    │   595.33532   │
│  81   │ nim            │ 1        │ marghidanu                                            │  2910   │ 5.00137  │    1    │   base    │   yes    │ 8    │   581.84041   │
│  82   │ csharp         │ 1        │ kinematics_pool                                       │  2826   │ 5.00046  │    1    │   base    │   yes    │ 1    │   565.14801   │
│  83   │ go             │ 1        │ bundgaard                                             │  2781   │ 5.00105  │    1    │   base    │   yes    │      │   556.08289   │
│  84   │ csharp         │ 1        │ kinematics_bool                                       │  2760   │ 5.00100  │    1    │   base    │   yes    │      │   551.88962   │
│  85   │ basic          │ 1        │ rbergen_8of30                                         │  2718   │ 5.00200  │    1    │   wheel   │   yes    │ 1    │   543.38265   │
│  86   │ assemblyscript │ 1        │ donmahallem                                           │  4648   │ 10.00100 │    1    │   base    │   yes    │      │   464.75351   │
│  87   │ python         │ 2        │ ssovest                                               │  2246   │ 5.00021  │    1    │   base    │   yes    │ 8    │   449.18079   │
│  88   │ fsharp         │ 1        │ rbergen                                               │  1827   │ 5.00017  │    1    │   base    │   yes    │ 1    │   365.38769   │
│  89   │ basic          │ 2        │ rbergen_vb                                            │  1776   │ 5.00157  │    1    │   base    │   yes    │ 1    │   355.08869   │
│  90   │ csharp         │ 1        │ kinematics_standard                                   │  1764   │ 5.00162  │    1    │   base    │   yes    │ 1    │   352.68573   │
│  91   │ csharp         │ 1        │ kinematics_original                                   │  1671   │ 5.00041  │    1    │   base    │   yes    │ 1    │   334.17260   │
│  92   │ octave         │ 1        │ octave                                                │  1648   │ 5.00314  │    1    │   base    │    no    │      │   329.39314   │
│  93   │ csharp         │ 2        │ davepl                                                │  3120   │ 10.00150 │    1    │   base    │   yes    │ 1    │   311.95321   │
│  94   │ scala          │ 1        │ rom1dep                                               │  1385   │ 5.00200  │    1    │   base    │   yes    │      │   276.88924   │
│  95   │ pony           │ 1        │ marghidanu                                            │  1379   │ 5.00000  │    1    │   base    │   yes    │ 1    │   275.80000   │
│  96   │ pascal         │ 1        │ rbergen                                               │  1287   │ 5.00000  │    1    │   base    │   yes    │      │   257.40000   │
│  97   │ cobol          │ 1        │ fvbakel_Cobol                                         │  1260   │ 5.00000  │    1    │   base    │    no    │ 8    │   252.00000   │
│  98   │ swift          │ 1        │ j-f1                                                  │  2220   │ 10.00386 │    1    │   base    │   yes    │      │   221.91430   │
│  99   │ haxe           │ 1        │ TayIorRobinson_Haxe_C++                               │  1540   │ 10.00012 │    1    │   base    │   yes    │      │   153.99815   │
│  100  │ basic          │ 1        │ rbergen_boolean                                       │   746   │ 5.00300  │    1    │   base    │   yes    │      │   149.11053   │
│  101  │ ocaml          │ 2        │ gkpotter-unfaithful                                   │   693   │ 5.00226  │    1    │   base    │    no    │      │   138.53738   │
│  102  │ r              │ 1        │ fvbakel_R                                             │   658   │ 5.00200  │    1    │   base    │   yes    │ 32   │   131.54738   │
│  103  │ ocaml          │ 1        │ gkpotter-faithful                                     │   633   │ 5.00206  │    1    │   base    │   yes    │      │   126.54774   │
│  104  │ julia          │ 1        │ dcbi                                                  │   539   │ 5.00360  │    1    │   base    │   yes    │ 1    │   107.72236   │
│  105  │ basic          │ 1        │ rbergen_bit64                                         │   472   │ 5.01000  │    1    │   base    │   yes    │ 1    │   94.21158    │
│  106  │ basic          │ 1        │ rbergen_bit32                                         │   469   │ 5.01000  │    1    │   base    │   yes    │ 1    │   93.61277    │
│  107  │ postscript     │ 1        │ epithet-ps                                            │   238   │ 5.01900  │    1    │   base    │    no    │ 8    │   47.41980    │
│  108  │ ruby           │ 1        │ rbergen                                               │   138   │ 5.01900  │    1    │   base    │   yes    │      │   27.49552    │
│  109  │ wren           │ 1        │ marghidanu                                            │   124   │ 5.03764  │    1    │   base    │   yes    │      │   24.61469    │
│  110  │ php            │ 1        │ DennisdeBest                                          │   167   │ 10.01142 │    1    │   base    │   yes    │      │   16.68095    │
│  111  │ smalltalk      │ 1        │ fvbakel_smalltalk                                     │   53    │ 5.00800  │    1    │   base    │   yes    │ 1    │   10.58307    │
│  112  │ lisp           │ 1        │ mikehw                                                │   84    │ 10.12387 │    1    │   base    │    no    │ 1    │    8.29722    │
│  113  │ perl           │ 1        │ marghidanu                                            │   37    │ 5.11649  │    1    │   base    │   yes    │      │    7.23153    │
│  114  │ mixal          │ 1        │ rbergen                                               │   30    │ 4.23000  │    1    │   base    │    no    │ 1    │    7.09220    │
│  115  │ haxe           │ 1        │ TayIorRobinson_Haxe_HaxeEval                          │   46    │ 10.15167 │    1    │   base    │   yes    │      │    4.53127    │
│  116  │ python         │ 1        │ davepl                                                │   43    │ 10.13962 │    1    │   base    │   yes    │      │    4.24079    │
│  117  │ sql            │ 2        │ fvbakel_MariaDB3                                      │    8    │ 5.67600  │    1    │   other   │    no    │ 32   │    1.40944    │
│  118  │ tcl            │ 1        │ fvbakeltcl                                            │    7    │ 5.81200  │    1    │   base    │   yes    │ 1    │    1.20440    │
│  119  │ sql            │ 1        │ fvbakel_sqlite                                        │    6    │ 5.29745  │    1    │   other   │    no    │ 8    │    1.13262    │
│  120  │ tcl            │ 2        │ fvbakel_ootcl                                         │    5    │ 5.35600  │    1    │   base    │   yes    │ 1    │    0.93353    │
│  121  │ powershell     │ 2        │ crowbar27_ps2                                         │    5    │ 5.51021  │    1    │   base    │   yes    │ 1    │    0.90741    │
│  122  │ sql            │ 2        │ fvbakel_MariaDB2                                      │    5    │ 5.79100  │    1    │   other   │    no    │ 32   │    0.86341    │
│  123  │ haxe           │ 1        │ TayIorRobinson_Haxe_Python                            │    9    │ 10.60204 │    1    │   base    │   yes    │      │    0.84889    │
│  124  │ sql            │ 2        │ fvbakel_MariaDB1                                      │    1    │ 6.27800  │    1    │   base    │    no    │ 32   │    0.15929    │
│  125  │ latex          │ 1        │ tjol                                                  │    2    │ 16.20195 │    1    │   base    │    no    │ 32   │    0.12344    │
│  126  │ prolog         │ 1        │ jimbxb_prolog                                         │    1    │ 8.19800  │    1    │   base    │   yes    │ 1    │    0.12198    │
│  127  │ bash           │ 1        │ bash                                                  │    1    │ 9.57403  │    1    │   base    │    no    │      │    0.10445    │
│  128  │ elixir         │ 1        │ cdesch                                                │    1    │ 16.07100 │    1    │   base    │    no    │      │    0.06222    │
│  129  │ lua            │ 1        │ lua                                                   │    1    │ 33.00000 │    1    │   base    │    no    │ 64   │    0.03030    │
│  130  │ powershell     │ 1        │ crowbar27_ps1                                         │    1    │ 56.67680 │    1    │   base    │   yes    │ 1    │    0.01764    │
└───────┴────────────────┴──────────┴───────────────────────────────────────────────────────┴─────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
                                                                                    Multi-threaded
┌───────┬────────────────┬──────────┬───────────────────────────────────────────────────────────────────┬──────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label                                                             │  Passes  │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼───────────────────────────────────────────────────────────────────┼──────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-clang                        │ 4924294  │ 5.00000  │    2    │   base    │    no    │ 1    │ 492429.40000  │
│   2   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-clang                        │ 9654723  │ 5.00000  │    4    │   base    │    no    │ 1    │ 482736.15000  │
│   3   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-clang                        │ 18443742 │ 5.00000  │    8    │   base    │    no    │ 1    │ 461093.55000  │
│   4   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-clang                        │ 31138373 │ 5.00000  │   16    │   base    │    no    │ 1    │ 389229.66250  │
│   5   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-gcc                          │ 4713138  │ 5.00000  │    4    │   base    │    no    │ 1    │ 235656.90000  │
│   6   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-gcc                          │ 2343026  │ 5.00000  │    2    │   base    │    no    │ 1    │ 234302.60000  │
│   7   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-gcc                          │ 9251515  │ 5.00000  │    8    │   base    │    no    │ 1    │ 231287.87500  │
│   8   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-clang                        │ 33543932 │ 5.00006  │   32    │   base    │    no    │ 1    │ 209647.05924  │
│   9   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-gcc                          │ 15422262 │ 5.00000  │   16    │   base    │    no    │ 1    │ 192778.27500  │
│  10   │ cpp            │ 4        │ BlackMark-pregenerated-inv_bits<u32>-gcc                          │ 15985149 │ 5.00004  │   32    │   base    │    no    │ 1    │  99906.38200  │
│  11   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-gcc                     │  233511  │ 5.00011  │    8    │   wheel   │   yes    │ 8    │  5837.64657   │
│  12   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-gcc                     │  116056  │ 5.00015  │    4    │   wheel   │   yes    │ 8    │  5802.62592   │
│  13   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-gcc                     │  57627   │ 5.00014  │    2    │   wheel   │   yes    │ 8    │  5762.53865   │
│  14   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-clang                   │  56765   │ 5.00017  │    2    │   wheel   │   yes    │ 1    │  5676.30701   │
│  15   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-gcc                       │  112076  │ 5.00014  │    4    │   wheel   │   yes    │ 8    │  5603.64310   │
│  16   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-clang                   │  111818  │ 5.00017  │    4    │   wheel   │   yes    │ 1    │  5590.70992   │
│  17   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-clang                   │  221360  │ 5.00018  │    8    │   wheel   │   yes    │ 1    │  5533.80078   │
│  18   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-gcc                   │  55290   │ 5.00013  │    2    │   wheel   │   yes    │ 1    │  5528.85625   │
│  19   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-gcc                       │  55279   │ 5.00011  │    2    │   wheel   │   yes    │ 8    │  5527.77839   │
│  20   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-clang                 │  54975   │ 5.00016  │    2    │   wheel   │   yes    │ 1    │  5497.32409   │
│  21   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-gcc                   │  109202  │ 5.00012  │    4    │   wheel   │   yes    │ 1    │  5459.96896   │
│  22   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-gcc                       │  218332  │ 5.00019  │    8    │   wheel   │   yes    │ 8    │  5458.09259   │
│  23   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-clang                 │  108065  │ 5.00009  │    4    │   wheel   │   yes    │ 1    │  5403.15274   │
│  24   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-gcc                   │  214675  │ 5.00018  │    8    │   wheel   │   yes    │ 1    │  5366.68180   │
│  25   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-clang                 │  213731  │ 5.00024  │    8    │   wheel   │   yes    │ 1    │  5343.01854   │
│  26   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-clang                   │  414190  │ 5.00018  │   16    │   wheel   │   yes    │ 1    │  5177.18862   │
│  27   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-gcc                     │  51029   │ 5.00013  │    2    │   wheel   │   yes    │ 1    │  5102.76733   │
│  28   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-gcc                     │  101583  │ 5.00012  │    4    │   wheel   │   yes    │ 1    │  5079.02810   │
│  29   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-gcc                     │  200108  │ 5.00016  │    8    │   wheel   │   yes    │ 1    │  5002.53992   │
│  30   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-gcc                     │  398405  │ 5.00021  │   16    │   wheel   │   yes    │ 8    │  4979.85335   │
│  31   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-gcc                   │  381136  │ 5.00027  │   16    │   wheel   │   yes    │ 1    │  4763.94275   │
│  32   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-clang                 │  376149  │ 5.00024  │   16    │   wheel   │   yes    │ 1    │  4701.63682   │
│  33   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-gcc                       │  368834  │ 5.00032  │   16    │   wheel   │   yes    │ 8    │  4610.12995   │
│  34   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-gcc                     │  358043  │ 5.00022  │   16    │   wheel   │   yes    │ 1    │  4475.34059   │
│  35   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-gcc                              │  35903   │ 5.00025  │    2    │   base    │   yes    │ 8    │  3590.12049   │
│  36   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-gcc                              │  69418   │ 5.00023  │    4    │   base    │   yes    │ 8    │  3470.74035   │
│  37   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-gcc                              │  138706  │ 5.00026  │    8    │   base    │   yes    │ 8    │  3467.46969   │
│  38   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-clang                   │  523126  │ 5.00027  │   32    │   wheel   │   yes    │ 1    │  3269.36095   │
│  39   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-gcc                   │  494206  │ 5.00027  │   32    │   wheel   │   yes    │ 1    │  3088.62071   │
│  40   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-gcc                              │  244088  │ 5.00033  │   16    │   base    │   yes    │ 8    │  3050.89864   │
│  41   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_bits<u32>-gcc                     │  459783  │ 5.00030  │   32    │   wheel   │   yes    │ 1    │  2873.47134   │
│  42   │ zig            │ 3        │ ManDeJan&ityonemo-zig-parallel-gustafson-bitSieve-u8-5760of30030  │  437994  │ 5.00018  │   32    │   wheel   │   yes    │ 1    │  2737.36395   │
│  43   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_bits<u32>-clang                 │  432026  │ 5.00087  │   32    │   wheel   │   yes    │ 1    │  2699.69275   │
│  44   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-clang                          │  25915   │ 5.00014  │    2    │   base    │   yes    │ 1    │  2591.42744   │
│  45   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-clang                          │  51592   │ 5.00027  │    4    │   base    │   yes    │ 1    │  2579.46071   │
│  46   │ zig            │ 3        │ ManDeJan&ityonemo-zig-parallel-gustafson-bitSieve-u8-480of2310    │  405374  │ 5.00004  │   32    │   wheel   │   yes    │ 1    │  2533.56723   │
│  47   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-clang                          │  101036  │ 5.00039  │    8    │   base    │   yes    │ 1    │  2525.70300   │
│  48   │ zig            │ 3        │ ManDeJan&ityonemo-zig-parallel-gustafson-bitSieve-u64-480of2310   │  397458  │ 5.00028  │   32    │   wheel   │   yes    │ 1    │  2483.97340   │
│  49   │ zig            │ 3        │ ManDeJan&ityonemo-zig-parallel-gustafson-bitSieve-u64-5760of30030 │  382846  │ 5.00016  │   32    │   wheel   │   yes    │ 1    │  2392.71093   │
│  50   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-clang                          │  188187  │ 5.00050  │   16    │   base    │   yes    │ 1    │  2352.10229   │
│  51   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-gcc                            │  46574   │ 5.00038  │    4    │   base    │   yes    │ 1    │  2328.52303   │
│  52   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-gcc                            │  23252   │ 5.00020  │    2    │   base    │   yes    │ 1    │  2325.10700   │
│  53   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-gcc                            │  91850   │ 5.00040  │    8    │   base    │   yes    │ 1    │  2296.06631   │
│  54   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-gcc                     │  338106  │ 5.00035  │   32    │   wheel   │   yes    │ 8    │  2113.01459   │
│  55   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-gcc                       │  329255  │ 5.00032  │   32    │   wheel   │   yes    │ 8    │  2057.71206   │
│  56   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-gcc                            │  151139  │ 5.00056  │   16    │   base    │   yes    │ 1    │  1889.02593   │
│  57   │ rust           │ 1        │ mike-barber_byte-storage                                          │  243922  │ 5.00073  │   32    │   base    │   yes    │ 8    │  1524.28936   │
│  58   │ zig            │ 3        │ ManDeJan&ityonemo-zig-parallel-gustafson-sieve-u8                 │  243390  │ 5.00020  │   32    │   base    │   yes    │ 8    │  1521.12665   │
│  59   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-clang                          │  239548  │ 5.00326  │   32    │   base    │   yes    │ 1    │  1496.19948   │
│  60   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-gcc                              │  235340  │ 5.00064  │   32    │   base    │   yes    │ 8    │  1470.68675   │
│  61   │ rust           │ 1        │ mike-barber_bit-storage-rotate                                    │  225536  │ 5.00083  │   32    │   base    │   yes    │ 1    │  1409.36667   │
│  62   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-clang                   │  13828   │ 5.00040  │    2    │   wheel   │   yes    │ 8    │  1382.68938   │
│  63   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-clang                   │  27315   │ 5.00055  │    4    │   wheel   │   yes    │ 8    │  1365.59978   │
│  64   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-clang                   │  54067   │ 5.00064  │    8    │   wheel   │   yes    │ 8    │  1351.50201   │
│  65   │ rust           │ 1        │ mike-barber_bit-storage                                           │  209107  │ 5.00077  │   32    │   base    │   yes    │ 1    │  1306.71836   │
│  66   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-clang                     │  12911   │ 5.00069  │    2    │   wheel   │   yes    │ 8    │  1290.92185   │
│  67   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-clang                     │  25592   │ 5.00060  │    4    │   wheel   │   yes    │ 8    │  1279.44647   │
│  68   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-clang                     │  50933   │ 5.00057  │    8    │   wheel   │   yes    │ 8    │  1273.17986   │
│  69   │ cpp            │ 4        │ BlackMark-1of2-os-fs-inv_bits<u32>-gcc                            │  202850  │ 5.00358  │   32    │   base    │   yes    │ 1    │  1266.90540   │
│  70   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-clang                     │  96997   │ 5.00079  │   16    │   wheel   │   yes    │ 8    │  1212.27096   │
│  71   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-clang                   │  96164   │ 5.00080  │   16    │   wheel   │   yes    │ 8    │  1201.85770   │
│  72   │ zig            │ 3        │ ManDeJan&ityonemo-zig-parallel-gustafson-bitSieve-u8              │  190154  │ 5.00000  │   32    │   base    │   yes    │ 1    │  1188.46250   │
│  73   │ zig            │ 3        │ ManDeJan&ityonemo-zig-parallel-gustafson-bitSieve-u64             │  176849  │ 5.00079  │   32    │   base    │   yes    │ 1    │  1105.13164   │
│  74   │ cpp            │ 4        │ BlackMark-92160of510510-os-hs-inv_vec<u8>-clang                   │  151909  │ 5.00101  │   32    │   wheel   │   yes    │ 8    │   949.23950   │
│  75   │ cpp            │ 4        │ BlackMark-5760of30030-os-hs-inv_vec<u8>-clang                     │  151153  │ 5.00223  │   32    │   wheel   │   yes    │ 8    │   944.28510   │
│  76   │ cpp            │ 3        │ flo80_constexpr                                                   │  138560  │ 5.00107  │   32    │   base    │    no    │ 1    │   865.81420   │
│  77   │ c              │ 2        │ danielspaangberg_480of2310_par                                    │  15816   │ 5.00009  │    4    │   wheel   │   yes    │ 1    │   790.78592   │
│  78   │ c              │ 2        │ danielspaangberg_5760of30030_par                                  │  15814   │ 5.00029  │    4    │   wheel   │   yes    │ 1    │   790.65446   │
│  79   │ c              │ 2        │ danielspaangberg_48of210_par                                      │  14884   │ 5.00027  │    4    │   wheel   │   yes    │ 1    │   744.16011   │
│  80   │ c              │ 2        │ danielspaangberg_8of30_par                                        │  14389   │ 5.00009  │    4    │   wheel   │   yes    │ 1    │   719.43777   │
│  81   │ c              │ 2        │ danielspaangberg_5760of30030_epar                                 │  114745  │ 5.01560  │   32    │   wheel   │   yes    │ 1    │   714.92625   │
│  82   │ c              │ 2        │ danielspaangberg_480of2310_epar                                   │  106095  │ 5.01535  │   32    │   wheel   │   yes    │ 1    │   661.06376   │
│  83   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-clang                            │   6591   │ 5.00150  │    2    │   base    │   yes    │ 8    │   658.90233   │
│  84   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-clang                            │  13042   │ 5.00133  │    4    │   base    │   yes    │ 8    │   651.92659   │
│  85   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-clang                            │  25891   │ 5.00153  │    8    │   base    │   yes    │ 8    │   647.07699   │
│  86   │ c              │ 2        │ danielspaangberg_1of2_par                                         │  12865   │ 5.00021  │    4    │   base    │   yes    │ 1    │   643.22311   │
│  87   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-clang                            │  49782   │ 5.00115  │   16    │   base    │   yes    │ 8    │   622.13191   │
│  88   │ c              │ 2        │ danielspaangberg_48of210_epar                                     │  97390   │ 5.01500  │   32    │   wheel   │   yes    │ 1    │   606.86702   │
│  89   │ cpp            │ 4        │ BlackMark-1of2-cs-hs-inv_vec<u8>-clang                            │  87635   │ 5.00233  │   32    │   base    │   yes    │ 8    │   547.46363   │
│  90   │ c              │ 2        │ danielspaangberg_8of30_epar                                       │  81912   │ 5.01553  │   32    │   wheel   │   yes    │ 1    │   510.36450   │
│  91   │ c              │ 2        │ danielspaangberg_1of2_epar                                        │  77069   │ 5.01826  │   32    │   base    │   yes    │ 1    │   479.92846   │
│  92   │ cpp            │ 2        │ davepl_par                                                        │  61440   │ 5.00047  │   32    │   base    │   yes    │ 1    │   383.96391   │
│  93   │ d              │ 2        │ BradleyChatha-Multi                                               │  56385   │ 5.00163  │   32    │   base    │   yes    │ 1    │   352.29140   │
│  94   │ zig            │ 3        │ ManDeJan&ityonemo-zig-parallel-amdahl-sieve-u8                    │   6015   │ 5.00011  │   32    │   base    │   yes    │ 8    │   37.59292    │
│  95   │ csharp         │ 1        │ kinematics_pool6p                                                 │   2793   │ 5.00100  │   32    │   wheel   │   yes    │ 1    │   17.45276    │
│  96   │ csharp         │ 1        │ kinematics_rawp                                                   │   2759   │ 5.00065  │   32    │   base    │   yes    │ 1    │   17.24151    │
└───────┴────────────────┴──────────┴───────────────────────────────────────────────────────────────────┴──────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
```

</details>
