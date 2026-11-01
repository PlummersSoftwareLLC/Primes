# C language solution by Daniel Spångberg

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

All these implementations are written in C.
The state of the sieve is stored in a C struct (closest to a class in C). 

There are different optimizations:
1of2 - check only odd integers
8of30 - check only integers not divisible by 2, 3, or 5
48of210 - check only integers not divisible by 2, 3, 5, or 7
480of2310 - check only integers not divisible by 2, 3, 5, 7, or 11
5760of30030 - check only integers not divisible by 2, 3, 5, 7, 11, or 13

Only bits corresponding to odd integers are stored.
The inner loop masking is handled in three different ways:
(1) Write each bit corresponding to the multiple of the found prime
(2) Only write every multiple that will be read by the outer loop bit checking (i.e. only write bits corresponding to integers not divisible by 2, 3, or 5 for the 8of30 algorithm.
(3) Write each bit corresponding to the multiple of the found prime in parallel on each thread, taking care to never touch the same word in two different threads. The implementation uses OpenMP (This is the _par versions)

Also, running the sieves in parallel in different threads in an embarrassingly parallel style is tested, also using OpenMP (The _epar versions)


1of2 - Implementation close to the original implementation of Dave.

8of30 - Implementation writing bits in style (1) above

8of30_owrb - Implementation writing bits in style (2)

48of210 - Implementation writing bits in style (1)

48of210_owrb - Implementation writing bits in style (2)

480of2310_owrb - Implementation writing bits in style (2)

5760of30030_owrb - Implementation writing bits in style (2)

1of2_par - Implementation writing bits in style (3)

8of30_par - Implementation writing bits in style (3)

48of210_par - Implementation writing bits in style (3)

480of2310_par - Implementation writing bits in style (3)

5760of30030_par - Implementation writing bits in style (3)

1of2_epar - Implementation writing bits in style (1), embarrassingly parallel

8of30_epar - Implementation writing bits in style (2), embarrassingly parallel

48of210_epar - Implementation writing bits in style (2), embarrassingly parallel

480of2310_epar - Implementation writing bits in style (2), embarrassingly parallel

5760of30030_epar - Implementation writing bits in style (2), embarrassingly parallel

8of30_msubra_extend - Contributed by msubra. An 8-of-30 wheel implementation
with two additional optimizations over the standard 8-of-30 approach:

1. **Extended pre-sieve**: composites of 7, 11, 13, and 17 are pre-computed
   as a 17017-byte template (lcm(7,11,13,17)) built inductively. For N=1,000,000
   this reduces tiling to 2 memcpy calls and eliminates four marking passes per
   iteration. The presieve is built as part of each sieve instance (inside the
   timed loop) so each pass owns its own complete state.

2. **Wide-tile vectorization**: for primes with stride ≤ 127, multiples are
   marked by building a stride-byte tile, expanding it 16× into a wide tile
   via O(log 16) doubling memcpy calls, then ORing across the sieve with
   Clang auto-vectorized inner loops (NEON on arm64, AVX2 on x86-64).
   Larger primes (stride > 127) use 8-deep strided scatter, which is faster
   at those strides where the wide-tile scan cost exceeds scatter's L2 cost.
   The stride is a runtime value — no hardcoded prime enumeration.

Compiled with GCC (uses `#pragma GCC ivdep` for auto-vectorization; Clang uses `clang loop vectorize(enable)` via the same `VECTORIZE_HINT` macro). Command-line
interface, `custom_atoll`, oneshot mode, and `benchmark.sh` pattern adapted
from PrimeCPP/solution_5.

## Run instructions
You can use the Dockerfile - but the code (especially the parallel implementations) runs (much) slower in docker.

First compile the code using the compile.sh script. Optimization parameters are at the beginning of the script. They are set up to use the gcc compiler. The msubra variant is compiled separately with clang (both gcc and clang are installed in the Docker image).

Then run the code using the run.sh script. (You can change the number of threads in the script, the default runs up to 4 threads on the parallel bit masking and the maximum number of cpus on the embarrassingly parallel implementation).

## Output

On i5-9400 (WSL2, no docker):
```
danielspaangberg_1of2;10000;5.000305;1;algorithm=base,faithful=yes,bits=1
danielspaangberg_8of30;15852;5.000120;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_8of30_owrb;17487;5.000032;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210;18425;5.000031;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210_owrb;23344;5.000077;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_480of2310_owrb;27994;5.000026;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_5760of30030_owrb;32872;5.000085;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_1of2_par;20197;5.000203;4;algorithm=base,faithful=yes,bits=1
danielspaangberg_8of30_par;24297;5.000176;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210_par;27088;5.000170;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_480of2310_par;28164;5.000065;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_5760of30030_par;29089;5.000059;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_1of2_epar;54939;5.000779;6;algorithm=base,faithful=yes,bits=1
danielspaangberg_8of30_epar;99934;5.000653;6;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210_epar;132728;5.001278;6;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_480of2310_epar;159456;5.000567;6;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_5760of30030_epar;187042;5.002346;6;algorithm=wheel,faithful=yes,bits=1
```

On i5-9400 (Docker / WSL2):
```
danielspaangberg_1of2;9692;5.000474;1;algorithm=base,faithful=yes,bits=1
danielspaangberg_8of30;14777;5.000164;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_8of30_owrb;9639;5.000136;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210;17084;5.000198;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210_owrb;12707;5.000190;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_480of2310_owrb;15219;5.000246;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_5760of30030_owrb;17836;5.000029;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_1of2_par;9024;5.000498;4;algorithm=base,faithful=yes,bits=1
danielspaangberg_8of30_par;10109;5.000448;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210_par;10598;5.000019;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_480of2310_par;10817;5.000096;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_5760of30030_par;11009;5.000264;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_1of2_epar;54120;5.002285;6;algorithm=base,faithful=yes,bits=1
danielspaangberg_8of30_epar;53375;5.002453;6;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210_epar;69328;5.010508;6;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_480of2310_epar;81199;5.003292;6;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_5760of30030_epar;90976;5.002068;6;algorithm=wheel,faithful=yes,bits=1
```

On Raspberry Pi 4b (2GHz, ARM64):
```
danielspaangberg_1of2;3150;5.000062;1;algorithm=base,faithful=yes,bits=1
danielspaangberg_8of30;4711;5.000430;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_8of30_owrb;6348;5.000481;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210;5550;5.000757;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210_owrb;7761;5.000091;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_480of2310_owrb;9065;5.000311;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_5760of30030_owrb;10191;5.000020;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_1of2_par;6907;5.000622;4;algorithm=base,faithful=yes,bits=1
danielspaangberg_8of30_par;7629;5.000126;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210_par;8654;5.000219;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_480of2310_par;9420;5.000243;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_5760of30030_par;9434;5.000104;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_1of2_epar;10524;5.002370;4;algorithm=base,faithful=yes,bits=1
danielspaangberg_8of30_epar;16896;5.001638;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210_epar;19032;5.001211;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_480of2310_epar;19070;5.001330;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_5760of30030_epar;19314;5.001127;4;algorithm=wheel,faithful=yes,bits=1
```

### msubra_8of30_extend — alternating runs vs all 8-of-30 entries

All variants compiled with `clang -Ofast -march=native` (existing entries) and
`clang -O3 -ffast-math -march=native` (msubra_8of30_extend). On macOS, `gcc`
is Apple Clang 16; Clang is used directly here for clarity.

---

#### macOS — Intel i9-9980HK @ 2.40 GHz, Coffee Lake, AVX2, 16 logical / 8 physical cores, GCC = Apple Clang 16

`danielspaangberg_8of30_par` run with 4 threads; `_epar` and msubra multi-threaded run with 16 threads.

**Run 1**
```
danielspaangberg_8of30;10849;5.000349;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_8of30_owrb;9900;5.000216;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_8of30_par;19319;5.000099;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_8of30_epar;90773;5.003841;16;algorithm=wheel,faithful=yes,bits=1
msubra_8of30_extend;74765;5.00795;1;algorithm=wheel,faithful=yes,bits=1
msubra_8of30_extend;502414;5.00815;16;algorithm=wheel,faithful=yes,bits=1
```

**Run 2**
```
danielspaangberg_8of30;10848;5.000449;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_8of30_owrb;9907;5.000390;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_8of30_par;19426;5.000116;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_8of30_epar;89962;5.003295;16;algorithm=wheel,faithful=yes,bits=1
msubra_8of30_extend;74647;5.00313;1;algorithm=wheel,faithful=yes,bits=1
msubra_8of30_extend;500648;5.00978;16;algorithm=wheel,faithful=yes,bits=1
```

**Run 3**
```
danielspaangberg_8of30;10773;5.000259;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_8of30_owrb;9880;5.000394;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_8of30_par;19338;5.000143;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_8of30_epar;90398;5.003578;16;algorithm=wheel,faithful=yes,bits=1
msubra_8of30_extend;74371;5.00598;1;algorithm=wheel,faithful=yes,bits=1
msubra_8of30_extend;501622;5.00157;16;algorithm=wheel,faithful=yes,bits=1
```

Single-threaded: msubra_8of30_extend averages 74,594 passes vs 10,823 for
danielspaangberg_8of30 (**+589%**) and 9,896 for danielspaangberg_8of30_owrb (**+654%**).

Multi-threaded at 16 threads: msubra_8of30_extend averages 501,561 passes vs 90,378
for danielspaangberg_8of30_epar (**+455%**).

---

#### Linux — Intel Xeon X5650 @ 2.67 GHz, Westmere, SSE4.2 only, 24 logical / 6 physical cores, GCC 10.2.1

`danielspaangberg_8of30_par` run with 4 threads; `_epar` and msubra multi-threaded run with 24 threads.

```
danielspaangberg_8of30;8684;5.000356;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_8of30_owrb;8431;5.000151;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_8of30_par;12788;5.000079;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_8of30_epar;88134;5.003377;24;algorithm=wheel,faithful=yes,bits=1
msubra_8of30_extend;32874;5.00036;1;algorithm=wheel,faithful=yes,bits=1
msubra_8of30_extend;356427;5.00062;24;algorithm=wheel,faithful=yes,bits=1
```

Single-threaded: msubra_8of30_extend scores 32,874 passes vs 8,684 for
danielspaangberg_8of30 (**+278%**) and 8,431 for danielspaangberg_8of30_owrb (**+290%**).

Multi-threaded at 24 threads: msubra_8of30_extend scores 356,427 passes vs 88,134
for danielspaangberg_8of30_epar (**+304%**).

The smaller gain on Linux is expected: the Xeon X5650 (Westmere, 2010) supports only
SSE4.2 (128-bit SIMD). The wide-tile OR loop that drives msubra's speed processes 256
bits per cycle on AVX2 (macOS) vs 128 bits on SSE4.2 (gamma), halving the vectorization
benefit. The algorithmic improvement is consistent across both platforms.
