# Ada solution by Lucretia

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-green)

<!-- Using the Community IDE and Compiler found :
https://www.adacore.com/download -->

This is my attempt at this program, I have not looked at the original Ada version, I have only run it a number of times, which gave [these results](./my_results.md#PrimeAda_solution_1) on my FX-8350 machine.

My aim with this is to compare against the original [C++](./my_results.md#PrimeCPP_solution_1), [C](./my_results.md#PrimeC_solution_1) and [Pascal](./my_results.md#PrimePascal_solution_1) versions.

* Ada 2012 Compiler required
* Tested with FSF GCC 9.3.0

## Run instructions

### The bits program

This just shows what the compiler generates for the sizes of various types used in this implementation, also as a means to give the correct values for the results. It is built along with the sieve program.

### Without Docker

```bash
cd PrimeAda/solution_2
gprbuild -p -XMODE=optimised
./prime_sieve
```

See the TODO inside [sieves.adb](./src/sieves.adb) as I think I converted it correctly.

## Output

### Build configuration: debug0

```bash
Passes: 413, Time:  5.001606, Avg:  0.012110426, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;413; 5.001606;algorithm=base,faithful=yes,bits=1

# Generic
Passes: 325, Time:  5.013335, Avg:  0.015425646, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;325; 5.013335;algorithm=base,faithful=yes,bits=1

# Generic Generator
Passes: 320, Time:  5.000179, Avg:  0.015625559, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;320; 5.000179;algorithm=base,faithful=yes,bits=1
Passes: 792, Time:  5.004842, Avg:  0.006319244, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;792; 5.004842;algorithm=base,faithful=yes,bits=8
```

### Build configuration: debug1

```bash
Passes: 1379, Time:  5.003487, Avg:  0.003628344, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1379; 5.003487;algorithm=base,faithful=yes,bits=1

# Generic
Passes: 1165, Time:  5.001849, Avg:  0.004293432, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1165; 5.001849;algorithm=base,faithful=yes,bits=1

# Generic Generator
Passes: 1251, Time:  5.000866, Avg:  0.003997494, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1251; 5.000866;algorithm=base,faithful=yes,bits=1
Passes: 2503, Time:  5.000539, Avg:  0.001997818, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;2503; 5.000539;algorithm=base,faithful=yes,bits=8
```

### Build configuration: debug2

```bash
Passes: 1732, Time:  5.000402, Avg:  0.002887068, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1732; 5.000402;algorithm=base,faithful=yes,bits=1

# Generic
Passes: 1604, Time:  5.001909, Avg:  0.003118397, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1604; 5.001909;algorithm=base,faithful=yes,bits=1

# Generic Generator
Passes: 1739, Time:  5.000676, Avg:  0.002875604, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1739; 5.000676;algorithm=base,faithful=yes,bits=1
Passes: 2499, Time:  5.000513, Avg:  0.002001005, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;2499; 5.000513;algorithm=base,faithful=yes,bits=8
```

### Build configuration: debug3

```bash
Passes: 1741, Time:  5.001630, Avg:  0.002872848, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1741; 5.001630;algorithm=base,faithful=yes,bits=1

# Generic
Passes: 1589, Time:  5.000123, Avg:  0.003146710, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1589; 5.000123;algorithm=base,faithful=yes,bits=1

# Generic Generator
Passes: 1737, Time:  5.001220, Avg:  0.002879228, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1737; 5.001220;algorithm=base,faithful=yes,bits=1
Passes: 3218, Time:  5.001153, Avg:  0.001554118, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;3218; 5.001153;algorithm=base,faithful=yes,bits=8
```

### Build configuration: release

```bash
Passes: 1748, Time:  5.000009, Avg:  0.002860417, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1748; 5.000009;algorithm=base,faithful=yes,bits=1

# Generic
Passes: 1590, Time:  5.000290, Avg:  0.003144836, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1590; 5.000290;algorithm=base,faithful=yes,bits=1

# Generic Generator
Passes: 1737, Time:  5.001094, Avg:  0.002879156, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1737; 5.001094;algorithm=base,faithful=yes,bits=1
Passes: 3203, Time:  5.000005, Avg:  0.001561038, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;3203; 5.000005;algorithm=base,faithful=yes,bits=8
```

### Build configuration: optimised

```bash
Passes: 1820, Time:  5.000495, Avg:  0.002747524, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1820; 5.000495;algorithm=base,faithful=yes,bits=1

# Generic
Passes: 1838, Time:  5.001230, Avg:  0.002721017, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1838; 5.001230;algorithm=base,faithful=yes,bits=1

# Generic Generator
Passes: 1813, Time:  5.000515, Avg:  0.002758143, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1813; 5.000515;algorithm=base,faithful=yes,bits=1
Passes: 3331, Time:  5.001093, Avg:  0.001501378, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;3331; 5.001093;algorithm=base,faithful=yes,bits=8

## Supress all checks - 5 successive runs, very odd.
Passes: 1810, Time:  5.000556, Avg:  0.002762738, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1810; 5.000556;algorithm=base,faithful=yes,bits=1
Passes: 3650, Time:  5.000006, Avg:  0.001369864, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;3650; 5.000006;algorithm=base,faithful=yes,bits=8

Passes: 1816, Time:  5.001252, Avg:  0.002753993, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1816; 5.001252;algorithm=base,faithful=yes,bits=1
Passes: 3592, Time:  5.000154, Avg:  0.001392025, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;3592; 5.000154;algorithm=base,faithful=yes,bits=8

Passes: 1827, Time:  5.000615, Avg:  0.002737063, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1827; 5.000615;algorithm=base,faithful=yes,bits=1
Passes: 3650, Time:  5.000388, Avg:  0.001369969, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;3650; 5.000388;algorithm=base,faithful=yes,bits=8

Passes: 1830, Time:  5.002268, Avg:  0.002733479, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1830; 5.002268;algorithm=base,faithful=yes,bits=1
Passes: 3653, Time:  5.001124, Avg:  0.001369045, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;3653; 5.001124;algorithm=base,faithful=yes,bits=8

Passes: 1826, Time:  5.000397, Avg:  0.002738443, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1826; 5.000397;algorithm=base,faithful=yes,bits=1
Passes: 3652, Time:  5.000872, Avg:  0.001369351, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;3652; 5.000872;algorithm=base,faithful=yes,bits=8
```
