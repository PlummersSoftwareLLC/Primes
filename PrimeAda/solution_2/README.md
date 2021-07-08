# Ada solution by Lucretia

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

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
```

### Build configuration: debug1

```bash
Passes: 1379, Time:  5.003487, Avg:  0.003628344, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1379; 5.003487;algorithm=base,faithful=yes,bits=1

# Generic
Passes: 1165, Time:  5.001849, Avg:  0.004293432, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1165; 5.001849;algorithm=base,faithful=yes,bits=1
```

### Build configuration: debug2

```bash
Passes: 1732, Time:  5.000402, Avg:  0.002887068, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1732; 5.000402;algorithm=base,faithful=yes,bits=1

# Generic
Passes: 1604, Time:  5.001909, Avg:  0.003118397, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1604; 5.001909;algorithm=base,faithful=yes,bits=1
```

### Build configuration: debug3

```bash
Passes: 1741, Time:  5.001630, Avg:  0.002872848, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1741; 5.001630;algorithm=base,faithful=yes,bits=1

# Generic
Passes: 1589, Time:  5.000123, Avg:  0.003146710, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1589; 5.000123;algorithm=base,faithful=yes,bits=1
```

### Build configuration: release

```bash
Passes: 1748, Time:  5.000009, Avg:  0.002860417, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1748; 5.000009;algorithm=base,faithful=yes,bits=1

# Generic
Passes: 1590, Time:  5.000290, Avg:  0.003144836, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1590; 5.000290;algorithm=base,faithful=yes,bits=1
```

### Build configuration: optimised

```bash
Passes: 1820, Time:  5.000495, Avg:  0.002747524, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1820; 5.000495;algorithm=base,faithful=yes,bits=1

# Generic
Passes: 1838, Time:  5.001230, Avg:  0.002721017, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: TRUE
Lucretia;1838; 5.001230;algorithm=base,faithful=yes,bits=1
```
