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

```bash
                                                               Single-threaded
┌───────┬────────────────┬──────────┬────────────────────────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label                      │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼────────────────────────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ ada            │ 2        │ Lucretia - Imperative      │  3474  │ 5.00042  │    1    │   base    │   yes    │ 8    │   694.74192   │
│   2   │ ada            │ 2        │ Lucretia - Object-Oriented │  2946  │ 5.00165  │    1    │   base    │   yes    │ 8    │   589.00575   │
│   3   │ ada            │ 2        │ Lucretia - Object-Oriented │  2505  │ 5.00080  │    1    │   base    │   yes    │ 1    │   500.91945   │
│   4   │ ada            │ 2        │ Lucretia - Imperative      │  2369  │ 5.00159  │    1    │   base    │   yes    │ 1    │   473.64947   │
└───────┴────────────────┴──────────┴────────────────────────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
```
