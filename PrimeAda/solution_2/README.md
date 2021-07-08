# Ada solution by Lucretia

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

<!-- Using the Community IDE and Compiler found :
https://www.adacore.com/download -->

*Give a short description of your implementation*

This is my attempt at this program, I have not looked at the original Ada version, I have only run it a number of times, which gave [these results](./my_results.md#PrimeAda_solution_1) on my FX-8350 machine.

My aim with this is to compare against the original [C++](./my_results.md#PrimeCPP_solution_1), [C](./my_results.md#PrimeC_solution_1) and [Pascal](./my_results.md#PrimePascal_solution_1) versions.

* Ada 2012 Compiler required
* Tested with FSF GCC 9.3.0

## Run instructions

*Describe how to run your application here. If build steps are required to make the solution runnable, include those too.*

### The bits program

This just shows what the compiler generates for the sizes of various types used in this implementation, also as a means to give the correct values for the results. It is built along with the sieve program.

### Without Docker

```bash
cd PrimeAda/solution_2
gprbuild -p -XMODE=speed
./prime_sieve
```

## Output

*Show the output you got on your machine(s) here, in code blocks*
