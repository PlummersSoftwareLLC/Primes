# Clojure solution 2 by Alex Vear (axvr)

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

A very fast and faithful [Clojure](https://clojure.org/) implementation of
the [Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes)
algorithm.

This solution contains 2 variants of the same algorithm, an 8-bit and a 1-bit
variant.  The 8-bit variant is roughly twice as fast as the 1-bit variant.


## Run instructions

1. Install a JDK.
2. Install the [Clojure CLI tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools).
3. Run with `./run.sh`

(Warm-up is enabled by default for the 8-bit variant.)


## Output

```
Passes: 6659, Time: 5.000674963, Avg: 7.5096486E-4, Limit: 1000000, Count: 78498, Valid: True
axvr_clj-sln-2_8-bit;6659;5.000674963;1;algorithm=base,faithful=yes,bits=8
Passes: 3127, Time: 5.000791197, Avg: 0.0015992296, Limit: 1000000, Count: 78498, Valid: True
axvr_clj-sln-2_1-bit;3127;5.000791197;1;algorithm=base,faithful=yes,bits=1
```

(On an Intel Core i7-8550U @1.80GHz x8 CPU with 16GB of RAM.)
