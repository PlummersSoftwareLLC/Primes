# Clojure solution 2 by Alex Vear (axvr)

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

A very fast and faithful [Clojure](https://clojure.org/) implementation of
the [Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes)
algorithm.


## Run instructions

1. Install a JDK.
2. Install the [Clojure CLI tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools).
3. Run with `clojure -X:run` or `clojure -X:run :warm-up? true` to reduce
   variability of results.

(Warm-up is enabled by default on the Docker image.)


## Output

```
Passes: 6652, Time: 5.000029983, Avg: 7.5165817E-4, Limit: 1000000, Count: 78498, Valid: True
axvr-clj-sln-2;6652;5.000029983;1;algorithm=base,faithful=yes,bits=8
```

(On an Intel Core i7-8550U @1.80GHz x8 CPU with 16GB of RAM using warm-up.)
