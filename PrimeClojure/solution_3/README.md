# Clojure solution 3 by Peter Strömberg (@PEZ)

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Some faithful [Clojure](https://clojure.org/) implementations of
the [Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes)
algorithm.

* `pez-clj-bitset` – a BitSet is sieved, all bits visited in one pass
* `pez-clj-bitset-pre` – a BitSet is first initialized so that all bits, but 0, 1 and a all even numbers > 2 are marked as primes, and then the sieve works on removing non-primes
* `pez-clj-boolean-array` – a boolean array is sieved, all indexes visited in one pass
* `pez-clj-boolean-array-pre` – a boolean array is first initialized so that all indexes, but 0, 1 and a all even numbers > 2 are marked as primes, and then the sieve works on removing non-primes
* `pez-clj-boolean-array-futures-pre` – same as `boolean-array-pre`, except the evens are cleared in a parallelized manner using [futures](https://clojure.org/about/concurrent_programming)
* `pez-clj-boolean-array-to-vector-futures` – a boolean array is first initialized so that all indexes, are marked as primes, then the sieve works on removing odd non-primes, then 0, 1 and all even numbers > 2 are removed. The last step uses [transients](https://clojure.org/reference/transients) and is also parallelized using `futures`.

## Run instructions

The runner infrastructure is built from Alex Vaer's [Clojure Solution 2](https://github.com/PlummersSoftwareLLC/Primes/tree/drag-race/PrimeClojure/solution_2).

1. Install a JDK.
2. Install the [Clojure CLI tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools).
3. Run with `clojure -X sieve/run :warm-up? true`

The warm-up makes the runner start with a silent run, before running one that is reported, giving slightly more consistent results. (But, really, you should use [Criterium](https://github.com/hugoduncan/criterium) for benchmarking while developing, see below.)

Running all three solutions via Docker:

```sh
$ docker pull clojure:openjdk-18-tools-deps-1.10.3.1040
$ docker build -t pez-primes-clojure .
$ docker run --rm -it pez-primes-clojure
```

## Sample output

```
Passes: 496, Time: 5.002424000, Avg: 0.010085532, Limit: 1000000, Count: 78498, Valid: True
pez-clj-bitset;496;5.002424000;1;algorithm=base,faithful=yes,bits=1
Passes: 1472, Time: 5.000757000, Avg: 0.0033972533, Limit: 1000000, Count: 78498, Valid: True
pez-clj-bitset-pre;1472;5.000757000;1;algorithm=base,faithful=yes,bits=1
Passes: 1246, Time: 5.004753000, Avg: 0.0040166555, Limit: 1000000, Count: 78498, Valid: True
pez-clj-boolean-array;1246;5.004753000;1;algorithm=base,faithful=yes,bits=8
Passes: 2560, Time: 5.000806000, Avg: 0.0019534398, Limit: 1000000, Count: 78498, Valid: True
pez-clj-boolean-array-pre;2560;5.000806000;1;algorithm=base,faithful=yes,bits=8
Passes: 3419, Time: 5.001685000, Avg: 0.0014629087, Limit: 1000000, Count: 78498, Valid: True
pez-clj-boolean-array-pre-futures;3419;5.001685000;8;algorithm=base,faithful=yes,bits=8
Passes: 2529, Time: 5.000828000, Avg: 0.0019773934, Limit: 1000000, Count: 78498, Valid: True
pez-clj-boolean-array-to-vector-futures;2529;5.000828000;8;algorithm=base,faithful=yes,bits=8
```

(On an Apple M1 Max with 8 performance cores and 32GB of RAM.)

## Development

If you have experience with Clojure: It's just a regular tools/deps project. Start it and connect your Clojure editor of choice to it.

If not, I suggest using [Calva](https://calva.io):

1. Open the project root in in VS Code.
1. Open `sieve.clj`
1. Issue the command **Calva: Starta Clojure REPL in your Project and Connect (aka Jack-in)**
1. When the REPL has started, issue **Calva: Load current file and its dependencies**

Then find one of the Rich Comment blocks in the file, one looks like so:

```clojure
(comment
  (sieve-ba-to-vector-even-filter-futures 1)
  ;; => []

  (sieve-ba-to-vector-even-filter-futures 10)
  ;; => [2 3 5 7]

  (sieve-ba-to-vector-even-filter-futures 100)
  ;; You try it!

  (with-progress-reporting (quick-bench (sieve-ba-to-vector-even-filter-futures 1000000)))
  (quick-bench (sieve-ba-to-vector-even-filter-futures 1000000))
  ;; Execution time mean : 2.703046 ms

  ;; This one takes a lot of time, you have been warned
  (with-progress-reporting (bench (sieve-ba-to-vector-even-filter-futures 1000000)))
  )
```

Place the cursor in one of the forms (say `(sieve-ba-to-vector-even-filter-futures 1)`) and issue the command **Calva: Evaluate top level form** (default key binding `alt+enter`). Try `alt+enter` in some of the other forms too.

The project is equipped with the excellent [Criterium](https://github.com/hugoduncan/criterium) library, which is very nice (and sort-of de-facto) for benchmarking Clojure code.

Happy sieving! ♥️