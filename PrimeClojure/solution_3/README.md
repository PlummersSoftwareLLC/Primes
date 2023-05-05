# Clojure solution 3 by Peter Strömberg (@PEZ)

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

Some faithful [Clojure](https://clojure.org/) implementations of
the [Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes)
algorithm.

Four of the implementations work with the odd numbers only, and mainly display the difference in performance between some choices of storage that Clojure gives you:

* `pez-clj-vector` – the ”standard” Clojure [immutable vector](https://clojure.org/reference/data_structures#Vectors)
* `pez-clj-vector-transient` – a [transient](https://clojure.org/reference/transients) Clojure vector
* `pez-clj-bitset` – a Java BitSet
* `pez-clj-boolean-array` - A `boolean-array` (a plain Java Boolean array behind the scenes). This is the fastest of the solutions.

The other implementations either start with all numbers, even and odd, or use some different strategies to deal with the even numbers, either before or after the sieving. Most worth mentioning is `pez-clj-boolean-array-futures-pre` which cleare the evens in a parallelized manner using [futures](https://clojure.org/about/concurrent_programming)

## Run instructions

The runner infrastructure is built from Alex Vear's [Clojure Solution 2](https://github.com/PlummersSoftwareLLC/Primes/tree/drag-race/PrimeClojure/solution_2).

1. Install a JDK.
1. Install the [Clojure CLI tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools).
1. Run with `clojure -X sieve/run :variant :boolean-array ':warm-up?' true`

The warm-up makes the runner start with a silent run, before running one that is reported, giving slightly more consistent results. (But, really, you should use [Criterium](https://github.com/hugoduncan/criterium) for benchmarking while developing, see below.)

You can also build an executable jar and run that:

1. clojure -T:build uber
1. java -jar target/primes.jar '{:variant :boolean-array :warm-up? true}'

To run the four ”main” solutions:

```sh
./build.sh && ./run.sh
```

Using Docker:

```sh
$ docker build -t pez-primes-clojure .
$ docker run --rm -it pez-primes-clojure
```

## Sample output

```
Passes: 53, Time: 5.084422000, Avg: 0.09593249, Limit: 1000000, Count: 78498, Valid: True
pez-clj-vector;53;5.084422000;1;algorithm=base,faithful=yes,bits=?
Passes: 258, Time: 5.018945000, Avg: 0.019453276, Limit: 1000000, Count: 78498, Valid: True
pez-clj-vector-transient;258;5.018945000;1;algorithm=base,faithful=yes,bits=?
Passes: 3445, Time: 5.001991000, Avg: 0.0014519568, Limit: 1000000, Count: 78498, Valid: True
pez-clj-bitset;3445;5.001991000;1;algorithm=base,faithful=yes,bits=1
Passes: 8322, Time: 5.000979000, Avg: 6.009347E-4, Limit: 1000000, Count: 78498, Valid: True
pez-clj-boolean-array;8322;5.000979000;1;algorithm=base,faithful=yes,bits=8
```

(On an Apple M1 Max with 8 performance cores and 32GB of RAM.)

## Development

If you have experience with Clojure: It's just a regular tools/deps project. Start it and connect your Clojure editor of choice to it.

If not, I suggest using [Calva](https://calva.io) (yes, partly because I created it 😄):

1. Open the project root in in VS Code.
1. Open `src/sieve.clj`
1. Issue the command **Calva: Start a Clojure REPL in your Project and Connect (aka Jack-in)**
1. When the REPL has started, issue **Calva: Load current file and its dependencies**

Then find one of the Rich Comment blocks in the file. They look something like so:

```clojure
(comment
  (seive 1)
  ;; => []

  (seive 10)
  ;; => [2 3 5 7]

  (seive 100)
  ;; You try it!

  (with-progress-reporting (quick-bench (seive 1000000)))
  (quick-bench (seive 1000000))
  ;; Execution time mean : 660.572205 µs

  ;; This one takes a lot of time, you have been warned
  (with-progress-reporting (bench (seive 1000000)))
  )
```

Place the cursor in one of the forms (say `(sieve 10)`) and issue the command **Calva: Evaluate top level form** (default key binding `alt+enter`). Try `alt+enter` in some of the other forms too.

The project is equipped with the [Criterium](https://github.com/hugoduncan/criterium) library, which is very convenient (and sort-of de-facto standard) for benchmarking Clojure code.

Happy sieving! ♥️