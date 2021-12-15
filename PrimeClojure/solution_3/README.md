# Clojure solution 3 by Peter Strömberg (@PEZ)

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

A faithful [Clojure](https://clojure.org/) implementation of
the [Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes)
algorithm.

The solution uses Clojure [transients](https://clojure.org/reference/transients) to make it run faster than it would do with the regular immutable datastructure, gaining about 2X in speed this way.

It also uses [futures](https://clojure.org/about/concurrent_programming) to try gain some extra speed, (hence the **Parallelism = yes** tag). This actually does not gain us all that much, but it's notable enough so I have kept it. Also, I think that when you start to sieve much more than the first one million primes, you will start to see gains from the parallelism. (But I'm not an expert, so don't take my word for it.)

## Run instructions

The runner infrastructure is copied from Alex Vaer's [Clojure Solution 2](https://github.com/PlummersSoftwareLLC/Primes/tree/drag-race/PrimeClojure/solution_2).


1. Install a JDK.
2. Install the [Clojure CLI tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools).
3. Run with `clojure -X sieve/run :warm-up? true`

The warm-up makes the runner start with a silent run, before running one that is reported, giving slightly more consistent results. (But, really, you should use Criterium for benchmarking while developing, see below.)

You can also run this via Docker:

```sh
$ docker pull clojure:openjdk-18-tools-deps-1.10.3.1040
$ docker build -t primes-clojure .
$ docker run --rm -it primes-clojure
```

## Output

```
Passes: 2546, Time: 5.000879000, Avg: 0.00196421, Limit: 1000000, Count: 78498, Valid: True
pez-clj;2546;5.000879000;1;algorithm=base,faithful=yes,bits=8
```

(On an Apple M1 Max with 8 performance cores and 32GB of RAM.)

## Development

If you have experience with Clojure: It's just a regular tools/deps project. Start it and connect your Clojure editor of choice to it.

If not, I suggest using [Calva](https://calva.io):

1. Open the project root in in VS Code.
1. Open `sieve.clj`
1. Issue the command **Calva: Starta Clojure REPL in your Project and Connect (aka Jack-in)**
1. When the REPL has started, issue **Calva: Load current file and its dependencies**

Then find this Rich Comment block in the file:

```clojure
(comment
  (sieve 1)
  ;; => ()

  (sieve 10)
  ;; => [2 3 5 7]

  (sieve 100)
  ;; You try it!
  
  ;; `doall` is not strictly necessary for this sieve, because it is not lazy,
  ;; but for good measure =)
  (with-progress-reporting (quick-bench (doall (sieve 1000000))))
  (quick-bench (doall (sieve 1000000)))

  ;; This one takes a lot of time, you have been warned
  (with-progress-reporting (bench (doall (sieve 1000000))))
  )
```

Place the cursor in one of the forms (say `(sieve 1)`) and issue the command **Calva: Evaluate top level form** (default key binding `alt+enter`). Try `alt+enter` in some of the other forms too.

The project is equipped with the excellent [Criterium](https://github.com/hugoduncan/criterium) library, which is very nice (and sort-of de-facto) for benchmarking Clojure code.

Happy sieving! ♥️