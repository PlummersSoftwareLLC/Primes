# Clojure solution by mmcdon20

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

A faithful implementation of the prime sieve algorithm in the [clojure language](https://clojure.org/).

## Run instructions

To run the solution using Docker, run the following command:

```
docker build -t primes-clojure .
docker run --rm -it primes-clojure
```

To run the solution using clojure, run the following command:

```
clojure -M PrimeClojure.clj
```

### Output

```
mmcdon20_clojure;3;5.436000;1;algorithm=base,faithful=yes,bits=1
```
