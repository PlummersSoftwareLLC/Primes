# Red solution by mmcdon20

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

A faithful implementation of the prime sieve algorithm in the [red programming language](https://www.red-lang.org).

## Run instructions

To run the solution using Docker, run the following command:

```
docker build -t primes-red .
docker run --rm -it primes-red
```

To run the solution using the red cli, run the following command:

```
./red --cli PrimeRed.red
```

### Output

```
mmcdon20_red;5;5.021970000001602;1;algorithm=base,faithful=yes,bits=1
```
