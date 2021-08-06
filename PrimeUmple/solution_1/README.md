# Umple solution by mmcdon20

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

A faithful implementation of the prime sieve algorithm in the [umple language](https://cruise.umple.org/umple/).

## Run instructions

To run the solution using Docker, run the following command:

```
docker build -t primes-umple .
docker run --rm -it primes-umple
```

To run the solution using the umple compiler, run the following command:

```
java -jar umple.jar PrimeUmple.ump
java PrimeUmple.java
```

### Output

```
mmcdon20_umple;3709;5.000000;1;algorithm=base,faithful=yes,bits=1
```
