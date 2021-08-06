# Yoix solution by mmcdon20

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

A faithful implementation of the prime sieve algorithm in the [yoix scripting language](https://github.com/att/yoix).

## Run instructions

To run the solution using Docker, run the following command:

```
docker build -t primes-yoix .
docker run --rm -it primes-yoix
```

To run the solution using the yoix interpreter, run the following command:

```
java -jar yoix.jar PrimeYoix.yx
```

### Output

```
mmcdon20_yoix;1;7.375000;1;algorithm=base,faithful=yes,bits=1
```
