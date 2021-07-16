# Prolog solution by jimbxb

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

Another solution written in Prolog. 

This solution differs from solution 1 by using a dynamically defined predicate (`composite/1`) to track which numbers are composite.

## Run

### Locally

```sh
$ cd path/to/prolog/solution
$ swipl -O -o primes -c ./primes.pl
$ ./primes
```

### Docker

```sh
$ cd path/to/dockerfile
$ docker build -t prolog-primes .
$ docker run --rm prolog-primes
```

## Output

```sh
$ swipl -O -o primes -c ./primes.pl
$ ./primes
jimbxb-prolog-2;4;5.586000;1;algorithm=base,faithful=no,bits=unknown
```

## Author

James Barnes (jimbxb)

https://github.com/jimbxb