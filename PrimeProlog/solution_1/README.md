# Prolog solution by jimbxb

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

A solution written in Prolog. This is probably far from optimal for the language, I just thought some Prolog representation would be nice!

## Run

### Locally

```
$ cd path/to/prolog/solution
$ swipl -O -o primes -c ./primes.pl
$ ./primes
```

### Docker

```
$ cd path/to/dockerfile
$ docker build -t prolog-primes .
$ docker run --rm prolog-primes
```

## Output

```
$ swipl -O -o primes -c ./primes.pl
$ ./primes
jimbxb_prolog;1;7.271000;1;algorithm=base,faithful=yes,bits=1
```

## Author

James Barnes (jimbxb)

https://github.com/jimbxb