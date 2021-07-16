# Prolog solution by jimbxb

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Yet another solution written in Prolog. 

This solution uses the SWI-PL foreign interface with C for a more optimised bitvector representation (`bitvectore/2`).

## Run

### Locally

```sh
$ cd path/to/prolog/solution
$ swipl-ld -cc-options,-w -shared -o bitvector bitvector.c 
$ swipl -O ./primes.pl
```

### Docker

```sh
$ cd path/to/dockerfile
$ docker build -t prolog-primes .
$ docker run --rm prolog-primes
```

## Output

```sh
$ swipl-ld -cc-options,-w -shared -o bitvector bitvector.c
$ swipl -O ./primes.pl
jimbxb-prolog-3;56;5.056000;1;algorithm=base,faithful=yes,bits=1
```

## Author

James Barnes (jimbxb)

https://github.com/jimbxb

## Acknowledgements

C Foreign Language Interface code is inspired by the SWI-PL examples [here](https://www.swi-prolog.org/pldoc/man?section=foreign).