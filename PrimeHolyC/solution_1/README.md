# Holy C solution by FunToHard

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

Faithful implementation of the Sieve of Eratosthenes algorithm in Holy C using the cross-platform [holyc-lang](https://github.com/Jamesbarford/holyc-lang) compiler.

## Run instructions

### Docker
Build and run using Docker:

```sh
docker build -t primes-holyc .
docker run --rm primes-holyc
```

### Native (if holyc-lang compiler is installed)

```sh
hcc sieve.HC -o sieve
./sieve
```

## Benchmarks

outputs on my machine (AMD Ryzen 5 7235HS, 12GM RAM, Windows 11 with WSL2):

```
legen-holyc;436;5.002504;1;algorithm=base,faithful=yes,bits=8
legen-holyc;438;5.004268;1;algorithm=base,faithful=yes,bits=8
legen-holyc;359;5.014401;1;algorithm=base,faithful=yes,bits=8
legen-holyc;362;5.009996;1;algorithm=base,faithful=yes,bits=8
```

## Notes

Holy C is the system programming language of TempleOS, originally created by Terry Davis. This implementation demonstrates how the Sieve of Eratosthenes can be implemented in Holy C.

The solution uses the holyc-lang compiler, which enables Holy C code to run on Linux and macOS.
