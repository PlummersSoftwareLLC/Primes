# Hack solution by da-strange-boi

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This implamentation is a copy of the PrimeCS version by davepl, but translated to Hack

## Run instructions
### Docker
1. Build: `docker build -t hack-primes .`
2. Run: `docker run --rm -it hack-primes`

### Hack
1. Install HHVM runtime from the [offical download page](https://docs.hhvm.com/hhvm/getting-started/getting-started)
2. Run the code using the follow syntax: `hhvm PrimeHack.hack`

## Output
Intel Core i5-2500 @ 3.30GHz (Ubuntu 20.04)
```
Passes: 51, Time: 5.011, Avg: 0.098254901960784, Limit: 1000000, Count: 78498, Valid: 1

da-strange-boi;51;5.011;1;algorithm=base,faithful=yes,parallel=no
```