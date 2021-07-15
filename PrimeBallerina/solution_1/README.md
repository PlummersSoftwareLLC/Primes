# Ballerina solution by da-strange-boi

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-yellowgreen)

This implamentation is a copy of the PrimeCS version by davepl, but translated to Ballerina

## Run instructions
### Docker
1. Build: `docker build -t ballerina-primes .`
2. Run: `docker run --rm -it ballerina-primes`

### Ballerina
1. Install Ballerina SDK from the [offical download page](https://ballerina.io/downloads/)
2. Run the code using the follow syntax: `bal run PrimeBal.bal`

## Output
Intel Core i5-2500 @ 3.30GHz (Ubuntu 20.04)
```
Passes: 102, Time: 5, Avg: 0.049019607843137254, Limit: 1000000, Count: 78498, Valid: true

da-strange-boi;102;5;1;algorithm=base,faithful=yes,parallel=no,bits=1
```