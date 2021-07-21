# NodeJS solution by rogiervandam
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Implementation in nodeJS, with a 32 bit integer array as buffer for the bit array.
This implementation is based on the logic from:
- previous implementation of NodeJS/solution_1 by Frank van Bakel
- Python/solution_2                            by ssovest
- PrimeCPP                                     by Dave Plummer

## Run instructions
Install nodeJS: <https://nodejs.org/en/download/>

```bash
cd path/to/sieve
node PrimeNode.js
node PrimeNode_cluster.js
```

## Output
Below is an example of the output

```bash
rogiervandam;4063;5.000575601999997;1;algorithm=base,faithful=yes,bits=1
rogiervandam;30382;5.060920399999945;6;algorithm=base,faithful=yes,bits=1
```
