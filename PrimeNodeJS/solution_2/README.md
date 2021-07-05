# NodeJS solution by rogiervandam

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Implementation in nodeJS, with a 32 bit integer array as buffer for the bit array.
This implementation is based on the logic from:

- NodeJS/solution_1 by Frank van Bakel
- Python/solution_2 by ssovest
- PrimeCPP          by Dave Plummer

## Run instructions
Install nodeJS: <https://nodejs.org/en/download/>

```bash
cd path/to/sieve
node PrimeNode.js
```

## Output
Below is an example of the verbose output

```bash
Passes: 4612, Time: 5.00, Avg: 0.00108430 (sec/pass), Sieve size: 1000000, Primes: 78498, Valid: true
