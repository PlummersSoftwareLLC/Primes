# NodeJS solution by rogiervandam

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
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
```

## Output
Below is an example of the verbose output

```bash
rogiervandam;4063;5.000575601999997;1;algorithm=base,faithful=yes,bits=1

The first 100 found primes are: [
    2,   3,   5,   7,  11,  13,  17,  19,  23,  29,  31,  37,
   41,  43,  47,  53,  59,  61,  67,  71,  73,  79,  83,  89,
   97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151,
  157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223,
  227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281,
  283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359,
  367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433,
  439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503,
  509, 521, 523, 541
]
Passes: 4063, Time: 5.00, Avg: 0.00123076 (sec/pass), Sieve size: 1000000, Primes: 78498, Valid: true
