# PHP wheel sieve by LauJosefsen

Every timed pass constructs a new `WheelSieve` from the requested limit. It
allocates its own candidate buffer and builds its presieve patterns anew. The
fixed residue tables describe which numbers the wheel represents.

On 64-bit PHP, each word holds one composite flag for each of the 48 residues
coprime to 210. On 32-bit PHP, each word holds the eight residues coprime to 30.
A set bit marks a composite or a position outside the limit. The unused high
bits and PHP array storage add overhead.

The sieve first marks multiples of the small primes through 37. Each pattern
for a prime `p` repeats every `p` words. Combining pairs of these short patterns
lets the sieve initialize many flags per write. It then finds the remaining
prime factors in the live sieve. For a factor `p` and a wheel residue `r`,
products whose cofactor is congruent to `r` occupy the same bit in every
`p`th word. Each marking loop starts at the first such product at or above
`p * p` and advances by adding `p` to the word index.

Both entry points report `algorithm=wheel,faithful=yes,bits=1`. The default
runs one worker. The optional parallel entry sums the completed passes of
independent PHP workers and includes their startup and final validation in its
wall time. Each worker checks that its last sieve contains 78,498 primes.

## Run

From this directory, build the official PHP 8.5.12 Alpine CLI image with
OPcache tracing JIT enabled, then run either entry point.

```sh
docker build -t primes-php-wheel .
docker run --rm primes-php-wheel
docker run --rm primes-php-wheel parallel.php 16
```

## Verify

```sh
docker run --rm primes-php-wheel check.php
```

The check compares every flag and the count with an independent sieve on two
fresh passes per limit. Cases include every limit from 0 through 450, wheel
boundaries, prime squares, 41 cubed, one million, and ten million.

## Example output

Single-worker and 16-worker runs on an AMD Ryzen 9 7950X under Linux amd64

```text
LauJosefsen-wheel;30542;5.000047824;1;algorithm=wheel,faithful=yes,bits=1
LauJosefsen-wheel-par;447747;5.027933212;16;algorithm=wheel,faithful=yes,bits=1
```
