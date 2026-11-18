# PHP wheel sieve by LauJosefsen

Each worker builds a presieved bitmap once inside the five-second timer. It stores
one bit per odd candidate in native PHP integers. A set bit marks a composite or
an unused bit at the end of the buffer. The seed crosses out multiples of the odd
primes through 97.

Each pass starts with the seed. PHP copies the array on its first write. A
remaining composite has a factorization `p * q` with `101 <= p <= q`, so the
pass collects surviving candidates through `intdiv(limit, 101)` as possible
cofactors. For each prime factor through the square root of the limit, it marks
the products from its square up to the limit. The implementation unrolls that
loop four products at a time.

The output uses `algorithm=wheel,faithful=no,bits=1`. The worker reuses a
full-size presieved bitmap across passes and discovers the factors above 97 in
each pass. Each worker validates its final count of 78,498 primes. The optional
parallel command sums completed passes across independent PHP processes and
includes their startup and validation in the parent's wall time.

## Run

Build and run from this directory using the official PHP 8.5.12 Alpine CLI image
with tracing JIT.

```sh
docker build -t primes-php-wheel .
docker run --rm primes-php-wheel
docker run --rm primes-php-wheel parallel.php 16
```

The last command starts 16 workers. Choose a worker count appropriate to the host.

## Verify

```sh
docker run --rm primes-php-wheel check.php
```

The check compares each flag and the total count with an independent sieve on
two passes per limit. The limits include word boundaries, prime squares,
1,000,000, and 101 cubed, where a product of three unpresieved factors first
appears.

## Example output

Single-worker and 16-worker runs on an AMD Ryzen 9 7950X (Linux amd64)

```text
LauJosefsen-wheel;53754;5.000000845;1;algorithm=wheel,faithful=no,bits=1
LauJosefsen-wheel-par;792137;5.027167654;16;algorithm=wheel,faithful=no,bits=1
```
