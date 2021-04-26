# FreeBASIC solutions by rbergen

*Category: Faithful*

This is a collection of implementations in FreeBASIC, of which:
1. one is effectively a FreeBASIC copy of the "8 of 30" implementation that [mckoss](https://github.com/mckoss) has written in C
2. one is a "naive" implementation that maintains the array of (non-)primes in an underlying array of bit-mapped 32-bit unsigned integers
3. one is a "naive" implementation that maintains the array of (non-)primes in an underlying array of bit-mapped 64-bit unsigned integers
4. one is a "naive" implementation that maintains the array of (non-)primes in an underlying array of booleans

## Run instructions

### FreeBASIC
Execute the following commands from the implementation directory, in a bash shell:
```
find . -name 'prime_*.bas' -exec fbc {} \;
. runprimes.sh
```

### Docker
A Dockerfile has been provided.

## Output
```
rbergen_8of30;4197;5.001;1
rbergen_bit32;720;5.000;1
rbergen_bit64;661;5.000;1
rbergen_boolean;1720;5.001;1
```