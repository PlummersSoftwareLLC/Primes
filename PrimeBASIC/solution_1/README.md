# FreeBASIC solutions by rbergen

*Category: Faithful*

This is a collection of implementations in FreeBASIC that maintain the array of (non-)primes in an underlying array of:
1. bit-mapped 32-bit unsigned integers
2. bit-mapped 64-bit unsigned integers
3. booleans

## Run instructions

### FreeBASIC
Execute the following commands from the implementation directory, in a bash shell:
```
find . -name 'prime_*.bas' -exec fbc {} \;
. runprimes.sh
```

### Docker
A Dockerfile has been provided. It will run the implementations with the FreeBASIC version provided with the solution. The reason is that Linux distros don't tend to supply FreeBASIC as a package.

## Output
```
rbergen_bit32;730;5.000;1
rbergen_bit64;664;5.006;1
rbergen_boolean;1711;5.002;1
```