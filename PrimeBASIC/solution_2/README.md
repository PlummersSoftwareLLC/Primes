# FreeBASIC 32-bit-mapped solution by rbergen

*Category: Faithful*

This is an implementation in FreeBASIC that maintains the array of (non-)primes in an underlying array of bit-mapped 32-bit unsigned integers.

## Run instructions

### FreeBASIC
Execute the following commands from the implementation directory:
```
fbc prime.bas
./prime
```

### Docker
A Dockerfile has been provided. It will run the implementation with the FreeBASIC version provided with the solution. The reason is that Linux distros don't tend to supply FreeBASIC as a package.

## Output
```
rbergen_bit32;724;5.004;1
```