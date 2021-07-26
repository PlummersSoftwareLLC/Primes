# Object Pascal solution by olivierbrun

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This solution implements a freepascal (Object Pascal) version very close to Dave's original implementation.  
Bits storing flags for prime numbers are packed.  
Some optimizations have been done in the class constructor for the memory allocation and the initialization of the bits to a true value to provide a performance boost.

## Run instructions

### On linux
Execute the following commands from the implementation directory:
```
fpc PrimePas -O2 -v0
./PrimePas
```
### On Windows
Execute the following commands from the implementation directory:
```
fpc PrimePas.pas -O2 -v0
PrimePas
```

The -O2 option provides a quick optimization

### Docker
A Dockerfile has been provided.

## Output
Result obtained on a Intel Core i7-7700HQ (base 2.8 GHz, boost 3.8GHz) running Windows 10 (version 20H2 build 19042.1110)  
passes: 5293, Time: 5.00, Avg: 0.000945, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true

```
olivierbrun;5293;5.00;1;algorithm=base,faithful=yes,bits=1
```
Result obtained on a Raspberry PI4 running Raspberry PI OS 64 bits  
passes: 1657, Time: 5.00, Avg: 0.003020, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true

```
olivierbrun;1657;5.00;1;algorithm=base,faithful=yes,bits=1
```
