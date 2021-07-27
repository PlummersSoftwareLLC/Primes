# Object Pascal solution by olivierbrun

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This solution implements a multithreaded version written in freepascal (Object Pascal).  
Bits storing flags for prime numbers are packed.  

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

### Command line options when running the PrimePas program
````
PrimePas -d<Duration> -t<Threads count> -s<Sieve size>

Default values are:

Duration: 5
Threads count: number of CPU cores calculated by the program
SieveSize: 1000000
````

### Docker
A Dockerfile has been provided.

## Output
Result obtained on a Intel Core i7-7700HQ (base 2.8 GHz, boost 3.8GHz) running Windows 10 (version 20H2 build 19042.1110)  
```
passes: 15424, Time: 5.00 s, Avg: 0.324170 ms, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
olivierbrun;15424;5.00;8;algorithm=base,faithful=yes,bits=1
```
Result obtained on a Raspberry PI4 running Raspberry PI OS 64 bits (5.10.17-v8+) 
```
passes: 4086, Time: 5.01 s, Avg: 1.224914 ms, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
olivierbrun;4086;5.01;4;algorithm=base,faithful=yes,bits=1
```
