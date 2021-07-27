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
### Result obtained on a Intel Core i7-7700HQ on Windows 10 (version 20H2 build 19042.1110)  
8 threads: PrimePas
```
passes: 18659, Time: 5.02 s, Avg: 0.268825 ms, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
olivierbrun;18659;5.02;8;algorithm=base,faithful=yes,bits=1
```
16 threads: PrimePas -t16
```
passes: 19469, Time: 5.02 s, Avg: 0.257589 ms, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
olivierbrun;19469;5.02;16;algorithm=base,faithful=yes,bits=1
```
32 threads: PrimePas -t32
```
passes: 20255, Time: 5.09 s, Avg: 0.251444 ms, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
olivierbrun;20255;5.09;32;algorithm=base,faithful=yes,bits=1
```
64 threads: PrimePas -t64
```
passes: 20617, Time: 5.09 s, Avg: 0.247078 ms, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
olivierbrun;20617;5.09;64;algorithm=base,faithful=yes,bits=1
```

### Result obtained on a Raspberry PI4 running Raspberry PI OS 64 bits (5.10.17-v8+) 
4 threads: ./PrimePas
```
passes: 5143, Time: 5.00 s, Avg: 0.972779 ms, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
olivierbrun;5143;5.00;4;algorithm=base,faithful=yes,bits=1
```
8 threads: ./PrimePas -t8
```
passes: 5850, Time: 5.01 s, Avg: 0.856239 ms, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
olivierbrun;5850;5.01;8;algorithm=base,faithful=yes,bits=1
```
16 threads: ./PrimePas -t16
```
passes: 6058, Time: 5.02 s, Avg: 0.828986 ms, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
olivierbrun;6058;5.02;16;algorithm=base,faithful=yes,bits=1
```

