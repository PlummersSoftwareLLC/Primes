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

### Command line options when running the PrimePas program.
All parameters are optional.
````
PrimePas [-d<Duration>] [-t<Threads count>] [-s<Sieve size>] [-l] [-v]

-d: provides the ability to specify the duration
-t: provides the ability to specify the number of threads
-s: provides the ability to specify the sieve size
-l: lists all found prime numbers when the latest thread is destroyed
-v: displays validation results such as "Passes: 19794, Theads: 8, Time: 5.016000 s, Avg: 0.253410 ms, Limit: 1000000, Counts: 78498/78498, Valid: true"

Default values are:

Duration: 5
Threads count: number of CPU cores calculated by the program
SieveSize: 1000000
````

### Docker
A Dockerfile has been provided.

## Output
### Result obtained on a Intel Core i7-7700HQ on Windows 10 (version 20H2 build 19042.1110)  
8 threads: PrimePas -v
```
Passes: 19794, Theads: 8, Time: 5.016000 s, Avg: 0.253410 ms, Limit: 1000000, Counts: 78498/78498, Valid: true

olivierbrun;19794;5.016000;8;algorithm=base,faithful=yes,bits=1
```
16 threads: PrimePas -t16 -v
```
Passes: 20819, Theads: 16, Time: 5.016000 s, Avg: 0.240934 ms, Limit: 1000000, Counts: 78498/78498, Valid: true

olivierbrun;20819;5.016000;16;algorithm=base,faithful=yes,bits=1
```
32 threads: PrimePas -t32 -v
```
Passes: 21296, Theads: 32, Time: 5.000000 s, Avg: 0.234786 ms, Limit: 1000000, Counts: 78498/78498, Valid: true

olivierbrun;21296;5.000000;32;algorithm=base,faithful=yes,bits=1
```
64 threads: PrimePas -t64 -v
```
Passes: 22372, Theads: 64, Time: 5.203000 s, Avg: 0.232567 ms, Limit: 1000000, Counts: 78498/78498, Valid: true

olivierbrun;22372;5.203000;64;algorithm=base,faithful=yes,bits=1
```

### Result obtained on a Raspberry PI4 running Raspberry PI OS 64 bits (5.10.17-v8+) 
4 threads: ./PrimePas -v
```
Passes: 5197, Theads: 4, Time: 5.001000 s, Avg: 0.962286 ms, Limit: 1000000, Counts: 78498/78498, Valid: true

olivierbrun;5197;5.001000;4;algorithm=base,faithful=yes,bits=1
```
8 threads: ./PrimePas -t8 -v
```
Passes: 5887, Theads: 8, Time: 5.007000 s, Avg: 0.850518 ms, Limit: 1000000, Counts: 78498/78498, Valid: true

olivierbrun;5887;5.007000;8;algorithm=base,faithful=yes,bits=1
```
16 threads: ./PrimePas -t16 -v
```
Passes: 6077, Theads: 16, Time: 5.008000 s, Avg: 0.824091 ms, Limit: 1000000, Counts: 78498/78498, Valid: true

olivierbrun;6077;5.008000;16;algorithm=base,faithful=yes,bits=1
```

