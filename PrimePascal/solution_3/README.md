# Object Pascal solution by olivierbrun

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This solution implements a multithreaded version written in freepascal (Object Pascal) using an array of 32 bits values to store flags for prime numbers.  
As an array index is 32 bits max in FreePascal, with this approach the addressable bit range on an array storing 32 bits values is actually 2^64.
Bits are used to flag odd numbers only, reducing the memory footprint by half.

Benefits from this approach:

- it passes the test with a sieve size of 10.000.000.000 (including on a Raspberry PI) or any range up to 2^64 - 1 (18.446.744.073.709.551.615), as long as the machine has sufficient memory space,
- GetBit and ClearBits functions are inlined to provide some performance boost.

(using GetMem to allocate memory on the heap for the array, provides better performances than using a setlength instruction in the constructor).

### Solution:

The following 3 simple principles are what has made this solution capable of truly handling 64 bits sieve sizes: 
- bits are stored in an array of 32 bits values, each bit stores a flag for an odd number.
- the Getbit(n) function retrieves a bit value using (pseudo code) ```bits[n >> 6] & (1 << ((n >> 1) & 31)))```, where  
index=n >> 6 (divide by 64 = 2 * 32 as only odd numbers are flagged, and each array element is 32 bits), the bit for the ```&``` operation is shifted left by the modulo 32 of n/2
- the ClearBits(n, skip) method loops from i=n/2 to sizeSize/2 and clears each bit using (pseudo code) ```bits[i>>5]&=~(1<<(i & 31))``` followed by ```i+=skip/2``` to take into account the fact that the bits array stores flags for odd numbers only.

## Run instructions

### On linux
Execute the following commands from the implementation directory:
```
fpc PrimePas.pas -O2 -v0
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
1 thread: PrimePas -t1 -v
```
Passes: 6159, Theads: 1, Time: 5.000000 s, Avg: 0.811820 ms, Limit: 1000000, Counts: 78498/78498, Valid: true

olivierbrun;6159;5.000000;1;algorithm=base,faithful=yes,bits=1
```
8 threads: PrimePas -v
```
Passes: 21232, Theads: 8, Time: 5.000000 s, Avg: 0.235494 ms, Limit: 1000000, Counts: 78498/78498, Valid: true

olivierbrun;21232;5.000000;8;algorithm=base,faithful=yes,bits=1
```
16 threads: PrimePas -t16 -v
```
Passes: 23111, Theads: 32, Time: 5.062000 s, Avg: 0.219030 ms, Limit: 1000000, Counts: 78498/78498, Valid: true

olivierbrun;23111;5.062000;32;algorithm=base,faithful=yes,bits=1
```
32 threads: PrimePas -t32 -v
```
Passes: 22872, Theads: 32, Time: 5.032000 s, Avg: 0.220007 ms, Limit: 1000000, Counts: 78498/78498, Valid: true

olivierbrun;22872;5.032000;32;algorithm=base,faithful=yes,bits=1
```
64 threads: PrimePas -t64 -v
```
Passes: 24414, Theads: 64, Time: 5.265000 s, Avg: 0.215655 ms, Limit: 1000000, Counts: 78498/78498, Valid: true

olivierbrun;24414;5.265000;64;algorithm=base,faithful=yes,bits=1
```

#### Results obtained for 10 billions (we exceeds the 5 seconds limit, but it confirms that the solution works for large sieve sizes).  
It's interesting to note that  when running multple threads for such large numbers the CPU utilization decreases to only 52%-58% after a while, so there might be room for improvement.

1 thread: PrimePas -s10000000000 -t1 -v
```
Passes: 1, Theads: 1, Time: 35.282000 s, Avg: 35282.000000 ms, Limit: 10000000000, Counts: 455052511/455052511, Valid: true

olivierbrun;1;35.282000;1;algorithm=base,faithful=yes,bits=1
```
4 threads: PrimePas -s10000000000 -t4 -v
```
Passes: 4, Theads: 4, Time: 97.531000 s, Avg: 24382.750000 ms, Limit: 10000000000, Counts: 455052511/455052511, Valid: true

olivierbrun;4;97.531000;4;algorithm=base,faithful=yes,bits=1
```

### Result obtained on a Raspberry PI4 running Raspberry PI OS 64 bits (5.10.17-v8+) 
1 thread: ./PrimePas -t1 -v
```
Passes: 2256, Theads: 1, Time: 5.001000 s, Avg: 2.216755 ms, Limit: 1000000, Counts: 78498/78498, Valid: true

olivierbrun;2256;5.001000;1;algorithm=base,faithful=yes,bits=1
```
4 threads: ./PrimePas -v
```
Passes: 7021, Theads: 4, Time: 5.002000 s, Avg: 0.712434 ms, Limit: 1000000, Counts: 78498/78498, Valid: true

olivierbrun;7021;5.002000;4;algorithm=base,faithful=yes,bits=1
```
8 threads: ./PrimePas -t8 -v
```
Passes: 7813, Theads: 8, Time: 5.007000 s, Avg: 0.640855 ms, Limit: 1000000, Counts: 78498/78498, Valid: true

olivierbrun;7813;5.007000;8;algorithm=base,faithful=yes,bits=1
```
16 threads: ./PrimePas -t16 -v
```
Passes: 8200, Theads: 16, Time: 5.033000 s, Avg: 0.613780 ms, Limit: 1000000, Counts: 78498/78498, Valid: true

olivierbrun;8200;5.033000;16;algorithm=base,faithful=yes,bits=1
```

Just for the fun, here is the result obtained on the Raspberry PI 4 on 1 thread for a sieve size of 10 billions (CPU utilization was also only 50%-52% and memory usage 597.9MB).  
./PrimePa4 -s10000000000 -t1 -v
```
Passes: 1, Theads: 1, Time: 248.897000 s, Avg: 248897.000000 ms, Limit: 10000000000, Counts: 455052511/455052511, Valid: true

olivierbrun;1;248.897000;1;algorithm=base,faithful=yes,bits=1
```
