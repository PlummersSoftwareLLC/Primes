# MIXAL solution by rbergen

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Deviation](https://img.shields.io/badge/Deviation-sievesize-blue)

*Category: Unfaithful / Closest Approximation*

This is an implementation in the assembly language for the MIX computer, described in D.E. Knuth's book series The Art of Computer Programming.

As a computer, MIX has a number of specific characteristics:
* It never existed in physical form.
* It was based on or inspired by more or less typical computer designs in the 1960s.
* (Even) in the context of the previous point, it includes a number of quirks. I believe these were introduced on purpose by its designer, to make the solution to some of the exercise problems in the book series less straight-forward.

Due to this, the core implementation deviates from the basic rules in two ways:
* By default, the sieve size is 200,000 instead of 1,000,000. The reason is that MIX has a total memory capacity of 3,999 words, each 30 bits wide. This does not allow for a bit array of 500,000 entries to be stored. In the practical sense, a sieve size of 200,000 is the maximum.
* The implementation does itself not run for a period of 5 seconds. Instead, depending on configuration it will either execute a configured number of sieve runs, or keep repeating sieve runs indefinitely. The reason is that MIX has no internal clock that measures actual time. Instruction execution times are measured in "ticks", the duration of which is undefined, by design. In practice, this means that if a timed execution is desired, the starting, timing and stopping of it must be controlled external to the program.

These deviations are part of the implementation out of necessity, but I have made a genuine effort to stay as close to the original implementation(s) and the basic rules as possible. 

To mitigate the second deviation, the implementation comes with a wrapper shell script that:
* Times the run of the implementation externally, using the GNU time command
* Embeds the measured time into the core implementation output, by replacing a placeholder in the latter
* Turns the upper-case core implementation output into lower case

With these mitigations in place, the output of the implementation + wrapper does conform to the drag-race output format.

## Parameters

Towards the top of the prime.mixal file, a number of parameters can be set:
* `LTSVSZ` (line 13): sieve size that should be used. Note that if the sieve size is changed, the next parameter must also be changed.
* `SVSQRT` (line 14): square root of the sieve size. It needs to be passed as a parameter because MIX does not include an instruction with which square roots can be calculated. 
* `RUNCT` (line 15): number of sieve runs that should be executed. If this is set to a number less than 1, the program will run indefinitely.
* `DOPRTRN` (line 16): set to something other than 0 to print a result line after each run
* `DOPRTRT` (line 17): set to something other than 0 to print a result line in "drag-race" format when the last run has completed

## Run instructions

The program has been tested to work and executed using:
* the MixEmul emulator, which can be acquired in source code and binary form from [its GitHub repository](https://github.com/rbergen/MixEmul). 
* the [GNU MIX Development Kit (MDK)](https://www.gnu.org/software/mdk/)

Other emulators are available, like those listed on [Donald Knuth's The Art of Computer Programming (TAOCP) webpage](https://www-cs-faculty.stanford.edu/~knuth/taocp.html), under the MIXware section. The emulator that is chosen must implement the JxE, JxO, SLB and SRB instructions.

### MixEmul
If MixEmul is used, the instructions for running the program are as follows:
1. Start MixEmul
2. Load prime.mixal using File -> Open program...
3. In the Assembly result window, click Load
4. Choose Actions -> Detach, to make program execution run in the background. This step can be skipped if it is desired to visually follow the program's execution. Do note that execution times in Attached mode are significantly longer.
5. Choose View -> Show Device Editor, and select the Printer tab in the window that appears
6. Choose Actions -> Run to start the implementation 

### MDK
#### Core implementation
Execute the following commands from the implementation directory:
```
mixasm prime
mixvm -r prime
```

#### Wrapper script
Execute the following command from the implementation directory:
```
. runprime.sh
```

### Docker
A Dockerfile has been provided. It creates an image that runs the implementation using the wrapper script.

## Output

The output included below was that of runs that took approximately 5 seconds on my machine when using MDK. This is also why 30 is the value for `RUNCT` in this version of the implementation.

### Core implementation
When using a sieve size of 200,000 and 30 runs, with both run and end result output enabled, output is as follows when using MDK:

```
Program loaded. Start address: 3429
Running ...
RUN: 00001, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00002, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00003, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00004, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00005, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00006, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00007, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00008, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00009, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00010, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00011, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00012, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00013, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00014, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00015, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00016, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00017, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00018, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00019, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00020, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00021, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00022, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00023, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00024, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00025, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00026, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00027, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00028, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00029, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RUN: 00030, SIEVE:  0000200000, PRIMES: 17984, RESULT: CORRECT
RBERGEN;30;<TIME>;1;ALGORITHM=BASE,FAITHFUL=NO,BITS=1
... done
```

When run result output is disabled, the output is as follows:
```
Program loaded. Start address: 3429
Running ...
RBERGEN;30;<TIME>;1
... done
```
### Wrapper script
When using the wrapper script and parameters indicated above, the output is as follows, regardless of whether run result output is enabled or not:
```
rbergen;30;5.19;1;algorithm=base,faithful=no,bits=1
```