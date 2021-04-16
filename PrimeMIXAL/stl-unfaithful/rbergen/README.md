# MIXAL solution by rbergen

*Category: Unfaithful / Best Possible*

This is an implementation in the assembly language for the MIX computer, described in D.E. Knuth's book series The Art of Computer Programming.

As a computer, MIX has a number of specific characteristics:
* It never existed in physical form.
* It was based on or inspired by more or less typical computer designs in the 1960s.
* (Even) in the context of the previous point, it includes a number of quirks. I believe these were introduced on purpose by its designer, to make the solution to some of the exercise problems in the book series less straight-forward.

Due to this, the implementation deviates from the basic rules in a number of ways:
* By default, the sieve size is 200,000 instead of 1,000,000. The reason is that MIX has a total memory capacity of 3,999 words, each 30 bits wide. This does not allow for a bit array of 500,000 entries to be stored. In the practical sense, a sieve size of 200,000 is the maximum.
* The implementation does itself not run for a period of 5 seconds and will in fact keep repeating sieve runs indefinitely. The reason is that MIX has no internal clock that measures actual time. Instruction execution times are measured in "ticks", the duration of which is undefined, by design. In practice, this means that if a timed execution is desired, the starting, timing and stopping of it must be controlled external to the program.
* The output of the implementation is not sent to the console, but to the (emulated) printer and has a different format than the basic rules specify. The output to printer was chosen because that allows the output of the program to be kept in the printer device file.

These deviations are part of the implementation out of necessity, but I have made a genuine effort to stay as close to the original implementation(s) and the basic rules as possible. It is therefore that I have labeled the category as "Best Possible". 

## Run instructions

The program has been tested to work executed using the MixEmul emulator, which can be acquired in source code and binary form from [its GitHub repository](https://github.com/rbergen/MixEmul). Alternative emulators are available, like the [GNU MIX Development Kit (MDK)](https://www.gnu.org/software/mdk/) and those listed on [Donald Knuth's The Art of Computer Programming (TAOCP) webpage](https://www-cs-faculty.stanford.edu/~knuth/taocp.html), under the MIXware section. The emulator that is chosen must implement the JxE, JxO, SLB and SRB instructions.

If MixEmul is used, the instructions for running the program are as follows:
1. Start MixEmul
2. Load prime.mixal using File -> Open program...
3. In the Assembly result window, click Load
4. Choose Actions -> Detach, to make program execution run in the background. This step can be skipped if it is desired to visually follow the program's execution. Do note that execution times in Attached mode are significantly longer.
5. Choose View -> Show Device Editor, and select the Printer tab in the window that appears
6. Choose Actions -> Run to start the implementation 

It is possible to modify the sieve size by changing the value of `LTSVSZ` on line 13 of the prime.mixal source file. Do note that the square root of the sieve size in `SVSQRT` on the next line **must** then also be changed accordingly. MIX does not include an instruction with which square roots can be calculated. 

## Output

When using a sieve size of 200,000 output is as follows:

```
RUN: 00001, PRIMES: 17984, RESULT: RIGHT
RUN: 00002, PRIMES: 17984, RESULT: RIGHT
RUN: 00003, PRIMES: 17984, RESULT: RIGHT
RUN: 00004, PRIMES: 17984, RESULT: RIGHT
RUN: 00005, PRIMES: 17984, RESULT: RIGHT
...
```
On my system, a manually timed, detached, single run in a Release build of MixEmul, takes 14 seconds to complete.