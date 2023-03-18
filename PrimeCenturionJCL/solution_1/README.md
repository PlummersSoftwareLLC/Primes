# Centurion JCL solution by ren14500

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Deviation](https://img.shields.io/badge/Deviation-sievesize-blue)

## Description

This is an implementation in Centurion Job Control Language (JCL) for the Centurion minicomputer. The Centurion minicomputer is the topic of a [series of videos](https://youtube.com/playlist?list=PLnw98JPyObn0wJFdbcRDP7LMz8Aw2T97V) on the YouTube channel [UsagiElectric](https://www.youtube.com/@UsagiElectric).  
In that, Centurion JCL is not to be confused with [JCL variants offered on IBM mainframes](https://en.wikipedia.org/wiki/Job_Control_Language).

That said, Centurion JCL _is_ the language used to write what we would now call shell scripts on the Centurion minicomputer. A pretty comprehensive discussion of the language is available on [the respective page](https://github.com/Nakazoto/CenturionComputer/wiki/JCL-(Job-Control-Language)) on Nakazoto's [CenturionComputer wiki](https://github.com/Nakazoto/CenturionComputer/wiki) on GitHub.

## Note on the implementation

Due to memory limitations that apply to JCL's execution environment on the Centurion minicomputer, the sieve limit for this solution is 98 instead of the usual 1,000,000. And yes, you are reading that number right. As far as the solution's author is aware, only 354 **bits** of memory are available to any JCL script when treating the variables as integers. That effectively limits the sieve space to 98.

## Run instructions

This solution can be executed on an actual Centurion minicomputer, or a sufficiently complete emulator. A list of known emulators can be found on [the respective page](https://github.com/Nakazoto/CenturionComputer/wiki/Emulators-and-Simulations) on the aforementioned wiki.

Use the following commands to edit and run the solution:

```text
S.CED SIEVE 0 CRT0
SIEVE
```
