# Intel 8080 assembly solution by Mike Douglas, added by rbergen

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)
![Deviation](https://img.shields.io/badge/Deviation-sievesize-blue)

This is an implementation in Intel 8080 assembly. The Intel 8080 processor was used in some of the earliest home computers, like the MITS Altair 8800.

As the systems in question typically had a maximum of 64KiB of total system RAM (and the Intel 8080 couldn't address more than that) this implementation uses a sieve size of 10,000.

This solution can be considered an assembly translation of the original version of davepl's implementation in Microsoft BASIC, included in this repository as [PrimeBASIC/solution_4](../../PrimeBASIC/solution_4/).

The build and run scripts, and therefore also the included Dockerfile, use [emuStudio](https://www.emustudio.net/) to assemble the source code and run it in a MITS Altair 8800 emulator. As the emulator aims to be clock accurate, the performance reported from the emulator should be very close to that on an actual Altair 8800.

To make the code assemble in emuStudio, some minor changes to the source code had to be made:

- Labels were postfixed with a colon
- The endless loop at the end of the program was replaced with an `hlt` instruction.

## Run instructions

### Linux

With awk, gcc, git, grep, OpenJDK and wget installed, execute the following commands from the implementation directory, in a bash shell:

```bash
./build.sh
./run.sh
```

For subsequent runs, only `run.sh` needs to be executed.

### Docker

A Dockerfile has been provided. With Docker installed on a supported operating system, it can be used with the following commands:

```bash
docker build -t primes-assembly-3 .
docker run primes-assembly-3
```

## Output

```log
mikedouglas-davepl;10;6.033929;1;algorithm=base,faithful=no,bits=8
```
