# Centurion assembly solution by ren14500

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Deviation](https://img.shields.io/badge/Deviation-sievesize-blue)

## Description

This solution provides two implementations in Centurion assembly, that being the assembly language for the Centurion minicomputer. The Centurion minicomputer is the topic of a [series of videos](https://youtube.com/playlist?list=PLnw98JPyObn0wJFdbcRDP7LMz8Aw2T97V) on the YouTube channel [UsagiElectric](https://www.youtube.com/@UsagiElectric).

The Centurion minicomputer was built around the [CPU6 board](https://github.com/Nakazoto/CenturionComputer/wiki/CPU6-Board), which was a further development of the original CPU4 architecture. In turn, that architecture was at least heavily inspired by the [Eldorado Electrodata Corporation EE200](https://github.com/Nakazoto/CenturionComputer/tree/main/Computer/EE200).

As it stands, there does not seem to be a comprehensive programmer's manual available for Centurion CPU6 assembly. The instruction set and the assembly syntax used in this solution have been deduced by combining the following sources:

- [The EE200 documentation](https://github.com/Nakazoto/CenturionComputer/tree/main/Computer/EE200) referred to earlier.
- [A wiki page](https://github.com/sjsoftware/centurion-cpu6/wiki/Centurion-CPU6-Instruction-Reference) written by [sjsoftware/gecho](https://github.com/sjsoftware). The page covers their efforts towards disassembling the CPU6 microcode to fully document the CPU's opcode behavior.
- The output of the CPL compiler. CPL is a language for which [a Primes solution is also available](../../PrimeCenturionPL/solution_1/).
- Reverse engineering of XASSM, the Centurion assembler executable.

In fact, for those interested in the Centurion CPU6 instruction set and the assembler syntax, this solution may itself act as a source of information. For one, the source code included in this solution has been extensively documented.

## Implementations

This solution includes two implementations:

- An implementation to be run on top of the Centurion OS. It uses service calls for time keeping and I/O, and is kept in the file [sieveo.asm](sieveo.asm).
- An implementation to be run on bare metal, that is directly on the Centurion hardware. It uses memory-mapped I/O (MMIO) to communicate with the console (CRT0), and is kept in the file [sieveb.asm](sieveb.asm).

## Sieve sizes

The sieve size for the implementations can be configured by uncommenting a triple of lines at the top of the assembly source files; details are included in the files themselves.  
The maxmimum sieve size supported by the implementations is 65,535.

## Run instructions

This solution's implementations can be assembled and executed on an actual Centurion minicomputer, or a sufficiently complete emulator. A list of known emulators can be found on [the respective page](https://github.com/Nakazoto/CenturionComputer/wiki/Emulators-and-Simulations) on Nakazoto's [CenturionComputer wiki](https://github.com/Nakazoto/CenturionComputer/wiki) on GitHub.

Use the following commands to edit, assemble and run the solution:

```text
S.CED ZSIEVE 0 CRT0
P.ASM SIEVE 0 DUMMY X
.RUN XSIEVE
```

For the bare metal version, use the same commands to edit and compile, but don't use `.RUN` to run the program. Instead, one needs to reboot the (emulated) Centurion and run it from the bootloader - those instructions are in the top of the sieveb.asm file.
