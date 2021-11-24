# 6502 assembly solution by rbergen for Commodore PET

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Deviation](https://img.shields.io/badge/Deviation-sievesize-blue)

This is a solution written in 6502 assembly, targeting the Commodore PET computer. More specifically, it targets a PET with 32K RAM.

## Description
### Characteristics
The following applies to the implementation embedded in this solution:
- It runs a sieve of size 400,000 instead of 1,0000,000. The reason is that the PET doesn't have enough RAM to hold more prime number candidates.
- It uses a bitmap to keep track of prime number candidates. The bitmap only contains entries for odd numbers. This means that the total sieve buffer is 400,000/2/8 = 25,000 bytes in size.
- The implementation uses a pointer consisting of a 16-bit (low byte/high byte) memory address pointer, and a bit index. The current factor is kept in a byte value and a bit number. The actual factor can thus be calculated using 8 * &lt;byte value&gt; + &lt;bit number&gt;. 

The first two points come with the consequence that some specific peculiarities apply:
- The sieve size (as specified using the `SIEVE_SIZE` symbol) needs to be divisable by 16.
- The square root of the sieve size (as specifed using the `SIEVE_SQRT` symbol) needs to be divisible by 8, or else rounded up to the nearest multiple of 8.
- The total memory available for the sieve buffer is 30,720 bytes (0x800 to 0x8000), which adds up to 245,760 bits. That means the largest supported sieve size is 491,520.

### Output
The implementation shows progress indicators on the screen, as follows:
- Initialization of the buffer to ones is marked with `-` (minus)
- Clearing of multiples of found factors is marked with `.` (dot), one per factor
- Counting of set bits is marked with `+` (plus)
- Saving of file output is marked with `@` (at sign)

The results of the sieve run are written to a sequential file called OUTPUT on the first disk (unit 8). It consists of two lines:
- A line indicating if the prime count is valid: `VALID Y` or `VALID N`
- A line reporting the runtime of the sieve, in the shape of a 6-digit hexadecimal count of software clock ticks (each 1/60th of a second): `TIME 0Xnnnnnn`

The parse script provided in the solution checks the prime count validity and, if valid, converts the software clock tick count to a number of seconds.

## Run Instructions
The instructions below use [Retro Assembler](https://enginedesigns.net/retroassembler) to assemble the source code and [VICE 3.5](https://vice-emu.sourceforge.io/) to run the assembled program. Note that VICE will also require certain support files to run, specifically the PET system ROMs.
Instructions for installing these applications aren't provided here; the applications' websites provide clear documentation on how to do this.

### Windows 10
- Make sure Retro Assembler and the VICE binaries are in your PATH.
- Open a PowerShell window and cd into the solution directory.
- Execute the following commands:
  ```
  .\build.ps1
  .\run.ps1
  ```
  Note that `build.ps1` does not need to be executed in subsequent runs; executing `run.ps1` will then suffice.
- When the execution of the prime sieve completes, BASIC will show a READY prompt. The VICE window will look like this:
  ![VICE window](https://i.ibb.co/S09QLfP/petprimes.png)

  VICE can now be closed.
- Execute the following command to parse and display the result:
  ```
  .\parse.ps1
  ```

### Linux
- Make sure .NET, awk, grep and mac2unix are installed on your system. In Ubuntu, this can be arranged by issuing the following commands:
  ```
  wget https://packages.microsoft.com/config/ubuntu/20.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
  sudo dpkg -i packages-microsoft-prod.deb
  rm packages-microsoft-prod.deb
  sudo apt-get update
  sudo apt-get install -y apt-transport-https && \
  sudo apt-get update && \
  sudo apt-get install -y aspnetcore-runtime-6.0 gawk dos2unix grep
  ```
- Make sure that Retro Assembler (the contents of the program's ZIP file, including retroassembler.dll) is located in the solution directory.
- Make sure the VICE binaries are in your PATH.
- cd into the solution directory.
- Execute the following commands:
  ```
  ./build.sh
  ./run.sh
  ```
  Note that `build.sh` does not need to be executed in subsequent runs; executing `run.sh` will then suffice.
- When the execution of the prime sieve completes, BASIC will show a READY prompt. The VICE window will look like shown above. <br/>
  VICE can now be closed.
- Execute the following command to parse and display the result:
  ```
  ./parse.sh
  ```

### Other systems/emulators/actual hardware
The core program (`primes.s`) can in principle be assembled using any assembler that a) accepts the "old style" 6502 assembly format and b) is able to produce a Commodore `PRG` output file. It's possible that the source file needs minor modifications for whatever assembler you use; please consult to the documentation for your assembler to see if this is the case.
After assembly, the program can be loaded in any PET emulator (or an actual 32K PET) using either (a) tape or disk (image). 

Assuming the program is itself the only program stored on a disk in device 8, it can be loaded and started using the following commands:
```
LOAD"*",8,1
SYS 4864
```

Note that the program writes a file to the disk in device 8, which thus has to be present. The contents of this file are described under [Output](#output).

## Results
On my system, on Windows 10, the following is shown when the parse command is executed:
```
rbergen-pet;1;63.2;1;algorithm=base,faithful=no,bits=1
```
