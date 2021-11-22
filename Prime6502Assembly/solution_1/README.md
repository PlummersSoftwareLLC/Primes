# 6502 assembly solution by rbergen

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is a solution written in 6502 assembly, targeting the Commodore 128 computer. The reason for targeting the C128 is that it has enough RAM to hold 500,000 bits.

## Run Instructions
The instructions below use [Retro Assembler](https://enginedesigns.net/retroassembler) to assemble the source code and [VICE 3.5](https://vice-emu.sourceforge.io/) to run the assembled program. Note that VICE will also require certain support files to run, specifically the C128 system ROMs.
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
  ![VICE window](https://i.ibb.co/7G9fk2N/c128primes.png)

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
- Make sure that Retro Assembler (the contents of the program's ZIP file, including retroassembler.dll) are located in the solution directory.
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

## Results
On my system, on Windows 10, the following is shown when the parse command is executed:
```
rbergen;1;182.55;1;algorithm=base,faithful=no,bits=1
```