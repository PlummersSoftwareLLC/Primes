# PowerShell solution by Rob Cannon (RobCannon)

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is a PowerShell implementation of davepl's sieve of Erastosthenes for
benchmarking programming languages and CPUs. This implementation used the
capability of PowerShell to build a dynamic assembly from C# and run that.
The C# code is based on the current fastest C# implementation by tannergooding, 
solution_3.  I am not sure if this is faithful or not since it is mostly using 
C# and not PowerShell.  I think it is as this has long been a capability of 
PowerShell that allows a script to jump to a lower level when it is needed for 
speed or accessing some feature not available in PowerShell.  Since this 
capability is built into PowerShell and does not need and external dependancies, 
it should count as faithful, IMO.

The implementation maintains the used of the `PsObject` to the pipeline from crowbar27's implementation, which can be discarded by piping to `Out-Null`.

## Run instructions
Run `.\PrimePowerShell.ps1` in a PowerShell. In order to suppress the pipeline output and only have the results written to the console in the standard output format, use `.\PrimePowerShell.ps1 | Out-Null`.

## Output
Intel Core i7-1165G7 @ 2.80GHz on Windows 11 with PowerShell 7.1.3 in Windows Subsystem for Linus for a sieve size of 10, 100, 1000, 10000, 100000 and 1000000:

```
RobCannon_ps4;29621618;5.00004;1;algorithm=base,faithful=yes,bits=1
RobCannon_ps4;23481503;5.00089;1;algorithm=base,faithful=yes,bits=1
RobCannon_ps4;6305039;5;1;algorithm=base,faithful=yes,bits=1
RobCannon_ps4;704742;5;1;algorithm=base,faithful=yes,bits=1
RobCannon_ps4;66692;5.00007;1;algorithm=base,faithful=yes,bits=1
RobCannon_ps4;5984;5.00021;1;algorithm=base,faithful=yes,bits=1
```
