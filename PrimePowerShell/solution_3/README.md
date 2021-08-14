# PowerShell solution by Rob Cannon (RobCannon)

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This PowerShell implementation of the sieve of Erastosthenes is derived from crowbar27's PowerShell solution #2. It improves
the speed by dereferencing object references used inside the calculation loop and other optimzations.  I also removed the 
pipelining aspect that, while a prime feature of PowerShell, do help the speed out.  In keeping with the original algorithm,
I used a BitArray that hold a bit for all number (and not just odds) which removes the need for the bit shift operation inside
the cacluation loop.  I also embraced the class, since it was a requirement of the implementation and used where appropriate. The
end result is about double the number of passes.

I also made sure this script was linux friendly and can be run from a shell prompt on a linux system as well as a Windows system.

The implementation maintains the used of the `PsObject` to the pipeline from crowbar27's implementation, which can be discarded by piping to `Out-Null`.

## Run instructions
Run `.\PrimePowerShell.ps1` in a PowerShell. In order to suppress the pipeline output and only have the results written to the console in the standard output format, use `.\PrimePowerShell.ps1 | Out-Null`.

## Output
Intel Core i7-1165G7 @ 2.80GHz on Windows 11 with PowerShell 7.1.3 in Windows Subsystem for Linus for a sieve size of 10, 100, 1000, 10000, 100000 and 1000000:

```
RobCannon_ps3;342712;5.00003;1;algorithm=base,faithful=yes,bits=1
RobCannon_ps3;288698;5.00001;1;algorithm=base,faithful=yes,bits=1
RobCannon_ps3;91910;5.00003;1;algorithm=base,faithful=yes,bits=1
RobCannon_ps3;8204;5.0002;1;algorithm=base,faithful=yes,bits=1
RobCannon_ps3;656;5.00515;1;algorithm=base,faithful=yes,bits=1
RobCannon_ps3;52;5.06592;1;algorithm=base,faithful=yes,bits=1
```
