# PowerShell solution by Christoph Müller (crowbar27)

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This PowerShell implementation of the sieve of Erastosthenes is derived from davepl's C# implementation and follows this implementation as closely as possible. Most notably, a .NET `BitArray` is used as in the original implementation.

The implementation differs in the naming of some functions, which have been renamed to use [approved verbs for PowerShell commands](https://docs.microsoft.com/en-gb/powershell/scripting/developer/cmdlet/approved-verbs-for-windows-powershell-commands?view=powershell-7.1) instead of the method names used by davepl. As PowerShell does not support integer division and the results would have to be rounded using `[System.Math]::Floor`, the access to the bit array (`$Index / 2`) has been replaced with shifts (`$Index -shr 1`).

Furthermore, the implementation returns the results of the run as a `PsObject` to the pipeline, which can be discarded by piping to `Out-Null`.

## Run instructions
Run `.\PrimePowerShell.ps1` in a PowerShell. In order to suppress the pipeline output and only have the results written to the console in the standard output format, use `.\PrimePowerShell.ps1 | Out-Null`.

## Output
Intel Core i7-9700K on Windows 10 Professional 21H1 with PowerShell 5.1.19041.906 for a sieve size of 10, 100, 1000, 10000, 100000 and 1000000:

```
crowbar27_ps1;27439;5.00012;1;algorithm=base,faithful=yes,bits=1
crowbar27_ps1;4070;5.00044;1;algorithm=base,faithful=yes,bits=1
crowbar27_ps1;373;5.00387;1;algorithm=base,faithful=yes,bits=1
crowbar27_ps1;31;5.01757;1;algorithm=base,faithful=yes,bits=1
crowbar27_ps1;3;5.52281;1;algorithm=base,faithful=yes,bits=1
crowbar27_ps1;1;20.5757;1;algorithm=base,faithful=yes,bits=1
```
