# VB solution by Nukepayload2

![Algorithm](https://img.shields.io/badge/Algorithm-wheel-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

- A faithful Visual Basic port of `PrimeCSharp/solution_1/Sieves/PrimeSieveArrayPool8of30M.cs` (original author: @Kinematics)
- Removed the use of `ArrayPool(Of T).Shared` to make it faithful.
- It uses .NET 5, and can be compiled using Visual Studio 16.9 or later.

## Run instructions

- .NET SDK 5 or later is required. You can download it from https://dotnet.microsoft.com/download .
- From the solution directory, run
```console
dotnet run -c release
```

## Output
```
Nukepayload2_FaithfulCsKinematicsArrayPool8of30M;12214;5.0002872;1;algorithm=wheel,faithful=yes,bits=1
```

Environment of the sample output:
- CPU:         Intel Core i5 9600KF (base 3.7 GHz, boost 4.6 GHz)
- RAM:         DDR4 2400 MHz, slots: 4/4
- OS Name:     Windows
- OS Version:  10.0.19043
- OS Platform: Windows
- RID:         win10-x64
- Base Path:   C:\Program Files\dotnet\sdk\5.0.202\
