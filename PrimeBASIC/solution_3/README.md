# VB solution by Nukepayload2

![Algorithm](https://img.shields.io/badge/Algorithm-wheel-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

- A Visual Basic .NET port of `PrimeCSharp/solution_1/Sieves/PrimeSieveArrayPool8of30M.cs` (original author: @Kinematics)
- Added an option to control whether to allocate large arrays with `ArrayPool(Of T).Shared` instead of the `ReDim` keyword.
- It uses .NET 5, and can be compiled using Visual Studio 16.9 or later.

## Run instructions 

- .NET SDK 5 or later is required. You can download it from https://dotnet.microsoft.com/download .
- Open terminal and set current directory to the solution directory

If you want to run the ArrayPool implementation, run:
```console
dotnet run -c release -- --arraypool yes
```

If you want to run the ReDim implementation, run:
```console
dotnet run -c release -- --arraypool no
```

If you want to run both implementations in sequence, run:
```console
dotnet run -c release -- --arraypool all
```

## Output
Use ArrayPool:
```
Nukepayload2_ArrayPool8of30M;12217;5.0002745;1;algorithm=wheel,faithful=yes,bits=1
```

Use ReDim:
```
Nukepayload2_ReDim8of30M;14982;5.0002527;1;algorithm=wheel,faithful=yes,bits=1
```

Environment of the sample output:
- CPU:         Intel Core i5 9600KF (base 3.7 GHz, boost 4.6 GHz)
- RAM:         DDR4 2400 MHz, slots: 4/4
- OS Name:     Windows
- OS Version:  10.0.19043
- OS Platform: Windows
- RID:         win10-x64
- Base Path:   C:\Program Files\dotnet\sdk\5.0.202\
