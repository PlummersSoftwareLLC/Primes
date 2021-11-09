#!/usr/bin/env pwsh
# PrimePowerShell.ps1
#
# Copyright (C) 2021 Robert Cannon
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
# This is a modification of the code found here:
# https://github.com/PlummersSoftwareLLC/Primes/tree/drag-race/PrimeCSharp/solution_3

<#
.SYNOPSIS 
Computes primes using the sieve of Erastosthenes for the purpose of
benchmarking.

.DESCRIPTION
This is a PowerShell implementation of davepl's sieve of Erastosthenes for
benchmarking programming languages and CPUs. This implementation uses the
capability of PowerShell to build a dynamic assembly from C# and run that.
The C# code is based on the current fastest C# implementation by tannergooding,
solution_3. It has long been a capability of PowerShell to allow a script to access 
.NET directly when it is needed for speed or accessing some feature not available 
in PowerShell. This capability is built into PowerShell and does not need any 
external dependencies.  This is considered a mixed solution for the drag-race
and is thus not considered faithful.

The implementation computes primes up to a given maximum number (1000000 by
default) and repeats this process until a user-specified minimum time
(5 seconds by default) have elapsed. The results are writting to standard
output in the format described at
https://github.com/PlummersSoftwareLLC/Primes/blob/drag-race/CONTRIBUTING.md
and also emitted to the pipeline as PsObject.

.PARAMETER SieveSize
The SieveSize parameter specifies the maximum number up to which the prime
sieve should look for primes. A list of numbers can be provided to execute
multiple runs in one invocation.  This parameter defaults to 1000000.

.PARAMETER MinimumRuntime
The MinimumRuntime parameter specifies the minimum number of seconds that the
script should run. If this value is not reached while computing a sieve of
the specified size, an additional pass of the same size is started. This
parameter defaults to 5.


.PARAMETER ShowResults
The ShowResults switch instructs the script to write all primes that have been
found to the console.

.INPUTS
None. You cannot pipe objects to this script.

.OUTPUTS
A PsObject containing the test results, namely the number of passes that have
been completed, the overall duration of the test and the primes that have been
found.

.EXAMPLE
.\PrimePowerShell

.EXAMPLE
.\PrimePowerShell | Out-Null

.EXAMPLE
.\PrimePowerShell -SieveSize 1000
#>

[CmdletBinding()] 
param(
  [Parameter(Mandatory = $false,
    Position = 0)]
  [ValidateSet(10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000)]
  [int[]]$SieveSize = 1000000,
  
  [Parameter(Mandatory = $false,
    Position = 1)]
  [int]$MinimumRuntime = 5,

  [switch] $ShowResults
)


$code = @"
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

public class PrimeCS
{
    private static readonly Dictionary<ulong, ulong> myDict = new Dictionary<ulong, ulong>
    {
        [10] = 4,                 // Historical data for validating our results - the number of primes
        [100] = 25,               // to be found under some limit, such as 168 primes under 1000
        [1000] = 168,
        [10000] = 1229,
        [100000] = 9592,
        [1000000] = 78498,
        [10000000] = 664579,
        [100000000] = 5761455,
        [1000000000] = 50847534,
        [10000000000] = 455052511,
    };

    private readonly ulong sieveSize;
    private readonly byte[] rawbits;

    public ulong Passes;
    public double Duration;

    [MethodImpl(MethodImplOptions.AggressiveOptimization)]
    public PrimeCS(ulong n)
    {
        sieveSize = n;
        rawbits = GC.AllocateUninitializedArray<byte>((int)((n / 8) + 1), pinned: true);
        rawbits.AsSpan().Fill(0xFF);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool GetBit(byte[] rawbits, ulong index)
    {
        Debug.Assert((index % 2) != 0);
        index /= 2;
        return (getrawbits(rawbits, index / 8U) & (1u << (int)(index % 8))) != 0;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void ClearBit(byte[] rawbits, ulong index)
    {
        Debug.Assert((index % 2) != 0);
        index /= 2;
        getrawbits(rawbits, index / 8) &= (byte)~(1u << (int)(index % 8));
    }

    // runSieve
    // 
    // Calculate the primes up to the specified limit

    [MethodImpl(MethodImplOptions.AggressiveOptimization)]
    public void runSieve()
    {
        var sieveSize = this.sieveSize;
        var rawbits = this.rawbits;

        // hoist null check
        _ = getrawbits(rawbits, 0);

        ulong factor = 3;
        ulong q = (ulong)Math.Sqrt(sieveSize);

        while (factor <= q)
        {
            for (ulong num = factor; num < sieveSize; num += 2)
            {
                if (GetBit(rawbits, num))
                {
                    factor = num;
                    break;
                }
            }

            // If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
            // We can then step by factor * 2 because every second one is going to be even by definition

            for (ulong num = factor * factor; num < sieveSize; num += factor * 2)
                ClearBit(rawbits, num);

            factor += 2;
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static ref byte getrawbits(byte[] rawbits, ulong index)
    {
        return ref Unsafe.Add(ref MemoryMarshal.GetArrayDataReference(rawbits), (nint)index);
    }

    public ulong countPrimes()
    {
        var sieveSize = this.sieveSize;
        var rawbits = this.rawbits;

        // hoist null check
        _ = getrawbits(rawbits, 0);

        ulong count = (sieveSize >= 2) ? 1UL : 0UL;
        for (ulong i = 3; i < sieveSize; i+=2)
            if (GetBit(rawbits, i))
                count++;
        return count;
    }

    public bool validateResults()
    {
        return myDict.TryGetValue(sieveSize, out ulong sieveResult) && (sieveResult == countPrimes());
    }

    public ulong[] getPrimes()
    {
        var sieveSize = this.sieveSize;
        var rawbits = this.rawbits;
        var primes = new ulong[this.countPrimes()];

        // hoist null check
        _ = getrawbits(rawbits, 0);

        ulong i = 0;
        primes[i++] = 2;

        for (ulong num = 3; num <= sieveSize; num += 2)
        {
            if (GetBit(rawbits, num))
            {
                primes[i++] = num;
            }
        }

        return primes;
    }

    public static PrimeCS Main(ulong sieveSize = 1000000, long maxSecondsToRun = 5)
    {
        long maxTicksToRun = maxSecondsToRun * Stopwatch.Frequency;

        PrimeCS sieve = new PrimeCS(sieveSize);
        // run a priming run
        sieve.runSieve();

        ulong passes = 0;
        var stopwatch = new Stopwatch();
        stopwatch.Start();

        while (stopwatch.ElapsedTicks < maxTicksToRun)
        {
            sieve = new PrimeCS(sieveSize);
            sieve.runSieve();
            passes++;
        }
        stopwatch.Stop();

        sieve.Duration = stopwatch.Elapsed.TotalSeconds;
        sieve.Passes = passes;

        return sieve;
    }
}
"@

Add-Type -TypeDefinition $code -CompilerOptions "-unsafe","-optimize","-langversion:preview"

# Force current session to 'en-US' for output.
[System.Threading.Thread]::CurrentThread.CurrentUICulture = `
    [System.Threading.Thread]::CurrentThread.CurrentCulture = `
    [System.Globalization.CultureInfo]::GetCultureInfo('en-US')


$SieveSize | % {
    $size = $_
    Write-Verbose "Starting benchmark ..."

    $sieve = [PrimeCS]::Main($size)

    Write-Verbose "Processing results ..."

    if ($ShowResults) {
        ($sieve.getPrimes() -join ", ") | Write-Host
    }
    Write-Host ("RobCannon_ps2;{0};{1:G6};{2};algorithm=base,faithful=no,bits=1" -f $sieve.Passes, $sieve.Duration, 1)

    [PsObject]@{
        AverageDuration = ($sieve.Duration / $sieve.Passes)
        Duration        = $sieve.Duration
        Passes          = $sieve.Passes
        Primes          = $sieve.countPrimes()
        SieveSize       = $size
        Valid           = $sieve.validateResults()
    }
}
