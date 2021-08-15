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
# https://github.com/PlummersSoftwareLLC/Primes/tree/drag-race/PrimePowerShell/solution_2

<#
.SYNOPSIS 
Computes primes using the sieve of Erastosthenes for the purpose of
benchmarking.

.DESCRIPTION
This is a PowerShell implementation of davepl's sieve of Erastosthenes for
benchmarking programming languages and CPUs. It is based on crowbar27's
PowerShell solution #, but inlines more items for performance improvement
and fully embraces the PowerShell class feature for the sieve code which
greatly improves performance.

The implementation computes primes up to a given maximum number (1000000 by
default) and repeats this process until a user-specified minimum time
(5 seconds by default) have elapsed. The results are writting to standard
output in the format described at
https://github.com/PlummersSoftwareLLC/Primes/blob/drag-race/CONTRIBUTING.md
and also emitted to the pipeline as PsObject.

.PARAMETER SieveSize
The SieveSize parameter specifies the maximum number up to which the prime
sieve should look for primes. This parameter defaults to 1000000.

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

    class prime_sieve
    {
        private readonly ulong sieveSize;
        private readonly byte[] rawbits;

        [MethodImpl(MethodImplOptions.AggressiveOptimization)]
        public prime_sieve(ulong n)
        {
            sieveSize = n;
            rawbits = GC.AllocateUninitializedArray<byte>((int)((n / 8) + 1), pinned: true);
            rawbits.AsSpan().Fill(0xFF);
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

        private bool validateResults()
        {
            return myDict.TryGetValue(sieveSize, out ulong sieveResult) && (sieveResult == countPrimes());
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

        // primeSieve
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

        public void printResults(bool showResults, double duration, ulong passes)
        {
            var sieveSize = this.sieveSize;
            var rawbits = this.rawbits;

            // hoist null check
            _ = getrawbits(rawbits, 0);

            if (showResults)
                Console.Write("2, ");

            ulong count = (sieveSize >= 2) ? 1UL : 0UL;
            for (ulong num = 3; num <= sieveSize; num += 2)
            {
                if (GetBit(rawbits, num))
                {
                    if (showResults)
                        Console.Write(num + ", ");
                    count++;
                }
            }

            if (showResults)
                Console.WriteLine();
            Console.WriteLine($"Passes: {passes}, Time: {duration}, Avg: {duration / passes}, Limit: {sieveSize}, Count: {countPrimes()}, Valid: {validateResults()}");

            Console.WriteLine();
            Console.WriteLine($"tannergooding;{passes};{duration};1;algorithm=base,faithful=yes,bits=1", passes, duration);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static ref byte getrawbits(byte[] rawbits, ulong index)
        {
            return ref Unsafe.Add(ref MemoryMarshal.GetArrayDataReference(rawbits), (nint)index);
        }
    }

    public static void Main()
    {
        const ulong SieveSize = 1000000;
        const long MillisecondsPerSecond = 1000;
        const long MicrosecondsPerSecond = 1000000;

        ulong passes = 0;
        prime_sieve sieve = null;

        var stopwatch = Stopwatch.StartNew();

        while (stopwatch.ElapsedMilliseconds < (5 * MillisecondsPerSecond))
        {
            sieve = new prime_sieve(SieveSize);
            sieve.runSieve();
            passes++;
        }
        stopwatch.Stop();

        if (sieve != null)
            sieve.printResults(false, (stopwatch.Elapsed.TotalSeconds * MicrosecondsPerSecond) / SieveSize, passes);
    }
}
"@

Add-Type -TypeDefinition $code -CompilerOptions "-unsafe","-optimize","-langversion:preview"

[PrimeCS]::Main()



# $stopWatch = [System.Diagnostics.Stopwatch]::new()
# [long]$minimumticks = $MinimumRuntime * [System.Diagnostics.Stopwatch]::Frequency

# $SieveSize | % {
#   $passes = 0
#   $size = $_
#   Write-Verbose "Starting benchmark ..."

#   $stopWatch.Restart()

#   while ($stopWatch.ElapsedTicks -lt $minimumticks) {
#     $sieve = [Sieve]::new($size)
#     $sieve.runSieve()
#     $passes++
#   }

#   $stopWatch.Stop()

#   Write-Verbose "Processing results ..."
#   # Force current session to 'en-US' for output.
#   [System.Threading.Thread]::CurrentThread.CurrentUICulture = `
#     [System.Threading.Thread]::CurrentThread.CurrentCulture = `
#     [System.Globalization.CultureInfo]::GetCultureInfo('en-US')    
#   $sieve.printResults($ShowResults, $stopWatch.Elapsed.TotalSeconds, $passes, 1)

#   [PsObject]@{
#     AverageDuration = ($stopWatch.Elapsed.TotalSeconds / $passes)
#     Duration        = $stopWatch.Elapsed
#     Passes          = $passes
#     Primes          = $sieve.countPrimes()
#     SieveSize       = $size
#     Valid           = $sieve.validateResults()
#   }
# }
