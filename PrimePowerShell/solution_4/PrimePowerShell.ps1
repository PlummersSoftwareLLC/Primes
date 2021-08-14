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
using System.Collections;
using System.Collections.Generic;
using System.Globalization;

public class PrimeCS
{
    class prime_sieve
    {
        private readonly int sieveSize = 0;
        private readonly BitArray bitArray; //making it readonly so we tell the compiler that the variable reference cant change. around 5% increase in performance
        private Dictionary<int, int> myDict = new Dictionary<int, int> 
        { 
            { 10 , 4 },                 // Historical data for validating our results - the number of primes
            { 100 , 25 },               // to be found under some limit, such as 168 primes under 1000
            { 1000 , 168 },
            { 10000 , 1229 },
            { 100000 , 9592 },
            { 1000000 , 78498 },
            { 10000000 , 664579 },
            { 100000000 , 5761455 } 
        };

        public prime_sieve(int size) 
        {
            sieveSize = size;
            bitArray = new BitArray(((this.sieveSize + 1) / 2), true);
        }

        public int countPrimes()
        {
            int count = 0;
            for (int i = 0; i < this.bitArray.Count; i++)
                if (bitArray[i])
                    count++;
            return count;
        }

        public bool validateResults()
        {
            if (myDict.ContainsKey(this.sieveSize))
                return this.myDict[this.sieveSize] == this.countPrimes();
            return false;
        }

        private bool GetBit(int index)
        {
            if (index % 2 == 0)
                return false;
            return bitArray[index / 2];
        }

        // primeSieve
        // 
        // Calculate the primes up to the specified limit

        public void runSieve()
        {
            int factor = 3;
            int q = (int) Math.Sqrt(this.sieveSize);

            while (factor < q)
            {
                for (int num = factor / 2; num <= this.bitArray.Count; num++)
                {
                    if (bitArray[num])
                    {
                        factor = num * 2 + 1;
                        break;
                    }
                }

                // If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
                // We can then step by factor * 2 because every second one is going to be even by definition.
                // Note that bitArray is only storing odd numbers. That means an increment of "num" by "factor" is actually an increment of 2 * "factor"

                for (int num = factor * 3 / 2; num < this.bitArray.Count; num += factor)
                    this.bitArray[num] = false;

                factor += 2;
            }
        }

        public void printResults(bool showResults, double duration, int passes)
        {
            if (showResults)
                Console.Write("2, ");

            int count = 1;
            for (int num = 3; num <= this.sieveSize; num++)
            {
                if (GetBit(num))
                {
                    if (showResults)
                        Console.Write(num + ", ");
                    count++;
                }
            }
            if (showResults)
                Console.WriteLine("");

            CultureInfo.CurrentCulture = new CultureInfo("en_US", false);

            Console.WriteLine("Passes: " + passes + ", Time: " + duration + ", Avg: " + (duration / passes) + ", Limit: " + this.sieveSize + ", Count: " + count + ", Valid: " + validateResults());
        
            // Following 2 lines added by rbergen to conform to drag race output format
            Console.WriteLine();
            Console.WriteLine($"davepl;{passes};{duration:G6};1;algorithm=base,faithful=yes,bits=1");
        }
    }

    public static void Main()
    {
        CultureInfo.CurrentCulture = new CultureInfo("en-US", false);

        var tStart = DateTime.UtcNow;
        var passes = 0;
        prime_sieve sieve = null;

        while ((DateTime.UtcNow - tStart).TotalSeconds < 5)
        {
            sieve = new prime_sieve(1000000);
            sieve.runSieve();
            passes++;
        }

        var tD = DateTime.UtcNow - tStart;
        if (sieve != null)
            sieve.printResults(false, tD.TotalSeconds, passes);
    }
}
"@


Add-Type -TypeDefinition $code

[PrimeCS]::Main()
