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


# Known results for validating the output.

class Sieve {
  [int]$SieveSize
  [System.Collections.BitArray]$BitArray
  static $KnowResults = @{
    10        = 4;
    100       = 25;
    1000      = 168;
    10000     = 1229;
    100000    = 9592;
    1000000   = 78498;
    10000000  = 664579;
    100000000 = 5761455
  }

  Sieve([int]$Size) {
    $this.SieveSize = $Size
    $this.BitArray = [System.Collections.BitArray]::new($Size, $true)
  }

  runSieve() {
    [int]$factor = 3
    [int]$q = [System.Math]::Sqrt($this.SieveSize)
    [int]$size = $this.SieveSize
    [System.Collections.BitArray]$array = $this.BitArray
    $array[0] = $false
    $array[1] = $false

    while ($factor -le $q) {
      for ([int]$i = $factor; $i -lt $size; $i++) {
        if (($i -band 1) -or $array[$i]) {
          $factor = $i
          break;
        }
      }

      [int]$step = 2 * $factor
      for ([int]$i = $factor * $factor; $i -lt $size; $i += $step) {
        $array[$i] = $false;
      }
      $factor += 2
    }
  }

  [bool]validateResults() {
    if ([Sieve]::KnowResults.ContainsKey($this.SieveSize)) {
      $count = $this.countPrimes()
      return ([Sieve]::KnowResults[$this.SieveSize] -eq $count)
    }
    else {
      return $false
    }
  }

  printResults(
    [bool]$ShowResults,
    [double]$Duration,
    [int]$Passes,
    [int]$Threads
  ) {
    $count = 1
    $primes = @()
    if ($ShowResults) {
      $primes += 2
    }
    
    for ($i = 3; $i -le $this.SieveSize; $i += 2) {
      if ($this.BitArray[$i]) {
        $count++
        if ($ShowResults) {
          $primes += $i
        }
      }
    }

    if ($ShowResults) {
      $primes -join ", " | Write-Host
    }

    Write-Host ("RobCannon_ps3;{0};{1:G6};{2};algorithm=base,faithful=yes,bits=1" -f $Passes, $Duration, $Threads)
  }

  [int]countPrimes() {
    $count = $this.SieveSize -ge 2 ? 1 : 0

    for ($i = 1; $i -lt $this.BitArray.Count; $i += 2) {
      if ($this.BitArray[$i]) {
        $count++
      }
    }

    return $count
  }
}

################################################################################
# MAIN SCRIPT BODY                                                             #
################################################################################

$stopWatch = [System.Diagnostics.Stopwatch]::new()
[long]$minimumticks = $MinimumRuntime * [System.Diagnostics.Stopwatch]::Frequency

$SieveSize | % {
  $passes = 0
  $size = $_
  Write-Verbose "Starting benchmark ..."

  $stopWatch.Restart()

  while ($stopWatch.ElapsedTicks -lt $minimumticks) {
    $sieve = [Sieve]::new($size)
    $sieve.runSieve()
    $passes++
  }

  $stopWatch.Stop()

  Write-Verbose "Processing results ..."
  # Force current session to 'en-US' for output.
  [System.Threading.Thread]::CurrentThread.CurrentUICulture = `
    [System.Threading.Thread]::CurrentThread.CurrentCulture = `
    [System.Globalization.CultureInfo]::GetCultureInfo('en-US')    
  $sieve.printResults($ShowResults, $stopWatch.Elapsed.TotalSeconds, $passes, 1)

  [PsObject]@{
    AverageDuration = ($stopWatch.Elapsed.TotalSeconds / $passes)
    Duration        = $stopWatch.Elapsed
    Passes          = $passes
    Primes          = $sieve.countPrimes()
    SieveSize       = $size
    Valid           = $sieve.validateResults()
  }
}
