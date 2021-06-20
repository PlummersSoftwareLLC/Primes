#
# PrimePowerShell.ps1
#
# Copyright (C) 2021 Christoph Müller
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

<#
.SYNOPSIS 
Computes primes using the sieve of Erastosthenes for the purpose of
benchmarking.

.DESCRIPTION
This is a PowerShell implementation of davepl's sieve of Erastosthenes for
benchmarking programming languages and CPUs. It is based on the original
C# version posted by davepl and follows this implementation as closely
as possible. There are, however, two notable differences: First, the access
to the bit array uses shifts instead of divisions, because PowerShell would
not truncate the result, and second, several functions have been
renamed to match the list of allowed/recommended verbs for PowerShell
commands.

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

.PARAMETER ResetState
The ResetState switch instructs the script to reset the bit array instead of
reallocating the state object in every iteration. Results using this switch
are not faithful according to
https://github.com/PlummersSoftwareLLC/Primes/blob/drag-race/CONTRIBUTING.md.

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
    [Parameter(Position = 0)] [string] $SieveSize = 1000000,
    [Parameter(Position = 1)] [int] $MinimumRuntime = 5,
    [switch] $ResetState,
    [switch] $ShowResults
)


# Known results for validating the output.
$KnowResults = @{
    10 = 4;
    100 = 25;
    1000 = 168;
    10000 = 1229;
    100000 = 9592;
    1000000 = 78498;
    10000000 = 664579;
    100000000 = 5761455
}


################################################################################
# BEGIN OF UTILITY FUNCTION DEFINITIONS                                        #
################################################################################

# Initialises a new data structure holding the state of the prime sieve.
function New-PrimeSieve([int] $Size) {
    $count = [int] (($Size + 1) / 2)

    return New-Object -TypeName PSObject -Property @{
        SieveSize = $Size;
        # Note: One could argue that BitArray should not be used here, because
        # it is not a native PS data structure like an array of booleans.
        # However, one of the key features of PS is that you can use any .NET
        # code, so I decided to do so here.
        BitArray = (New-Object -TypeName System.Collections.BitArray -ArgumentList $count, $true)
    }
}


# Counts the number of primes in the given state object. Note that this is the
# original 'countPrimes' method, which has been renamed to conform with standard
# PS verbs
# (https://docs.microsoft.com/de-de/powershell/scripting/developer/cmdlet/approved-verbs-for-windows-powershell-commands?view=powershell-7.1).
function Measure-Primes([PsObject] $Sieve) {
    $retval = 0

    for ($i = 0; $i -lt $Sieve.BitArray.Count; ++$i) {
        if ($Sieve.BitArray[$i]) {
            ++$retval
        }
    }

    return $retval
}


# Validates the results agains known results. Note that this is the original
# 'validateResults' method, which has been renamed to conform with standard PS
# verbs.
function Test-Results([PsObject] $Sieve) {
    if ($KnowResults.ContainsKey($Sieve.SieveSize)) {
        return ($KnowResults[$Sieve.SieveSize] -eq (Measure-Primes $Sieve))
    } else {
        return $false
    }
}


# Retrieves the bit at the specified index.
function Get-Bit([PsObject] $Sieve, [int] $Index) {
    if (($Index % 2) -eq 0) {
        return $false
    } else {
        # Note: On PowerShell, the following division would not truncate.
        #return $Sieve.BitArray[$Index / 2]
        return $Sieve.BitArray[$Index -shr 1]
    }
}


# Clears the bit at the specified index.
function Clear-Bit([PsObject] $Sieve, [int] $Index) {
    if (($Index % 2) -eq 0) {
        Write-Warning "You are clearing even bits, which is sub-optimal."
    } else {
        # Note: On PowerShell, the following division would not truncate.
        #$Sieve.BitArray[$Index / 2] = $false;
        $Sieve.BitArray[$Index -shr 1] = $false;
    }
}


# Computes the primes up to the specified limit. Note this is the original
# 'runSieve' method, which has been renamed to conform with standard PS verbs.
function Invoke-Sieve([PsObject] $Sieve) {
    $factor = 3
    $q = [int] [System.Math]::Sqrt($Sieve.SieveSize)

    while ($factor -lt $q) {
        for ($i = $factor; $i -le $Sieve.SieveSize; ++$i) {
            if (Get-Bit $Sieve $i) {
                $factor = $i
                break;
            }
        }
        
        $step = 2 * $factor
        for ($i = $factor * 3; $i -le $Sieve.SieveSize; $i += $step) {
            Clear-Bit $Sieve $i
        }

        $factor += 2
    }
}


# Writes the results to the console. Note this is the original 'printResults'
# method, which has been renamed to conform with standard PS verbs.
# As an additional change, the function returns the primes that have been found,
# which are used to generate the pipeline output.
function Write-Results([PsObject] $Sieve, [bool] $ShowResults,
        [double] $Duration, [int] $Passes) {
    $count = 1
    $retval = [System.Collections.Generic.List[int]]::New($Sieve.SieveSize)
    $retval.Add(2)
    
    for ($i = 3; $i -le $Sieve.SieveSize; ++$i) {
        if (Get-Bit $Sieve $i) {
            ++$count
            $retval.Add($i)
        }
    }

    if ($ShowResults) {
        Write-Host $retval
    }

    # Force current session to 'en-US' for subsequent output.
    $culture = [System.Globalization.CultureInfo]::GetCultureInfo('en-US')
    [System.Threading.Thread]::CurrentThread.CurrentUICulture = $culture
    [System.Threading.Thread]::CurrentThread.CurrentCulture = $culture       

    # Note: Omit original output to conform with output requirements.
    #Write-Host "Passes: $Passes, Time: $Duration, Avg: $($Duration / $Passes), Limit: $($Sieve.SieveSize), Count: $count, Valid: $(Test-Results $Sieve)"
    #Write-Host ""

    Write-Host "crowbar27_ps1;$Passes;$($Duration.ToString("G6"));1;algorithm=base,faithful=yes,bits=1"

    return $retval.ToArray()
}

# Resets the sieve object in order to reuse it.
function Reset-Sieve([PsObject] $Sieve) {
    $Sieve.BitArray.SetAll($true)
}


################################################################################
# MAIN SCRIPT BODY                                                             #
################################################################################

$stopWatch = New-Object -TypeName System.Diagnostics.Stopwatch
$passes = 0

if ($ResetState) {
    Write-Verbose "Starting benchmark with reusable state ..."
    Write-Warning "Results obtained with a reusable state object are not faithful."
    $sieve = New-PrimeSieve $SieveSize

    $stopWatch.Start()

    while ($stopWatch.Elapsed.TotalSeconds -lt $MinimumRuntime) {
        Reset-Sieve $sieve
        Invoke-Sieve $sieve
        ++$passes
    }

    $stopWatch.Stop()

} else {
    Write-Verbose "Starting benchmark ..."

    $stopWatch.Start()

    while ($stopWatch.Elapsed.TotalSeconds -lt $MinimumRuntime) {
        $sieve = New-PrimeSieve $SieveSize
        Invoke-Sieve $sieve
        ++$passes
    }

    $stopWatch.Stop()
}


Write-Verbose "Processing results ..."
$primes = (Write-Results $sieve $ShowResults $stopWatch.Elapsed.TotalSeconds $passes)

New-Object -TypeName PsObject -Property @{
    AverageDuration = ($stopWatch.Elapsed.TotalSeconds / $passes)
    Duration = $stopWatch.Elapsed;
    Passes = $passes;
    Primes = $primes;
    SieveSize = $SieveSize;
    Valid = (Test-Results $Sieve);
}
