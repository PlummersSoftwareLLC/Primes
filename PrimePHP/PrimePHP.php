<?php

/**
 * PHP 8 Prime Sieve
 * Class PrimeSieve
 */

class PrimeSieve
{
    private array $rawbits;

    public static array $primeCounts = [
        10 => 1,
        100 => 25,
        1000 => 168,
        10000 => 1229,
        100000 => 9592,
        1000000 => 78498,
        10000000 => 664579,
        100000000 => 5761455,
    ];

    public function __construct(
        private int $sieveSize = 1000000,
    )
    {
        $rawbitSize = (int)($this->sieveSize + 1) / 2;
        $this->rawbits = array_fill(0, $rawbitSize, true);
    }

    private function getBit(int $index): bool
    {
        if ($index % 2 !== 0) {
            return $this->rawbits[(int)$index / 2];
        }
        return false;
    }

    private function clearBit(int $index): void
    {
        if ($index % 2 !== 0) {
            $this->rawbits[(int)$index / 2] = false;
        }
    }

    public function runSieve()
    {
        $factor = 3;
        $q = sqrt($this->sieveSize);

        while ($factor < $q) {
            foreach (range($factor, $this->sieveSize) as $num) {
                if ($this->getBit($num)) {
                    $factor = $num;
                    break;
                }
            }


            for ($i = $factor * 3; $i <= $this->sieveSize; $i += $factor * 2) {
                $this->clearBit($num);
            }

            $factor += 2;
        }
    }

    public function printResults(): void
    {
        foreach (range(3, $this->sieveSize) as $num) {
            if ($this->getBit($num)) {
                echo $num . ", ";
            }
        }
    }

    public function getRawbitCount(): int
    {
        return array_sum($this->rawbits);
    }
}

//Entry
$tStart = hrtime(true);       //Init time
$passes = 0;                            //Init passes
$sieveSize = 1000000;                   //Set sieve size
$printResults = false;                  //Print the prime numbers that are found
$rawbitCount = null;                    //Init a rawbitCount to validate the result
$runTime = 10;                          //The amount of seconds the script should be running for

while (getTimeDiffInMs($tStart) < $runTime * 1000) {
    $sieve = new PrimeSieve($sieveSize);
    $sieve->runSieve();
    $rawbitCount = $sieve->getRawbitCount();
    $passes++;

    if ($printResults) {
        $sieve->printResults();
        echo "\n";
    }
}


$tD = getTimeDiffInMs($tStart);     //Get to total time passed

//Print the results
printf(
    "Passes: %d, Time: %dms, Avg: %dms, Limit: %d, Count: %d, Valid: %s",
    $passes,
    $tD,
    $tD / $passes,
    $sieveSize,
    $rawbitCount,
    (validateResult($sieveSize) === $rawbitCount) ? 'True' : 'False'
);

function getTimeDiffInMs(int $tStart): int
{
    return (hrtime(true) - $tStart) / 1e+6;
}

function validateResult($sieveSize): ?int
{
    return PrimeSieve::$primeCounts[$sieveSize];
}
