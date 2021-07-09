<?php

declare(strict_types=1);

/**
 * PHP Prime Sieve
 * Class PrimeSieve
 */

class PrimeSieve
{
    private array $rawbits;

    private int $sieveSize;
    private int $rawBitsSize;

    public static array $primeCounts = [
        10 => 4,
        100 => 25,
        1000 => 168,
        10000 => 1229,
        100000 => 9592,
        1000000 => 78498,
        10000000 => 664579,
        100000000 => 5761455,
    ];

    public function __construct(
        $sieveSize = 1000000
    )
    {
        $this->sieveSize = $sieveSize;
        $this->rawBitsSize = (int)(($this->sieveSize + 1) / 2);
    }

    public function runSieve()
    {
        $factor = 3;
        $sieveSize = $this->sieveSize;
        $q = sqrt($sieveSize);
        $rb = array_fill(0, $this->rawBitsSize, 1);

        while ($factor < $q) {
            for ($i = $factor; $i <= $sieveSize; $i += 2) {
                if ($rb[$i / 2]) {
                    $factor = $i;
                    break;
                }
            }

            $ft2 = $factor * 1;
            $start = ($factor * $factor) / 2;
            for ($i = $start; $i <= $this->rawBitsSize; $i += $ft2) {
                $rb[$i] = 0;
            }

            $factor += 2;
        }
        $this->rawbits = $rb;
    }

    public function printResults(): void
    {
        for ($i = 0; $i < $this->sieveSize; $i++) {
            if ($i % 2 && $this->rawbits[$i / 2]) {
                echo $i . ", ";
            }
        }
    }

    public function getRawbitCount(): int
    {
        return array_sum($this->rawbits);
    }
}

//Entry
$tStart = microtime(true);       //Init time
$passes = 0;                            //Init passes
$sieveSize = 1000000;                   //Set sieve size
$printResults = false;                  //Print the prime numbers that are found
$rawbitCount;                           //Init a rawbitCount to validate the result
$runTime = 10;                          //The amount of seconds the script should be running for

while (getTimeDiffInMs($tStart) < $runTime * 1000) {
    $sieve = new PrimeSieve($sieveSize);
    $sieve->runSieve();
    $passes++;
}
$rawbitCount = $sieve->getRawbitCount();
if ($printResults) {
    $sieve->printResults();
    echo "\n";
}


$tD = getTimeDiffInMs($tStart);     //Get to total time passed

//Print the results
printf(
    "Passes: %d, Time: %dms, Avg: %dms, Limit: %d, Count: %d, Valid: %s",
    $passes,
    (int)$tD,
    $tD / $passes,
    $sieveSize,
    $rawbitCount,
    (validateResult($sieveSize) === $rawbitCount) ? 'True' : 'False'
);

// Following 2 lines added by rbergen to conform to drag race output format
echo "\n\n";
printf("DennisdeBest;%d;%f;1;algorithm=base,faithful=yes\n", $passes, ((float)$tD) / 1000);

function getTimeDiffInMs(float $tStart): float
{
    return (microtime(true) - $tStart) * 1000;
}

function validateResult($sieveSize): ?int
{
    return PrimeSieve::$primeCounts[$sieveSize];
}