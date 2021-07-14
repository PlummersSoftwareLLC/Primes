<?php

declare(strict_types=1);

final class PrimeSieve
{
    private int $sieveSize;
    private int $rawBitsSize;

    /** @var SplFixedArray<bool> */
    private SplFixedArray $rawBits;

    /** @var array<int, int> */
    private static $resultsDictionary = [
        10 => 4,                 // Historical data for validating our results - the number of primes
        100 => 25,               // to be found under some limit, such as 168 primes under 1000
        1000 => 168,
        10000 => 1229,
        100000 => 9592,
        1000000 => 78498,
        10000000 => 664579,
        100000000 => 5761455,
    ];

    private function validateResults(int $count): bool
    {
        if (!\array_key_exists($this->sieveSize, self::$resultsDictionary)) {
            return false;
        }

        return self::$resultsDictionary[$this->sieveSize] === $count;
    }

    public function __construct(int $sieveSize)
    {
        $this->sieveSize = $sieveSize;
        $this->rawBitsSize = (int)(($this->sieveSize + 1) / 2);
    }

    public function runSieve(): void
    {
        $factor = 3;
        $sieveSize = $this->sieveSize;
        $rawBitsSize = $this->rawBitsSize;
        $q = (int) \sqrt($sieveSize);
        $rawBits = new SplFixedArray($rawBitsSize);

        while ($factor < $q) {
            for ($num = $factor; $num <= $sieveSize; $num += 2) {
                // Invert the checks
                if (null === $rawBits[$num / 2]) {
                    $factor = $num;

                    break;
                }
            }

            $start = ($factor ** 2) / 2;

            for ($num = $start; $num <= $rawBitsSize; $num += $factor) {
                // Invert value asignment to keep main functionality intact with inverted checks
                $rawBits[$num] = true;
            }

            $factor += 2;
        }

        $this->rawBits = $rawBits;
    }

    public function printResults(bool $showResults, float $duration, int $passes): void
    {
        if ($showResults) {
            echo '2, ';

            for ($num = 3; $num < $this->sieveSize; $num += 2) {
                if (($num & 1) === 1 && null === $this->rawBits[$num / 2]) {
                    echo $num, ", ";
                }
            }

            echo "\n";
        }

        $count = $this->countPrimes();

        \printf(
            "Passes: %d, Time: %lf, Avg: %lf, Limit: %ld, Count: %d, Valid: %d\n",
            $passes,
            $duration,
            $duration / $passes,
            $this->sieveSize,
            $count,
            $this->validateResults($count)
        );

        // Following 2 lines added by rbergen to conform to drag race output format
        echo "\n";
        \printf("HugoSantiagoBecerraAdan;%d;%f;1;algorithm=base,faithful=yes\n", $passes, $duration);
    }

    public function countPrimes(): int
    {
        $count = (int) ($this->sieveSize >= 2);

        for ($num = 3; $num < $this->sieveSize; $num++) {
            // Invert the checks
            if (($num & 1) === 1 && null === $this->rawBits[$num / 2]) {
                ++$count;
            }
        }

        return $count;
    }
}

$sieveSize = 1000000;                  // Set sieve size
$runTime = 5;                          // The amount of seconds the script should be running for
$printResults = false;                 // Print the prime numbers that are found

$passes = 0;
$tStart = \microtime(true);

while (true) {
    $sieve = new PrimeSieve($sieveSize);
    $sieve->runSieve();

    ++$passes;

    $duration = \microtime(true) - $tStart;

    if ($duration >= $runTime) {
        $sieve->printResults($printResults, $duration, $passes);

        break;
    }
}
