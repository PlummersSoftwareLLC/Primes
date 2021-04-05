<?php

declare(strict_types=1);

final class PrimeSieve
{
    private int $sieveSize = 0;

    /** @var SplFixedArray<bool> */
    private SplFixedArray $Bits;

    /** @var array<int, int> */
    private const myDict = [
        10 => 1,                 // Historical data for validating our results - the number of primes
        100 => 25,               // to be found under some limit, such as 168 primes under 1000
        1000 => 168,
        10000 => 1229,
        100000 => 9592,
        1000000 => 78498,
        10000000 => 664579,
        100000000 => 5761455,
    ];

    private function validateResults(): bool
    {
        if (!\array_key_exists($this->sieveSize, self::myDict)) {
            return false;
        }

        return self::myDict[$this->sieveSize] === $this->countPrimes();
    }

    public function __construct(int $n)
    {
        $this->sieveSize = $n;
        $this->Bits = new SplFixedArray($this->sieveSize);
    }

    public function runSieve(): void
    {
        $factor = 3;
        $q = \sqrt($this->sieveSize);

        while ($factor <= $q) {
            for ($num = $factor; $num < $this->sieveSize; $num += 2) {
                if (!$this->Bits[$num]) {
                    $factor = $num;

                    break;
                }
            }

            for ($num = $factor * $factor; $num < $this->sieveSize; $num += $factor * 2) {
                $this->Bits[$num] = true;
            }

            $factor += 2;
        }
    }

    public function printResults(bool $showResults, float $duration, int $passes): void
    {
        if ($showResults) {
            echo '2, ';
        }

        $count = 1;

        for ($num = 3; $num <= $this->sieveSize; $num += 2) {
            if (!$this->Bits[$num]) {
                if ($showResults) {
                    echo $num.', ';
                }

                ++$count;
            }
        }

        if ($showResults) {
            echo "\n";
        }

        \printf(
            "Passes: %d, Time: %lf, Avg: %lf, Limit: %ld, Count1: %d, Count2: %d, Valid: %d\n",
            $passes,
            $duration,
            $duration / $passes,
            $this->sieveSize,
            $count,
            $this->countPrimes(),
            $this->validateResults()
        );
    }

    public function countPrimes(): int
    {
        $count = 1;

        for ($i = 3; $i < $this->sieveSize; $i += 2) {
            if (!$this->Bits[$i]) {
                ++$count;
            }
        }

        return $count;
    }
}

$passes = 0;
$tStart = \microtime(true);

while (true) {
    $sieve = new PrimeSieve(1000000);
    $sieve->runSieve();

    ++$passes;

    $tD = \microtime(true) - $tStart;

    if ($tD >= 5) {
        $sieve->printResults(false, $tD, $passes);

        break;
    }
}
