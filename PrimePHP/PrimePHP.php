<?php

/**
 * PHP 8 Prime Sieve
 * Class PrimeSieve
 */

class PrimeSieve
{
    private array $rawbits;

    private array $primeCounts = [
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
        $this->rawbits = array_fill(0, (int)($this->sieveSize + 1) / 2, true);
    }

    private function validateResult(): ?int
    {
        return $this->primeCounts[$this->sieveSize];
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

            foreach (range($factor * 3, $this->sieveSize, $factor * 2) as $num) {
                $this->clearBit($num);
            }

            $factor += 2;
        }
    }

    public function printResults(
        int $duration,
        int $passes,
        bool $showResults = true,
    ): void
    {
        if ($showResults) {
            echo '2' . ", ";
        }

        $count = 1;

        foreach (range(3, $this->sieveSize) as $num) {
            if ($this->getBit($num)) {
                if ($showResults) {
                    echo $num . ", ";
                }
                $count++;
            }
        }

        echo sprintf("Passes: %d, Time: %dms, Avg: %dms, Limit: %d, Count: %d, Valid: %s",
            $passes,
            $duration,
            $duration / $passes,
            $this->sieveSize,
            $count,
            ($this->validateResult() === $count) ? 'True' : 'False'
        );
    }
}

$tStart = hrtime(true);
$passes = 0;

while ((hrtime(true) - $tStart) / 1e+6 < 10000) {
    $sieve = new PrimeSieve(1000000);
    $sieve->runSieve();
    $passes += 1;

    $sieve->printResults((hrtime(true) - $tStart) / 1e+6, $passes, false);
    echo "\n";
}
