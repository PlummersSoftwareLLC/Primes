class PrimeSieve {
    private int $sieveSize = 0;
    private vec<bool> $bitArray = vec[];
    private dict<int, int> $primeCounts = dict[
        10 => 4,
        100 => 25,
        1000 => 168,
        10000 => 1229,
        100000 => 9592,
        1000000 => 78498,
        10000000 => 664579,
        100000000 => 5761455
    ];

    public function __construct(int $size) {
        $this->sieveSize = $size;
        for ($i = 0; $i < (($this->sieveSize + 1) / 2); ++$i) {
            $this->bitArray[] = true;
        }
    }

    public function countPrimes(): int {
        $count = 0;
        for ($i = 0; $i < \count($this->bitArray); ++$i) {
            if ($this->bitArray[$i]) {
                $count += 1;
            }
        }
        return $count;
    }

    public function validateResults(): bool {
        if (idx($this->primeCounts, $this->sieveSize)) {
            return $this->primeCounts[$this->sieveSize] === $this->countPrimes();
        }
        return false;
    }

    private function GetBit(int $index): bool {
        if ($index % 2 === 0) {
            return false;
        }
        return $this->bitArray[(int)($index / 2)];
    }

    public function runSieve(): void {
        $factor = 3;
        $q = (int)\sqrt((float)$this->sieveSize);

        while ($factor < $q) {
            for ($num = (int)($factor / 2); $num <= \count($this->bitArray); ++$num) {
                if ($this->bitArray[$num]) {
                    $factor = ($num * 2) + 1;
                    break;
                }
            }

            for ($num = (int)(($factor * 3) / 2); $num < \count($this->bitArray); $num += $factor)
                $this->bitArray[$num] = false;

            $factor += 2;
        }
    }

    public function printResults(bool $showResults, int $duration, int $passes): void {
        if ($showResults)
            echo "2, ";

        $count = 1;
        for ($num = 3; $num <= $this->sieveSize; ++$num) {
            if ($this->GetBit($num)) {
                if ($showResults)
                    echo "{num}, ";

                $count += 1;
            }
        }
        if ($showResults)
            echo "\n";

        $avg = ($duration / $passes) / 1000;
        $timeInSecs = $duration / 1000;
        echo "Passes: {$passes}, Time: {$timeInSecs}, Avg: {$avg}, Limit: {$this->sieveSize}, Count: {$count}, Valid: {$this->validateResults()}\n";
        echo "\n";
        echo "da-strange-boi;{$passes};{$timeInSecs};1;algorithm=base,faithful=yes,parallel=no\n";
    }
}

function getTimeMS(): int {
    return (int)(microtime(true) * 1000);
}

<<__EntryPoint>>
function main(): void {
    $tStart = getTimeMS();
    $passes = 0;
    $sieve = null;

    while (getTimeMS() - $tStart < 5000) {
        $sieve = new PrimeSieve(1000000);
        $sieve->runSieve();
        $passes += 1;
    }

    $tD = getTimeMS() - $tStart;
    if (!($sieve is null))
        $sieve->printResults(false, $tD, $passes);
}
