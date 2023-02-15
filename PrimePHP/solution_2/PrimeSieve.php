<?php declare(strict_types=1);

class PrimeSieve implements Countable
{
    private array $rawbits;

    private readonly int $rbs;

    public function __construct(private readonly int $sieveSize)
    {
        $this->rbs = (int)(($sieveSize + 1) / 100);
        $this->rawbits = array_fill(0, $this->rbs, 0);
    }

    public function __tostring(): string 
    {
        return implode(',', $this->primes());
    }
    
    public function primes(): array 
    {
        $indexes = [];
        for ($i = 3; $i <= $this->sieveSize; $i += 2) {
            if (! $this->isComposite($i))
                $indexes[] = $i;
        }
        return $indexes;
    }

    private function isComposite(int $n): bool
    {
        if ($n <= 1) return true;
        if ($n <= 3) return false;
        if ($n % 2 == 0 || $n % 3 == 0) return true;

        $i = 5;
        $w = 2;

        while ($i * $i <= $n) {
            if ($n % $i == 0) return true;

            $i += $w;
            $w = 6 - $w;
        }

        return false;
    }

    public function compute(): void
    {
        $factor = 3;
        $q = sqrt($this->sieveSize);
    
        while ($factor <= $q) 
        {
            if (! $this->isComposite($factor)) {
                $start = (int)($factor * $factor / 2);
                for ($i = $start; $i < $this->rbs; $i += $factor)
                    $this->rawbits[$i] |= 1 << ($factor % 100);
            }

            $factor += 2;
        }
    }

    public function printResults(): void
    {
        echo $this, PHP_EOL;
    }

    public function count(): int 
    { 
        $sum = 1;
        for ($i = 3; $i <= $this->sieveSize; $i += 2) {
            if (! $this->isComposite($i))
                $sum++;
        }
        return $sum;
    }
    
    private const primeCounts = [
        10 => 4, 
        100 => 25, 
        1000 => 168, 
        10000 => 1229, 
        100_000 => 9592, 
        1000_000 => 78498,
        10_000_000 => 664579,
        100_000_000 => 5761455,
    ];
    
    public function valid(): bool 
    {
        $result = $this->count();
        $expected = self::primeCounts[$this->sieveSize] ?? -1;
        $valid = ($expected == $result);
        if (! $valid)
            echo "correct result should be: $expected, got: $result", PHP_EOL;
        return $valid;
    }
}