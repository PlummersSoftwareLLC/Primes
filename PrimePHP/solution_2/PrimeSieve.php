<?php declare(strict_types=1);

define('BITS_PER_BYTE', 8);
define('BITS_PER_INT', (BITS_PER_BYTE * PHP_INT_SIZE) - 1);

function getPreparedResults(int $size): ?array
{
    $file = __DIR__."/tests/expected/$size.txt";
    if (! file_exists($file))
        return null;
    return explode(',', file_get_contents($file));
}

class PrimeSieve implements Countable
{
    private array $sieve;

    private readonly int $int_count;
    private readonly int $half_n;
    
    public function __construct(private readonly int $sieveSize)
    {
        $this->half_n = (int)(($sieveSize + 1) >> 1);
        $this->int_count = (int)(($sieveSize + 1) / BITS_PER_INT) + 1;
        $this->sieve = array_fill(0, $this->int_count, 0);
    }

    public function __tostring(): string 
    {
        return implode(',', $this->primes());
    }
    
    public function primes(): array 
    {
        $primes = [];
        for ($i = 1; $i < $this->sieveSize; $i++)
        {
            if ($i % 2) 
            {
                $sieveIndex = (int)($i >> 1);
                $byteIndex = (int)($sieveIndex / BITS_PER_INT);
                $bitIndex = $sieveIndex % BITS_PER_INT;
                if (! ($this->sieve[$byteIndex] & (1 << $bitIndex)))
                    $primes[] = $i;
            }
        }
        return $primes;
    }

    private function isComposite(int $n): bool
    {
        if ($n <= 1) {
            return true;
        }

        if ($n == 2 || $n == 3) {
            return false;
        }

        $i = 3;
        $sqrt = (int)sqrt($n);

        while ($i <= $sqrt) {
            if ($n % $i == 0) {
                return true;
            }
            $i += 2;
        }

        return false;
    }

    public function compute(): void
    {
        $factor = 3;
        $q = sqrt($this->sieveSize);
        $intIdx = 0;
        $nextEnd = BITS_PER_INT-1;
        while ($factor <= $q)
        {
            if (! $this->isComposite($factor))
            {
                $start = (int)($factor * $factor / 2);
                $iiIdx = $intIdx;
                $iiEnd = $nextEnd;
                for ($i = $start; $i < $this->half_n; $i += $factor) 
                {
                    while ($iiEnd < $i) {
                        $iiIdx++;
                        $iiEnd += BITS_PER_INT;
                    }
                    $this->sieve[$iiIdx] |= 1 << ($i % BITS_PER_INT);
                }
            }

            $factor += 2;
            if ($factor > $nextEnd) {
                $intIdx++;
                $nextEnd += BITS_PER_INT;
            }
        }
    }

    public function printResults(): void
    {
        echo $this, PHP_EOL;
    }

    public function count(): int 
    { 
        return count($this->primes());
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