<?php
// ---------------------------------------------------------------------------
// PrimePHP.php : Prime Sieve in PHP (7.4 and 8.0) implementation
// (as close as possible to the C++ implementation)
// ---------------------------------------------------------------------------

class PrimeSieve {
    private int $sieveSize = 0;
    private array $rawbits;
    const myDict = [
        10 => 1,        
        100 => 25,
        1000 => 168,
        10000 => 1229,
        100000 => 9592,
        1000000 => 78498,
        10000000 => 664579,
        100000000 => 5761455
    ];
 
    private function validateResults() : bool
    {
        return (self::myDict[$this->sieveSize]??null) === $this->countPrimes();
    }

    private function GetBit(int $index): bool
    {
        if ($index % 2 == 0)
            return false;
        $index = $index / 2;
        return (($this->rawbits[$index / 8]) & (1 << ($index % 8))) != 0;
    }

    private function ClearBit(int $index)
    {
        if ($index % 2 == 0)
        {
            printf("You're setting even bits, which is sub-optimal.\n");
            return;
        }
        $index = $index / 2;
        $this->rawbits[$index / 8] &= ~(1 << ($index % 8));
    }

    function __construct(int $n)
    {
        $this->sieveSize = $n;
        $this->rawbits = array_fill(0, $n / 8 + 1, 0xff);
    }

    public function runSieve()
    {
        $factor = 3;
        $q = sqrt($this->sieveSize);

        while ($factor < $q)
        {
            for ($num = $factor; $num < $this->sieveSize; $num++)
            {
                if ($this->GetBit($num))
                {
                    $factor = $num;
                    break;
                }
            }
            for ($num = $factor * 3; $num < $this->sieveSize; $num += $factor * 2)
            $this->ClearBit($num);
             
            $factor += 2;
        }
    }


    public function printResults(bool $showResults, float $duration, int $passes)
    {
        if ($showResults)
            printf("2, ");

        $count = 1;
        for ($num = 3; $num <= $this->sieveSize; $num++)
        {
            if ($this->GetBit($num))
            {
                if ($showResults)
                    printf("%d, ", $num);
                $count++;
            }
        }

        if ($showResults)
            printf("\n");
        
        printf("Passes: %d, Time: %lf, Avg: %lf, Limit: %d, Count: %d, Valid: %d\n", 
               $passes, 
               $duration, 
               $duration / $passes, 
               $this->sieveSize, 
               $count, 
               $this->validateResults());
    }

    public function countPrimes(): int
    {
        $count = 0;
        for ($i = 0; $i < $this->sieveSize; $i++)
            if ($this->GetBit($i))
                $count++;
        return $count;
    }    
}

$tstart = microtime(true);
$passes = 0;

while((microtime(true) - $tstart) < 10.00) {
    $sieve = new PrimeSieve(1000000);
    $sieve->runSieve();
    $passes++;
}

$tD = microtime(true) - $tstart;
if ($sieve) {
    $sieve->printResults(false, $tD, $passes);
}
