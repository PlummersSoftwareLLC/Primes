<?php declare(strict_types=1);

require_once 'PrimeSieve.php';

function main(array $args): void
{
    $size = 1_000_000; 
    $start = microtime(true);       
    $passes = 0;                            
    $printResults = false;
    $onceOnly = false;
            
    foreach ($args as $a) {
        if (str_starts_with(haystack:$a, needle:'--size=')) {
            $size = max(0, (int)substr($a, strlen('--size=')));
        } 
        else if ($a == '--print') {
            $printResults = true;
        }
        else if ($a == '--once') {
            $onceOnly = true;
        }
    }
            
    $duration = 0;               
    do
    {
        $sieve = new PrimeSieve($size);
        $sieve->compute();
        $passes++;
        $duration = microtime(true) - $start;
    }
    while ($duration < 5 && ! $onceOnly);
    
    $count = count($sieve);
    
    if ($printResults) {
        $sieve->printResults();
    }

    //Print the results
    $avg = number_format($duration / $passes, 2);
    $dstr = number_format($duration, 2);
    $valid = $sieve->valid() ? 'True' : 'False';
    if ($valid == 'False') 
    {
        $expected = getPreparedResults($size);
        if ($expected)
        {
            $actual = $sieve->primes();
            $missing = array_diff($expected, $actual);
            if (count($missing) > 0)
                echo "## MISSING: The following primes are missing from the sieve: ", implode(',', $missing), PHP_EOL;
        
            $falsePrimes = array_diff($actual, $expected);
            if (count($falsePrimes) > 0)
                echo "## INCORRECT PRIMES: The following numbers were miscalculated as primes: ", implode(',', $falsePrimes), PHP_EOL;
        }
    }
    echo "Passes: $passes, Time: {$dstr}, Avg: $avg, Limit: $size, Count: $count, Valid: $valid", PHP_EOL;

    // Following 2 lines added by rbergen to conform to drag race output format
    echo PHP_EOL, PHP_EOL;
    echo "sqonk;$passes;$dstr;1;algorithm=other,faithful=no,bits=1", PHP_EOL;
}


main($argv);

