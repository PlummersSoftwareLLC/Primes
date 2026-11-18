<?php

declare(strict_types=1);

require __DIR__ . '/WheelSieve.php';

$limits = [
    ...range(0, 450), 1_680, 1_681, 1_682, 9_409, 10_200, 10_201, 10_202,
    30_599, 68_921, 999_983, 1_000_000, 1_030_301, 3_000_000, 10_000_000,
];
foreach ($limits as $limit) {
    $reference = str_repeat('1', $limit + 1);
    $reference[0] = '0';
    if ($limit >= 1) {
        $reference[1] = '0';
    }
    for ($p = 2; $p * $p <= $limit; ++$p) {
        if ($reference[$p] === '1') {
            for ($i = $p * $p; $i <= $limit; $i += $p) {
                $reference[$i] = '0';
            }
        }
    }

    for ($pass = 0; $pass < 2; ++$pass) {
        $sieve = new WheelSieve($limit);
        if ($sieve->isPrime(-1) || $sieve->isPrime($limit + 1)) {
            throw new RuntimeException("Out-of-range result at limit $limit");
        }
        if ($sieve->count() !== substr_count($reference, '1')) {
            throw new RuntimeException("Wrong count at limit $limit");
        }
        for ($n = 0; $n <= $limit; ++$n) {
            if (($reference[$n] === '1') !== $sieve->isPrime($n)) {
                throw new RuntimeException("Wrong flag for $n at limit $limit on pass $pass");
            }
        }
    }
}
echo "All sieve flags verified.\n";
