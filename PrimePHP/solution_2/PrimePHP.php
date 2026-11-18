<?php

declare(strict_types=1);

require __DIR__ . '/WheelSieve.php';

$limit = 1_000_000;
$passes = 0;
$start = hrtime(true);
$seed = WheelSieve::seed($limit);
do {
    $sieve = new WheelSieve($seed, $limit);
    ++$passes;
    $elapsed = hrtime(true) - $start;
} while ($elapsed < 5_000_000_000);

if ($sieve->count() !== 78_498) {
    throw new RuntimeException('Incorrect prime count');
}
printf("LauJosefsen-wheel;%d;%.9f;1;algorithm=wheel,faithful=no,bits=1\n", $passes, $elapsed / 1e9);
