<?php

declare(strict_types=1);

$workers = filter_var($argv[1] ?? null, FILTER_VALIDATE_INT, ['options' => ['min_range' => 1]]);
if ($workers === false) {
    fwrite(STDERR, "Usage: php parallel.php WORKERS\n");
    exit(2);
}

$children = [];
$passes = 0;
$start = hrtime(true);
try {
    for ($i = 0; $i < $workers; ++$i) {
        $process = proc_open([PHP_BINARY, __DIR__ . '/PrimePHP.php'], [1 => ['pipe', 'w']], $pipes);
        if ($process === false) {
            throw new RuntimeException('Could not start a sieve worker');
        }
        $children[] = [$process, $pipes[1]];
    }
    while ($children) {
        [$process, $output] = array_pop($children);
        $row = stream_get_contents($output);
        fclose($output);
        if (proc_close($process) !== 0 || !preg_match('/^LauJosefsen-wheel;([0-9]+);/', $row, $match)) {
            throw new RuntimeException('Sieve worker failed');
        }
        $passes += (int) $match[1];
    }
} finally {
    foreach ($children as [$process, $output]) {
        fclose($output);
        proc_close($process);
    }
}
printf("LauJosefsen-wheel-par;%d;%.9f;%d;algorithm=wheel,faithful=yes,bits=1\n",
    $passes, (hrtime(true) - $start) / 1e9, $workers);
