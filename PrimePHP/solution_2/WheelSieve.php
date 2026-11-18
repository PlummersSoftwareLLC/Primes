<?php

declare(strict_types=1);

final class WheelSieve
{
    private const BITS = PHP_INT_SIZE * 8;
    private const MASK = self::BITS - 1;
    // Each word holds one bit per odd number, spanning twice the native word width.
    private const SHIFT = PHP_INT_SIZE === 8 ? 7 : 6;

    private readonly array $words;

    public static function seed(int $limit): array
    {
        $odds = ($limit + 1) >> 1;
        if ($odds === 0) {
            return [];
        }

        $words = array_fill(0, intdiv($odds + self::MASK, self::BITS), 0);
        $tail = $odds & self::MASK;
        if ($tail !== 0) {
            $words[count($words) - 1] = ~(PHP_INT_MAX >> (self::MASK - $tail));
        }
        $words[0] |= 1; // Exclude 1.
        for ($p = 3; $p <= 97 && $p * $p <= $limit; $p += 2) {
            if ((($words[$p >> self::SHIFT] >> (($p >> 1) & self::MASK)) & 1) !== 0) {
                continue;
            }
            for ($n = $p * $p; $n <= $limit; $n += 2 * $p) {
                $words[$n >> self::SHIFT] |= 1 << (($n >> 1) & self::MASK);
            }
        }
        return $words;
    }

    public function __construct(array $words, private readonly int $limit)
    {
        // After presieving, each remaining composite has factors p <= q with q <= limit / 101.
        $cofactors = [];
        $max = intdiv($limit, 101);
        for ($q = 101; $q <= $max; $q += 2) {
            if ((($words[$q >> self::SHIFT] >> (($q >> 1) & self::MASK)) & 1) === 0) {
                $cofactors[] = $q;
            }
        }

        $last = count($cofactors) - 1;
        foreach ($cofactors as $first => $p) {
            if ($p * $p > $limit) {
                break;
            }
            if ((($words[$p >> self::SHIFT] >> (($p >> 1) & self::MASK)) & 1) !== 0) {
                continue;
            }
            while ($p * $cofactors[$last] > $limit) {
                --$last;
            }
            $stop = $last - 3;
            for ($k = $first; $k <= $stop; $k += 4) {
                $n = $p * $cofactors[$k];
                $words[$n >> self::SHIFT] |= 1 << (($n >> 1) & self::MASK);
                $n = $p * $cofactors[$k + 1];
                $words[$n >> self::SHIFT] |= 1 << (($n >> 1) & self::MASK);
                $n = $p * $cofactors[$k + 2];
                $words[$n >> self::SHIFT] |= 1 << (($n >> 1) & self::MASK);
                $n = $p * $cofactors[$k + 3];
                $words[$n >> self::SHIFT] |= 1 << (($n >> 1) & self::MASK);
            }
            for (; $k <= $last; ++$k) {
                $n = $p * $cofactors[$k];
                $words[$n >> self::SHIFT] |= 1 << (($n >> 1) & self::MASK);
            }
        }
        $this->words = $words;
    }

    public function count(): int
    {
        $count = (int) ($this->limit >= 2);
        foreach ($this->words as $word) {
            $count += substr_count(decbin(~$word), '1');
        }
        return $count;
    }

    public function isPrime(int $n): bool
    {
        if ($n < 2 || $n > $this->limit || ($n & 1) === 0) {
            return $n === 2 && $n <= $this->limit;
        }
        return (($this->words[$n >> self::SHIFT] >> (($n >> 1) & self::MASK)) & 1) === 0;
    }
}
