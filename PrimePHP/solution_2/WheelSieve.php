<?php

declare(strict_types=1);

final class WheelSieve
{
    // One native word covers the residues coprime to 210 (48 bits), or 30 (8 bits).
    private const WHEEL = PHP_INT_SIZE === 8 ? 210 : 30;
    // Rounding p/WHEEL up at this precision adds less than 1/WHEEL after multiplying by any residue.
    private const SHIFT = PHP_INT_SIZE === 8 ? 16 : 10;
    private const RESIDUES = PHP_INT_SIZE === 8 ? [
        1, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47,
        53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103,
        107, 109, 113, 121, 127, 131, 137, 139, 143, 149, 151, 157,
        163, 167, 169, 173, 179, 181, 187, 191, 193, 197, 199, 209,
    ] : [1, 7, 11, 13, 17, 19, 23, 29];

    private readonly array $words;
    private readonly array $masks;

    public function __construct(private readonly int $limit)
    {
        $masks = array_fill(0, self::WHEEL, 0);
        foreach (self::RESIDUES as $bit => $residue) {
            $masks[$residue] = 1 << $bit;
        }
        $this->masks = $masks;
        $size = intdiv($limit, self::WHEEL) + 1;

        $a = $this->pairPattern(11, 37);
        $b = $this->pairPattern(13, 31);
        $c = $this->pairPattern(17, 29);
        $d = $this->pairPattern(19, 23);
        $words = array_fill(0, $size, 0);
        for ($i = 0; $i < $size;) {
            $ia = $i % (11 * 37);
            $ib = $i % (13 * 31);
            $ic = $i % (17 * 29);
            $id = $i % (19 * 23);
            // All four pattern indices can advance together until the first wraps.
            $end = $i + min($size - $i, 11 * 37 - $ia, 13 * 31 - $ib, 17 * 29 - $ic, 19 * 23 - $id);
            for (; $i < $end; ++$i) {
                $words[$i] = $a[$ia++] | $b[$ib++] | $c[$ic++] | $d[$id++];
            }
        }
        if (self::WHEEL === 30) {
            foreach ($this->primePattern(7) as $start => $mask) {
                for ($i = $start; $i < $size; $i += 7) {
                    $words[$i] |= $mask;
                }
            }
        }

        // The divisibility patterns include the presieving primes themselves.
        foreach ([7, 11, 13, 17, 19, 23, 29, 31, 37] as $p) {
            if ($p <= $limit) {
                $words[intdiv($p, self::WHEEL)] &= ~$masks[$p % self::WHEEL];
            }
        }
        $words[0] |= $masks[1];
        $tail = $limit % self::WHEEL;
        foreach (self::RESIDUES as $residue) {
            if ($residue > $tail) {
                $words[$size - 1] |= $masks[$residue];
            }
        }

        // An index-controlled pair loop miscounted at limit 4,831 with PHP 8.5.12 tracing JIT.
        $pairStarts = range(0, intdiv(count(self::RESIDUES), 2) - 2, 2);
        for ($p = 41; $p * $p <= $limit; $p += 2) {
            $remainder = $p % self::WHEEL;
            $flag = $masks[$remainder];
            $block = intdiv($p, self::WHEEL);
            if (!$flag || ($words[$block] & $flag)) {
                continue;
            }
            // If p*r lands at offset k, p*(WHEEL-r) lands at p-1-k.
            $base = $block * $p;
            $scaled = intdiv(($p << self::SHIFT) + self::WHEEL - 1, self::WHEEL);
            foreach ($pairStarts as $j) {
                $r = self::RESIDUES[$j];
                $aOffset = ($scaled * $r) >> self::SHIFT;
                $aResidue = $p * $r - $aOffset * self::WHEEL;
                $aBit = $masks[$aResidue];
                $aMirror = $masks[self::WHEEL - $aResidue];

                $r = self::RESIDUES[$j + 1];
                $bOffset = ($scaled * $r) >> self::SHIFT;
                $bResidue = $p * $r - $bOffset * self::WHEEL;
                $bBit = $masks[$bResidue];
                $bMirror = $masks[self::WHEEL - $bResidue];

                $gap = $bOffset - $aOffset;
                $far = $p - 1 - 2 * $aOffset;
                $near = $far - $gap;
                for ($i = $base + $aOffset, $end = $size - $far; $i < $end; $i += $p) {
                    $words[$i] |= $aBit;
                    $words[$i + $gap] |= $bBit;
                    $words[$i + $near] |= $bMirror;
                    $words[$i + $far] |= $aMirror;
                }
                if ($i < $size) {
                    $words[$i] |= $aBit;
                }
                if ($i + $gap < $size) {
                    $words[$i + $gap] |= $bBit;
                }
                if ($i + $near < $size) {
                    $words[$i + $near] |= $bMirror;
                }
            }
            // The first wheel block includes cofactor 1.
            $words[$block] &= ~$flag;
        }
        $this->words = $words;
    }

    private function primePattern(int $p): array
    {
        $pattern = array_fill(0, $p, 0);
        foreach (self::RESIDUES as $residue) {
            $product = $p * $residue;
            $pattern[intdiv($product, self::WHEEL)] |= $this->masks[$product % self::WHEEL];
        }
        return $pattern;
    }

    private function pairPattern(int $p, int $q): array
    {
        $first = $this->primePattern($p);
        $second = $this->primePattern($q);
        $pattern = [];
        for ($i = 0, $end = $p * $q; $i < $end; ++$i) {
            $pattern[] = $first[$i % $p] | $second[$i % $q];
        }
        return $pattern;
    }

    public function isPrime(int $n): bool
    {
        if ($n < 2 || $n > $this->limit) {
            return false;
        }
        $mask = $this->masks[$n % self::WHEEL];
        if ($mask === 0) {
            return $n === 2 || $n === 3 || $n === 5 || ($n === 7 && self::WHEEL === 210);
        }
        return ($this->words[intdiv($n, self::WHEEL)] & $mask) === 0;
    }

    public function count(): int
    {
        $count = (int) ($this->limit >= 2) + (int) ($this->limit >= 3) + (int) ($this->limit >= 5)
            + (int) ($this->limit >= 7 && self::WHEEL === 210);
        foreach ($this->words as $word) {
            $count += count(self::RESIDUES) - substr_count(decbin($word), '1');
        }
        return $count;
    }
}
