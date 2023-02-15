<?php declare(strict_types=1);

require_once __DIR__.'/../PrimeSieve.php';

use PHPUnit\Framework\TestCase;

class PrimeSieveCountsTest extends TestCase
{
    protected function runSieve(int $size, ?array $expectedOutput = null): void 
    {
        $sieve = new PrimeSieve($size);
        $sieve->compute();
        
        $this->assertTrue($sieve->valid());
        if ($expectedOutput)
            $this->assertEquals(expected:$expectedOutput, actual:$sieve->primes());
    }
    
    public function testSieveSize10(): void 
    {
        $this->runSieve(10, [3,5,7]);
    }
    
    public function testSieveSize100(): void 
    {
        $this->runSieve(100, [
            3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97
        ]);
    }
    
    public function testSieveSize1000(): void 
    {
        $this->runSieve(1000, [
            3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997
        ]);
    }
    
    public function testSieveSize10000(): void 
    {
        $this->runSieve(10000);
    }
    
    public function testSieveSize100000(): void 
    {
        $this->runSieve(100000);
    }
    
    public function testSieveSize1000000(): void 
    {
        $this->runSieve(1000000);
    }
    
    public function testSieveSize10000000(): void
    {
        $this->runSieve(10000000);
    }

    public function testSieveSize100000000(): void
    {
        $this->runSieve(100000000);
    }
}