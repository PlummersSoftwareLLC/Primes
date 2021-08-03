package main

import (
	"fmt"
	"runtime"
	"testing"
)

var numPrimesTest = map[int]int{
	10:  4,
	1e2: 25,
	1e3: 168,
	1e4: 1229,
	1e5: 9592,
	1e6: 78498,
	1e7: 664579,
	1e8: 5761455,
}

func TestPrimeChecker(t *testing.T) {
	sieve := NewSieve(Pr(10))
	sieve.PrimeChecker(1, 1, 3)
	if sieve[0] || sieve[1] || sieve[2] {
		t.Errorf("%v", sieve)
	}
	sieve.PrimeChecker(3, 3, 10)
	if sieve[0] || sieve[1] || sieve[2] || !sieve[3] || sieve[4] || sieve[5] || !sieve[6] || sieve[7] || sieve[8] || !sieve[9] {
		t.Errorf("%v", sieve)
	}
}

func TestPrimeCheckerMulti(t *testing.T) {
	sieve := NewSieve(Pr(10))
	sieve.PrimeCheckerMulti(4, 1, 3)
	if sieve[0] || sieve[1] || sieve[2] {
		t.Errorf("%v", sieve)
	}
	sieve.PrimeCheckerMulti(4, 3, 10)
	if sieve[0] || sieve[1] || sieve[2] || !sieve[3] || sieve[4] || sieve[5] || !sieve[6] || sieve[7] || sieve[8] || !sieve[9] {
		t.Errorf("%v", sieve)
	}
}

func TestBuildSieve(t *testing.T) {
	for N, P := range numPrimesTest {
		sieve := NewSieve(N)
		sieve.Build(4)
		C := sieve.CountPrimes()
		if C != P {
			t.Errorf("Up to %d there are %d primes, but I count %d.", N, P, C)
		}
	}
}

func ExampleBuildSieve() {
	sieve := NewSieve(10)
	sieve.Build(runtime.NumCPU())
	fmt.Print("2") // the only even prime
	for p := 0; p < len(sieve); p++ {
		if sieve[p] {
			continue
		}
		fmt.Print(",", 2*p+3)
	}
	// Output: 2,3,5,7
}

var numPrimesBench = map[int]int{
	// 10:  4,
	// 1e2: 25,
	// 1e3: 168,
	1e4: 1229,
	1e5: 9592,
	1e6: 78498,
	1e7: 664579,
	// 1e8: 5761455,
}

func BenchmarkBuildSieve(b *testing.B) {
	for max, _ := range numPrimesBench {
		for _, numProc := range []int{runtime.NumCPU() / 2, runtime.NumCPU(), 2 * runtime.NumCPU(), 3 * runtime.NumCPU(), 4 * runtime.NumCPU()} {
			b.Run(fmt.Sprintf("Multi %d using %d workers", max, numProc), func(b *testing.B) {
				for i := 0; i < b.N; i++ {
					sieve := NewSieve(max)
					sieve.Build(numProc)
				}
			})
		}
	}
}
