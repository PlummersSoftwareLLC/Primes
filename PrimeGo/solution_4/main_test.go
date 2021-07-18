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
	// 1e6: 78498,
	// 1e7: 664579,
	// 1e8: 5761455,
}

func TestPrimeChacker(t *testing.T) {
	sieve := NewSieve(10)
	wg.Add(1)
	sieve.PrimeChacker(1, 1, 3)
	if sieve[0] || sieve[1] || sieve[2] {
		t.Errorf("%v", sieve)
	}
	wg.Add(1)
	sieve.PrimeChacker(3, 3, 10)
	if sieve[0] || sieve[1] || sieve[2] || !sieve[3] || sieve[4] || sieve[5] || !sieve[6] || sieve[7] || sieve[8] || !sieve[9] {
		t.Errorf("%v", sieve)
	}
}

func TestPrimeChackerMulti(t *testing.T) {
	sieve := NewSieve(10)
	sieve.PrimeChackerMulti(4, 1, 3)
	if sieve[0] || sieve[1] || sieve[2] {
		t.Errorf("%v", sieve)
	}
	sieve.PrimeChackerMulti(4, 3, 10)
	if sieve[0] || sieve[1] || sieve[2] || !sieve[3] || sieve[4] || sieve[5] || !sieve[6] || sieve[7] || sieve[8] || !sieve[9] {
		t.Errorf("%v", sieve)
	}
}

func TestDoSieveMulti(t *testing.T) {
	for N, P := range numPrimesTest {
		sieve := DoSieveMulti(N, 4)
		C := sieve.CountPrimes()
		if C != P {
			t.Errorf("Up to %d there are %d primes, but I count %d.", N, P, C)
		}
	}
}

func ExampleDoSieveMulti() {
	sieve := DoSieveMulti(10, runtime.NumCPU())
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

func BenchmarkDoSieveMulti(b *testing.B) {
	for max, _ := range numPrimesBench {
		for i := 2; i < 5; i++ {
			numProc := 1 << i
			b.Run(fmt.Sprintf("Multi %d using %d workers", max, numProc), func(b *testing.B) {
				for i := 0; i < b.N; i++ {
					_ = DoSieveMulti(max, numProc)
				}
			})
		}
	}
}
