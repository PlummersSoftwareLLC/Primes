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
	NewSieve(10)
	wg.Add(1)
	PrimeChacker(1, 1, 3)
	if Sieve[0] || Sieve[1] || Sieve[2] {
		t.Errorf("%v", Sieve)
	}
	wg.Add(1)
	PrimeChacker(3, 3, 10)
	if Sieve[0] || Sieve[1] || Sieve[2] || !Sieve[3] || Sieve[4] || Sieve[5] || !Sieve[6] || Sieve[7] || Sieve[8] || !Sieve[9] {
		t.Errorf("%v", Sieve)
	}
}

func TestPrimeChackerMulti(t *testing.T) {
	NewSieve(10)
	PrimeChackerMulti(4, 1, 3)
	if Sieve[0] || Sieve[1] || Sieve[2] {
		t.Errorf("%v", Sieve)
	}
	PrimeChackerMulti(4, 3, 10)
	if Sieve[0] || Sieve[1] || Sieve[2] || !Sieve[3] || Sieve[4] || Sieve[5] || !Sieve[6] || Sieve[7] || Sieve[8] || !Sieve[9] {
		t.Errorf("%v", Sieve)
	}
}

func TestDoSieveMulti(t *testing.T) {
	for N, P := range numPrimesTest {
		DoSieveMulti(N, 4)
		C := CountPrimes()
		if C != P {
			t.Errorf("Up to %d there are %d primes, but I count %d.", N, P, C)
		}
	}
}

func ExampleDoSieveMulti() {
	DoSieveMulti(10, runtime.NumCPU())
	fmt.Print("2") // the only even prime
	for p := 0; p < len(Sieve); p++ {
		if Sieve[p] {
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
					DoSieveMulti(max, numProc)
				}
			})
		}
	}
}
