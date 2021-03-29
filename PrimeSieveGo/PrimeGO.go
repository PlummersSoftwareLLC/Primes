// ---------------------------------------------------------------------------
// PrimeGO.go : Dave's Garage Prime Sieve in Go
// ---------------------------------------------------------------------------
package main

import (
	"fmt"
	"log"
	"math"
	"time"
)

type PrimeSieve struct {
	sieveSize   int
	bits        []uint8
	primeCounts map[int]int
}

func NewPrimeSieve(sieveSize int) *PrimeSieve {
	bits := make([]uint8, sieveSize/8+1)
	for i := range bits {
		bits[i] = 0xFF
	}
	return &PrimeSieve{
		sieveSize: sieveSize,
		bits:      bits,
		primeCounts: map[int]int{
			10:          4,
			100:         25,
			1000:        168,
			10000:       1229,
			100000:      9592,
			1000000:     78498,
			10000000:    664579,
			100000000:   5761455,
			1000000000:  50847534,
			10000000000: 455052511,
		},
	}
}

func (p *PrimeSieve) getBit(index int) bool {
	if index%2 == 0 {
		return false
	}
	index = index / 2
	return ((p.bits[index/8]) & (1 << (index % 8))) != 0
}

func (p *PrimeSieve) clearBit(index int) {
	if index%2 == 0 {
		fmt.Println("You're clearing even bits, which is sub-optimal.")
		return
	}
	index = index / 2
	p.bits[index/8] &^= (1 << (index % 8))
}

func (p *PrimeSieve) validateResults() bool {
	if _, ok := p.primeCounts[p.sieveSize]; !ok {
		return false
	}
	return p.primeCounts[p.sieveSize] == p.CountPrimes()
}

func (p *PrimeSieve) RunSieve() {
	factor := 3
	q := int(math.Sqrt(float64(p.sieveSize)))

	for factor < q {
		for num := factor; num < p.sieveSize; num += 1 {
			if p.getBit(num) {
				factor = num
				break
			}
		}

		for num := factor * 3; num < int(p.sieveSize); num += factor * 2 {
			p.clearBit(num)
		}

		factor += 2
	}
}

func (p *PrimeSieve) PrintResults(showResults bool, duration time.Duration, passes int) {
	if showResults {
		fmt.Printf("2, ")
	}

	count := 1
	for num := 3; num <= p.sieveSize; num += 1 {
		if p.getBit(num) {
			if showResults {
				fmt.Printf("%d, ", num)
			}
			count++
		}
	}

	if showResults {
		fmt.Println()
	}

	log.Printf("Passes: %d, Time: %.6f, Avg: %.6f, Limit: %d, Count1: %d, Count2: %d, Valid: %v\n",
		passes,
		float64(duration)/float64(time.Second),
		float64(duration)/float64(time.Second)/float64(passes),
		p.sieveSize,
		count,
		p.CountPrimes(),
		p.validateResults())
}

func (p *PrimeSieve) CountPrimes() int {
	count := 0
	for i := 0; i < p.sieveSize; i += 1 {
		if p.getBit(i) {
			count += 1
		}
	}
	return count
}

func main() {
	passes := 0
	var sieve *PrimeSieve

	tStart := time.Now()
	for time.Since(tStart) < (10 * time.Second) {
		sieve = NewPrimeSieve(1000000)
		sieve.RunSieve()
		passes += 1
	}
	tD := time.Since(tStart)

	if sieve != nil {
		sieve.PrintResults(false, tD, passes)
	}
}
