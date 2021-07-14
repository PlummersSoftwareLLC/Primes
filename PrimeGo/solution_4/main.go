// ---------------------------------------------------------------------------
// main.go : Prime sieve written in Go, for Dave's Garage drag race
// ---------------------------------------------------------------------------

package main

import (
	"flag"
	"fmt"
	"math"
	"math/bits"
	"time"
)

type sieve struct {
	bits []uint8
	size uint64
}

func (s *sieve) runSieve() {
	factor := 3
	q := int(math.Sqrt(float64(s.size)))

	for factor <= q {
		for num := factor; num < int(s.size); num += 2 {
			if s.getBit(num / 2) {
				factor = num
				break
			}
		}
		for num := factor * factor; num < int(s.size); num += factor * 2 {
			s.clearBit(num / 2)
		}
		factor += 2
	}
}

func (s *sieve) getBit(num int) bool {
	// which BYTE is it in?
	byte := num / 8
	// which position in the byte is the bit stored?
	position := num % 8
	return (s.bits[byte] & bits.RotateLeft8(1, position)) != 0
}

func (s *sieve) clearBit(num int) {
	// which BYTE is it in?
	byte := num / 8
	// which position in the byte is the bit stored?
	position := num % 8
	s.bits[byte] = bits.RotateLeft8(254, position) & s.bits[byte]
}

func (s *sieve) countPrimes() int {
	count := 0
	for _, v := range s.bits {
		count += bits.OnesCount8(v)
	}
	return count
}

func (s *sieve) validateResults() bool {
	result, ok := primesDictionary[s.size]
	if !ok {
		return false
	} else {
		return result == s.countPrimes()
	}
}

func (s *sieve) printResults(show bool, duration time.Duration, passes int) {
	if show {
		fmt.Printf("2, ")
	}

	count := 1
	for num := 3; num <= int(s.size); num += 2 {
		if s.getBit(num / 2) {
			count++
			if show {
				fmt.Printf("%d, ", num)
			}
		}
	}

	if show {
		fmt.Printf("\n")
	}

	fmt.Printf("Passes: %d, Time: %v, Avg: %fs, Limit: %d, Count1: %d, Count2: %d, Valid: %t\n",
		passes,
		duration,
		float64(duration.Seconds())/float64(passes),
		s.size,
		count,
		s.countPrimes(),
		s.validateResults())
	fmt.Printf("\n")
	fmt.Printf("jdemchukprime-go;%v;%v;1;algorithm=base,faithful=yes,bits=1\n", passes, duration.Seconds())
}

var primesDictionary = map[uint64]int{
	10:        4,
	100:       25,
	1000:      168,
	10000:     1229,
	100000:    9592,
	1000000:   78498,
	10000000:  664579,
	100000000: 5761455,
}

func main() {
	var limit int
	var duration time.Duration
	var lastsieve *sieve
	var verbose bool

	flag.IntVar(&limit, "limit", 1000000, "limit")
	flag.DurationVar(&duration, "time", 5*time.Second, "duration")
	flag.BoolVar(&verbose, "verbose", false, "verbose")
	flag.Parse()

	passes := 0

	// allocate the proper amount of bytes
	// we do not need to track even numbers
	// therefore we can half the number of bits to represent only primes
	bytes := (limit / 2) / 8
	if (limit/2)%8 != 0 {
		bytes = bytes + 1
	}

	start := time.Now()
	timer := time.NewTimer(duration)

loop:
	for {
		select {
		case <-timer.C:
			break loop
		default:
			s := &sieve{
				bits: make([]uint8, bytes),
				size: uint64(limit),
			}
			// initialize all bits to 1
			for i := range s.bits {
				s.bits[i] = 255
			}

			// set any extra unused bits at the end to 0
			s.bits[len(s.bits)-1] = s.bits[len(s.bits)-1] >> ((8 * bytes) - (limit / 2))
			s.runSieve()
			lastsieve = s
			passes++
		}
	}

	lastsieve.printResults(verbose, time.Since(start), passes)
}
