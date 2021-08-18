package main

import (
	"flag"
	"fmt"
	"math"
	"math/bits"
	"time"
)

var label string = "ssovest-go-u8-B"

var primeCounts = map[uintptr]uintptr{
	10:        4,
	100:       25,
	1000:      168,
	10000:     1229,
	100000:    9592,
	1000000:   78498,
	10000000:  664579,
	100000000: 5761455,
}

type Sieve struct {
	bits []uint8
	size uintptr
}

func (s Sieve) RunSieve() {
	var factor, start, step uintptr
	end := (s.size + 1) / 2
	q := uintptr(math.Sqrt(float64(s.size)) / 2)

	for factor = 1; factor <= q; factor++ {
		if (s.bits[factor/8] & bits.RotateLeft8(1, int(factor))) != 0 {
			continue
		}

		start = 2 * (factor*factor + factor)
		step = factor*2 + 1

		for ; start < end; start += step {
			s.bits[start/8] |= bits.RotateLeft8(1, int(start))
		}
	}
}

func (s Sieve) CountPrimes() uintptr {
	t := uintptr(0)
	end := (s.size + 1) / 2
	for i := uintptr(0); i < end; i++ {
		t += uintptr(bits.RotateLeft8(^s.bits[i/8], -int(i)) & 1)
	}
	return t
}

func (s Sieve) ValidateResults() bool {
	h, ok := primeCounts[s.size]
	return ok && h == s.CountPrimes()
}

func main() {
	var limit uintptr
	var l64 uint64
	var duration time.Duration
	var verbose bool
	var sieve Sieve

	flag.Uint64Var(&l64, "limit", 1000000, "limit")
	flag.DurationVar(&duration, "time", 5*time.Second, "duration")
	flag.BoolVar(&verbose, "v", false, "verbose output")
	flag.Parse()

	limit = uintptr(l64)
	stop := make(chan struct{})
	passes := 0
	start := time.Now()
	time.AfterFunc(duration, func() { stop <- struct{}{} })

loop:
	for {
		select {
		case <-stop:
			break loop
		default:
			sieve = Sieve{make([]uint8, (limit+15)/16), limit}
			sieve.RunSieve()
			passes++
		}
	}

	timeDelta := time.Since(start).Seconds()
	if verbose {
		avg := float64(timeDelta)/float64(passes)
		count := sieve.CountPrimes()
		valid := sieve.ValidateResults()
		fmt.Printf("Passes: %v, Time: %v, Avg: %v, Limit: %v, Count: %v, Valid: %v\n\n", passes, timeDelta, avg, limit, count, valid)
	}
	fmt.Printf("%v;%v;%v;1;algorithm=base,faithful=yes,bits=1\n", label, passes, timeDelta)
}
