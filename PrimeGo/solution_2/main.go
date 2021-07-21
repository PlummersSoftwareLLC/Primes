package main

import (
	"flag"
	"fmt"
	"math"
	"math/bits"
	"time"
)

var primeCounts = map[uint64]uint64{
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
	size uint64
}

func (s Sieve) RunSieve() {
	var factor, start, step uint64
	end := (s.size + 1) / 2
	q := uint64(math.Sqrt(float64(s.size)) / 2)

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

func (s Sieve) CountPrimes() uint64 {
	t := uint64(0)
	end := (s.size + 1) / 2
	for i := uint64(0); i < end; i++ {
		t += uint64(bits.RotateLeft8(^s.bits[i/8], -int(i)) & 1)
	}
	return t
}

func (s Sieve) ValidateResults() bool {
	h, ok := primeCounts[s.size]
	return ok && h == s.CountPrimes()
}

func main() {
	var limit uint64
	var duration time.Duration
	var sieve Sieve

	flag.Uint64Var(&limit, "limit", 1000000, "limit")
	flag.DurationVar(&duration, "time", 5*time.Second, "duration")
	flag.Parse()

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
	fmt.Printf("ssovest-go;%v;%v;1;algorithm=base,faithful=yes,bits=1\n", passes, time.Since(start).Seconds())
}
