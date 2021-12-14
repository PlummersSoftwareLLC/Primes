package main

import (
	"context"
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
	var routines uint

	flag.Uint64Var(&limit, "limit", 1000000, "limit")
	flag.DurationVar(&duration, "time", 5*time.Second, "duration")
	flag.UintVar(&routines, "routines", 1, "number of go routines (may exceed available threads)")
	flag.Parse()

	sieves := make([]Sieve, routines)
	bch := make(chan struct{}, routines-1)
	passes := 0

	t0 := time.Now()
	deadline := t0.Add(duration)
	ctx, cancel := context.WithDeadline(context.Background(), deadline)
	defer cancel()

	// Spawns n-1 additional go routines
	MAXR := int(routines - 1)
	for i := 0; i < MAXR; i++ {
		sieves[i+1] = Sieve{make([]uint8, (limit+15)/16), limit}
		go func(sieve Sieve) {
			for {
				select {
				case <-ctx.Done():
					return
				default:
					sieve.RunSieve()
					bch <- struct{}{}
				}
			}
		}(sieves[i+1])
	}

loop:
	for {
		select {
		case <-ctx.Done():
			break loop
		case <-bch:
			passes++
		default:
			sieves[0] = Sieve{make([]uint8, (limit+15)/16), limit}
			sieves[0].RunSieve()
			passes++
		}
	}
	fmt.Printf("zanicar-go;%v;%f;%v;algorithm=base,faithful=yes,bits=1\n", passes, time.Since(t0).Seconds(), routines)
}
