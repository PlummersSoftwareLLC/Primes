package main

import (
	"context"
	"flag"
	"fmt"
	"log"
	"math"
	"math/bits"
	"time"
)

var primeCounts = map[uint64]uint64{
	10:             4,
	100:            25,
	1_000:          168,
	10_000:         1_229,
	100_000:        9_592,
	1_000_000:      78_498,
	10_000_000:     664_579,
	100_000_000:    57_61_455,
	10_000_000_000: 455_052_511,
}

type sieve struct {
	bits []uint8
	size uint64
}

func (s *sieve) RunSieve() {
	var factor, start, step uint64

	// We ignore all even numbers.
	end := (s.size + 1) / 2

	// The factors of the biggest number in the search range can't be bigger than
	//  its square root, so we only need to cross off multiples of factors smaller
	//  than this. Dividing by 2 because we're ignoring all even numbers.
	q := uint64(math.Sqrt(float64(s.size)) / 2)

	// The outer loop loops over potential factors of the numbers in the search range. The
	//  body of the loop marks off multiples of these factors as composite (non-prime) numbers.
	for factor = 1; factor <= q; factor++ {
		if (s.bits[factor/8] & bits.RotateLeft8(1, int(factor))) != 0 {
			continue
		}

		start = 2 * (factor * (factor + 1))
		step = 2*factor + 1

		for ; start < end; start += step {
			s.bits[start/8] |= bits.RotateLeft8(1, int(start))
		}
	}
}

func (s *sieve) CountPrimes() uint64 {
	t := uint64(0)
	end := (s.size + 1) / 2
	for i := uint64(0); i < end; i++ {
		t += uint64(bits.RotateLeft8(^s.bits[i/8], -int(i)) & 1)
	}
	return t
}

func (s *sieve) ListPrimes() []uint64 {
	res := []uint64{2}
	end := (s.size + 1) / 2
	for i := uint64(1); i < end; i++ {
		if bits.RotateLeft8(^s.bits[i/8], -int(i))&1 != 1 {
			continue
		}
		res = append(res, 1+2*i)
	}
	return res
}

func (s *sieve) ValidateResults() bool {
	h, ok := primeCounts[s.size]
	return ok && h == s.CountPrimes()
}

func main() {
	var limit uint64
	var duration time.Duration
	var s sieve

	flag.Uint64Var(&limit, "limit", 1000000, "limit")
	flag.DurationVar(&duration, "time", 5*time.Second, "duration")
	flag.Parse()

	passes := 0
	start := time.Now()
	ctx, cancel := context.WithTimeout(context.Background(), duration)
	defer cancel()

	for {
		if ctx.Err() != nil {
			break
		}

		s = sieve{make([]uint8, (limit+15)/16), limit}
		s.RunSieve()
		passes++
	}
	if !s.ValidateResults() {
		log.Fatal("failed to validate results")
	}
	fmt.Printf("ssovest-go;%v;%v;1;algorithm=base,faithful=yes,bits=1\n", passes, time.Since(start).Seconds())
}
