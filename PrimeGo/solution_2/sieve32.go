package main

import (
	"flag"
	"fmt"
	"math/bits"
	"time"
)

var label string = "ssovest-go-uint32"

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

type Bitarray []uint32

func (b Bitarray) SetSliceTrue(start, stop, step uint64) {
	var index, end uint64
	var mask uint32
	end = (stop + 31) / 32

	step2 := step * 2
	step3 := step * 3
	step4 := step * 4
	step5 := step * 5
	step6 := step * 6
	step7 := step * 7
	step8 := step * 8

	for i := 0; i < 32; i++ {
		mask = bits.RotateLeft32(1, int(start))
		index = start / 32
		start += step

		for ; index+step8 < end; index += step8 {
			b[index] |= mask
			b[index+step] |= mask
			b[index+step2] |= mask
			b[index+step3] |= mask
			b[index+step4] |= mask
			b[index+step5] |= mask
			b[index+step6] |= mask
			b[index+step7] |= mask
		}

		for ; index < end; index += step {
			b[index] |= mask
		}
	}
}

func (b Bitarray) Find(val bool, start, stop uint64) uint64 {
	for start < stop && val != (b[start/32]&bits.RotateLeft32(1, int(start)) != 0) {
		start++
	}
	return start
}

func (b Bitarray) Count(val bool, start, stop uint64) uint64 {
	var count uint64
	for ; start < stop; start++ {
		if val == (b[start/32]&bits.RotateLeft32(1, int(start)) != 0) {
			count++
		}
	}
	return count
}

type Sieve struct {
	bits Bitarray
	size uint64
}

func (s Sieve) RunSieve() {
	var factor, start, stop, step uint64
	stop = (s.size + 1) / 2
	for {
		factor = s.bits.Find(false, factor+1, stop)

		start = 2 * factor * (factor + 1)
		step = factor*2 + 1

		// start is factor squared, so it's the same as factor <= q
		if start >= stop {
			break
		}

		s.bits.SetSliceTrue(start, stop, step)
	}
}

func (s Sieve) CountPrimes() uint64 {
	return s.bits.Count(false, 0, (s.size+1)/2)
}

func (s Sieve) ValidateResults() bool {
	h, ok := primeCounts[s.size]
	return ok && h == s.CountPrimes()
}

func main() {
	var limit uint64
	var duration time.Duration
	var verbose bool
	var sieve Sieve

	flag.Uint64Var(&limit, "limit", 1000000, "limit")
	flag.DurationVar(&duration, "time", 5*time.Second, "duration")
	flag.BoolVar(&verbose, "v", false, "verbose output")
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
			sieve = Sieve{make(Bitarray, (limit+63)/64), limit}
			sieve.RunSieve()
			passes++
		}
	}

	timeDelta := time.Since(start).Seconds()
	if verbose {
		avg := float64(timeDelta) / float64(passes)
		count := sieve.CountPrimes()
		valid := sieve.ValidateResults()
		fmt.Printf("Passes: %v, Time: %v, Avg: %v, Limit: %v, Count: %v, Valid: %v\n\n", passes, timeDelta, avg, limit, count, valid)
	}
	fmt.Printf("%v;%v;%v;1;algorithm=base,faithful=yes,bits=1\n", label, passes, timeDelta)
}
