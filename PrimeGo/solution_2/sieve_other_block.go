package main

import (
	"flag"
	"fmt"
	"math/bits"
	"time"
)

var label string = "ssovest-go-other-blocks-B"

var blockSize uint64 = 600_000

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

type Bitarray []uint64

func NewBitarray(length uint64) Bitarray {
	return make(Bitarray, (length+63)/64)
}

func (b Bitarray) SetSliceTrue(start, stop, step uint64) {
	var index, next, end, b8e, blockStart, blockEnd uint64
	var mask uint64
	end = (stop + 63) / 64

	indices := [64]uint64{}
	masks := [64]uint64{}
	idx := 0

	step2 := step * 2
	step3 := step * 3
	step4 := step * 4
	step5 := step * 5
	step6 := step * 6
	step7 := step * 7
	step8 := step * 8

	next = start / 64
	mask = 0
	index = next

	for next == index {
		mask |= bits.RotateLeft64(1, int(start))
		start += step
		next = start / 64
	}
	b[index] |= mask

	
	for i := 0; i < 64; {
		indices[idx] = next
		for next == indices[idx] {
			masks[idx] |= bits.RotateLeft64(1, int(start))
			i++
			start += step
			next = start / 64
		}
		idx++
	}

	for blockEnd < end {
		blockEnd += blockSize
		if blockEnd > end {
			blockEnd = end
		}

		b8e = blockEnd - step8
		if step8 > blockEnd {
			b8e = 0
		}

		for i, mask := range masks[:idx] {
			index = indices[i]

			for ; index < b8e; index += step8 {
				b[index] |= mask
				b[index+step] |= mask
				b[index+step2] |= mask
				b[index+step3] |= mask
				b[index+step4] |= mask
				b[index+step5] |= mask
				b[index+step6] |= mask
				b[index+step7] |= mask
			}

			for ; index+step2 < blockEnd; index += step2 {
				b[index] |= mask
				b[index+step] |= mask
			}

			for ; index < blockEnd; index += step {
				b[index] |= mask
			}

			indices[i] = index
		}

		blockStart += blockSize
	}

}

func (b Bitarray) Find(val bool, start, stop uint64) uint64 {
	for start < stop && val != (b[start/64]&bits.RotateLeft64(1, int(start)) != 0) {
		start++
	}
	return start
}

func (b Bitarray) Count(val bool, start, stop uint64) uint64 {
	var count uint64
	for ; start < stop; start++ {
		if val == (b[start/64]&bits.RotateLeft64(1, int(start)) != 0) {
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
	var limit, bsize uint64
	var duration time.Duration
	var verbose bool
	var sieve Sieve

	flag.Uint64Var(&limit, "limit", 1_000_000, "limit")
	flag.Uint64Var(&bsize, "block", 128_000, "block size")
	flag.DurationVar(&duration, "time", 5*time.Second, "duration")
	flag.BoolVar(&verbose, "v", false, "verbose output")

	flag.Parse()

	if bsize == 0 {
		bsize = 128_000
	}
	blockSize = bsize / 32

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
			sieve = Sieve{NewBitarray((limit + 1) / 2), limit}
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

	fmt.Printf("%v;%v;%v;1;algorithm=other,faithful=yes,bits=1\n", label, passes, timeDelta)
}
