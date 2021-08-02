package main

import (
	"flag"
	"fmt"
	"math/bits"
	"time"
	"unsafe"
)

var label string = "ssovest-go-other"

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

type Bitarray []uint64

func NewBitarray(length uintptr) Bitarray {
	return make(Bitarray, (length+63)/64)
}

func (b Bitarray) SetSliceTrue(start, stop, step uintptr) {
	var index, next, end uintptr
	var mask uint64
	begin := unsafe.Pointer(&b[0])
	end = (stop + 63) / 64

	next = start / 64
	mask = 0
	index = next

	for next == index {
		mask |= bits.RotateLeft64(1, int(start))
		start += step
		next = start / 64
	}

	*(*uint64)(unsafe.Pointer(uintptr(begin) + index*8)) |= mask

	i := 0
	for i < 64 && next < end {

		mask = 0
		index = next

		for next == index {
			mask |= bits.RotateLeft64(1, int(start))
			i++
			start += step
			next = start / 64
		}

		for ; index < end; index += step {
			*(*uint64)(unsafe.Pointer(uintptr(begin) + index*8)) |= mask
		}
	}
}

func (b Bitarray) Find(val bool, start, stop uintptr) uintptr {
	begin := unsafe.Pointer(&b[0])
	for start < stop && val != ((*(*uint64)(unsafe.Pointer(uintptr(begin) + (start/64)*8))&bits.RotateLeft64(1, int(start))) != 0) {
		start++
	}
	return start
}

func (b Bitarray) Count(val bool, start, stop uintptr) uintptr {
	begin := unsafe.Pointer(&b[0])
	var count uintptr
	for ; start < stop; start++ {
		if val == ((*(*uint64)(unsafe.Pointer(uintptr(begin) + (start/64)*8)) & bits.RotateLeft64(1, int(start))) != 0) {
			count++
		}
	}
	return count
}

type Sieve struct {
	bits Bitarray
	size uintptr
}

func (s Sieve) RunSieve() {
	var factor, start, stop, step uintptr
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

func (s Sieve) CountPrimes() uintptr {
	return s.bits.Count(false, 0, (s.size+1)/2)
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
