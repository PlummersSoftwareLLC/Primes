package main

import (
	"flag"
	"fmt"
	"math/bits"
	"time"
)

/*
#cgo CFLAGS: -Ofast -march=native -mtune=native
#include <stdint.h>
#include <math.h>

void clearbits(uint32_t* bits, size_t start, size_t end, size_t step) {
	uint32_t mask;
	size_t index;

	end = (end + 31) / 32;
	size_t next = start / 32;
	int i = 0;
	while (i < 32 && next < end) {
		mask = 0;
		index = next;

		while (next == index) {
			mask |= (1 << (start % 32));
			++i;
			start += step;
			next = start / 32;
		}

		for (; index < end; index += step) {
			bits[index] |= mask;
		}
	}
}

void run(uint32_t* bits, size_t limit) {
	size_t factor, start, step;
	size_t end = (limit + 1) / 2;
	size_t q = sqrt(limit) / 2;

	for (factor = 1; factor <= q; ++factor) {
		if (bits[factor/32] & (1 << (factor % 32))) {
			continue;
		}

		start = 2 * (factor*factor + factor);
		step = factor*2 + 1;

		clearbits(bits, start, end, step);
	}
}
*/
import "C"

var label string = "ssovest-cgo"

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
	bits []uint32
	size uintptr
}

func (s Sieve) RunSieve() {
	C.run((*C.uint32_t)(&s.bits[0]), C.size_t(s.size))
}

func (s Sieve) CountPrimes() uintptr {
	t := uintptr(0)
	end := (s.size + 1) / 2
	for i := uintptr(0); i < end; i++ {
		t += uintptr(bits.RotateLeft32(^s.bits[i/32], -int(i)) & 1)
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
			sieve = Sieve{make([]uint32, (limit+63)/64), limit}
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
	fmt.Printf("%v;%v;%v;1;algorithm=other,faithful=no,bits=1\n", label, passes, timeDelta)
}
