package main

import (
	"fmt"
	"math"
	"math/bits"
	"os"
	"time"

	"github.com/spf13/pflag"
)

// I found that more is not better. Sometimes fewer go routines, each routine running a loop of sieves gave better performance. You can adjust the number of go routines and the number of loops each go routine runs.

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

var loops int

func (s Sieve) RunSieve(doneprime chan<- bool) {
	var factor, start, step uint64
	end := (s.size + 1) / 2
	q := uint64(math.Sqrt(float64(s.size)) / 2)
	for i := 1; i <= loops; i++ {
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
	doneprime <- true
}

// Validate Results comes from the solution 1
func (s Sieve) ValidateResults() bool {
	_, ok := primeCounts[s.size]
	if !ok {
		return false
	} else {
		return true
	}
}

func main() {
	var (
		limit      uint64
		duration   time.Duration
		sieve      Sieve
		help       bool
		goRoutines int
	)
	pflag.Uint64VarP(&limit, "limit", "L", 1000000, "limit")
	pflag.DurationVarP(&duration, "time", "t", 5*time.Second, "duration")
	pflag.IntVarP(&goRoutines, "routines", "r", 100, "Go routintes to launch")
	pflag.IntVarP(&loops, "loops", "l", 10, "Number of loops inside each go routine")
	pflag.BoolVarP(&help, "help", "h", false, "Help")
	pflag.Parse()
	if help {
		fmt.Println("Please see README.md for more information. You can set the number of go routines to launch. Even if you launch 1 (go run main.go -g 1) the program runs faster. Default is 1000.")
		pflag.PrintDefaults()
		os.Exit(1)
	}
	stop := make(chan struct{})
	doneprime := make(chan bool)
	passes := 0
	start := time.Now()
	time.AfterFunc(duration, func() { stop <- struct{}{} })
	sieve = Sieve{make([]uint8, (limit+15)/16), limit}
	for i := 0; i <= goRoutines; i++ {
		go sieve.RunSieve(doneprime)
	}
	for {
		select {
		case <-stop:
			ts := time.Since(start).Seconds()
			fmt.Printf("rmasci;%d;%.2f;1;algorithm=base,faithful=%v\n", passes, ts, sieve.ValidateResults())
			os.Exit(0)
		default:
			isDone := <-doneprime
			if isDone {
				// if isDone, then the routine ran the Sieve 'loops' (default is 10) number of times.
				passes = passes + loops
				sieve = Sieve{make([]uint8, (limit+15)/16), limit}
				go sieve.RunSieve(doneprime)
			}
		}
	}
}
