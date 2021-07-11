package main

import (
	"fmt"
	"math"
	"math/bits"
	"os"
	"time"

	"github.com/spf13/pflag"
)

/*
I took parts of solution 1 and solution 2 to get this program. Without much work I am able to spawn 5000 (use -g to set
the number of Go Routines  you want to launch. Experiment, too many and it will slow down. I found on different systems
there was a sweet spot. On my macbook pro, 5000-10000 worked best, on my mac mini -- 1000 worked best.
The reason I am highlighting this is on how easy it is to write concurrent in Go.

I essentially just added an extra for loop, a channel to know when the go routine was done. I then modified the old for
loop to wait for a channel to finish, (returns a bool) and then launch a go routine in it's place.

*/

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

func (s Sieve) RunSieve(doneprime chan<- bool) {
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
	pflag.Uint64VarP(&limit, "limit", "l", 1000000, "limit")
	pflag.DurationVarP(&duration, "time", "t", 5*time.Second, "duration")
	pflag.IntVarP(&goRoutines, "go", "g", 1000, "Go routintes to launch")
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
				passes++
				sieve = Sieve{make([]uint8, (limit+15)/16), limit}
				go sieve.RunSieve(doneprime)
			}
		}
	}
}
