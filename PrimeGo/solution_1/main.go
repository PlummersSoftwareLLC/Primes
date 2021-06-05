package main

import (
	"flag"
	"fmt"
	"math"
	"time"
)

var myDict = map[int]int{
	10:  4,
	1e2: 25,
	1e3: 168,
	1e4: 1229,
	1e5: 9592,
	1e6: 78498,
	1e7: 664579,
	1e8: 5761455,
}

type Sieve struct {
	sieveSize int
	bitArray  []uint16
}

func (s *Sieve) runSieve() {
	q := int(math.Sqrt(float64(s.sieveSize)))

	for factor := 3; factor <= q; factor += 2 {
		for num := factor; num < s.sieveSize; {
			if s.GetBit(num) {
				factor = num
				break
			}
			num += 2
		}
		for num := factor * factor; num < s.sieveSize; num += factor * 2 {
			s.SetBit(num)
		}
	}
}

const (
	shift = 5
	mask  = 0b1111
)

func (s Sieve) GetBit(i int) bool { return s.bitArray[i>>shift]&(1<<((i>>1)&mask)) == 0 }
func (s Sieve) SetBit(i int)      { s.bitArray[i>>shift] |= (1 << ((i >> 1) & mask)) }

func (s Sieve) Validate() bool {
	count, ok := myDict[s.sieveSize]
	if !ok {
		return false
	}
	return count == s.countPrimes()
}

func (s Sieve) countPrimes() int {
	count := 1
	for i := 3; i < s.sieveSize; i += 2 {
		if s.GetBit(i) {
			count++
		}
	}
	return count
}

func (s Sieve) Results(showResults bool, duration time.Duration, passes int) {
	if showResults {
		fmt.Printf("2, ")
	}

	count := 1
	for num := 3; num <= s.sieveSize; num += 2 {
		if s.GetBit(num) {
			if showResults {
				fmt.Printf("%d, ", num)
			}
			count++
		}
	}
	if showResults {
		fmt.Println()
	}

	fmt.Printf("Passes: %d, Time: %v, Avg: %v, Limit: %d, Count1: %d, Count2: %d, Valid: %v\n",
		passes,
		duration,
		time.Duration(int64(duration)/int64(passes)),
		s.sieveSize,
		count,           /* count */
		s.countPrimes(), /* countPrimes() */
		s.Validate(),    /* validateResults*/
	)

	// Following 2 lines added by rbergen to conform to drag race output format
	fmt.Println()
	fmt.Printf("bundgaard;%d;%f;1;algorithm=base,faithful=yes\n", passes, duration.Seconds())
}

func main() {
	var sz int
	flag.IntVar(&sz, "size", 1e6, "enter the size as a multiple of 10")
	flag.Parse()
	run(sz)
}

func run(sz int) {
	passes := 0
	startClock := time.Now()

	for {
		sieve := Sieve{
			bitArray:  make([]uint16, sz/32),
			sieveSize: sz,
		}
		sieve.runSieve()
		passes++
		if t := time.Since(startClock); t.Seconds() >= 5 {
			sieve.Results(false, t, passes)
			break
		}
	}
}
