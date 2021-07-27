package main

import (
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

func (s *Sieve) runSieve() {
	factor := 3
	var q = int(math.Sqrt(float64(s.sieveSize)))

	for factor <= q {
		for num := factor; num < s.sieveSize; num += 2 {
			if s.bitArray[num] {
				factor = num
				break
			}
		}
		for num := factor * factor; num < s.sieveSize; num += factor * 2 {
			s.bitArray[num] = false
		}
		factor += 2
	}
}

type Sieve struct {
	sieveSize int
	bitArray  []bool
}

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
		if s.bitArray[i] {
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
		if s.bitArray[num] {
			if showResults {
				fmt.Printf("%d, ", num)
			}
			count++
		}
	}

	if showResults {
		fmt.Println()
	}

	fmt.Printf("Passes: %d, Time: %d, Avg: %f, Limit: %d, Count1: %d, Count2: %d, Valid: %v\n",
		passes,
		duration,
		float64(duration.Milliseconds()/int64(passes)),
		s.sieveSize,
		count,           /* count */
		s.countPrimes(), /* countPrimes() */
		s.Validate() /* validateResults*/)

	// Following 2 lines added by rbergen to conform to drag race output format
	fmt.Println()
	fmt.Printf("bundgaard;%d;%f;1;algorithm=base,faithful=yes\n", passes, duration.Seconds())
}

func main() {

	passes := 0
	startClock := time.Now()

	for {
		initBitArray := make([]bool, 1e6)
		for i := range initBitArray {
			initBitArray[i] = true
		}
		sieve := Sieve{bitArray: initBitArray, sieveSize: 1e6}
		sieve.runSieve()
		passes++
		if time.Since(startClock).Seconds() >= 5 {
			sieve.Results(false, time.Since(startClock), passes)
			break
		}
	}
}
