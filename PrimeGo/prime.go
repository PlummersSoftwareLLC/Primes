package main

import (
	"fmt"
	"math"
	"time"
)

type primeBool struct {
	size    int
	sqrt    int
	isPrime []bool
}

var myDict = map[int]int{
	10:        4,
	100:       25,
	1000:      168,
	10000:     1229,
	100000:    9592,
	1000000:   78498,
	10000000:  664579,
	100000000: 5761455,
}

func main() {
	passes := 0
	start := time.Now()
	p := primeBool{}
	for time.Since(start).Seconds() < 10 {
		p.Set(1000000)
		p.Run()
		passes++
	}
	tD := time.Since(start)
	p.PrintResults(false, tD, passes)
}

func (p *primeBool) Set(index int) {
	p.size = index
	p.isPrime = make([]bool, index+1)
	p.sqrt = int(math.Sqrt(float64(index)))
	p.isPrime[2] = true
	for i := 3; i <= index; i = i + 2 {
		if i%2 != 0 {
			p.isPrime[i] = true
		}
	}

}

func (p *primeBool) Run() {
	factor := 3
	for factor <= p.sqrt {
		for num := factor; num <= p.size; num++ {
			if p.isPrime[num] {
				factor = num
				break
			}
		}
		for num := factor * 3; num <= p.size; num += factor * 2 {
			p.isPrime[num] = false
		}
		factor++
	}
}

func (p *primeBool) PrintResults(showResults bool, duration time.Duration, passes int) {
	primes := make([]int, 0)
	for num := 2; num <= p.size; num++ {
		if p.isPrime[num] {
			primes = append(primes, num)
		}
	}
	if showResults {
		fmt.Println(primes)
	}
	var avg time.Duration = duration / time.Duration(passes)
	fmt.Printf("Passes: %d, Time: %v, Avg: %v, Limit: %d, Count: %d, Valid: %t\n",
		passes,
		duration,
		avg,
		p.size,
		len(primes),
		p.validateResults())
}

func (p *primeBool) validateResults() bool {
	value, ok := myDict[p.size]
	if !ok {
		fmt.Println("Warning: validation quantity not in map")
		return false
	}
	return value == p.countPrimes()
}

func (p *primeBool) countPrimes() int {
	count := 0
	for i := 0; i < p.size; i++ {
		if p.isPrime[i] {
			count++
		}
	}
	return count
}
