package main

import (
	"flag"
	"fmt"
	"math"
	"runtime"
	"sync"
	"time"
)

// Known results
var numPrimes = map[int]int{
	10:  4,
	1e2: 25,
	1e3: 168,
	1e4: 1229,
	1e5: 9592,
	1e6: 78498,
	1e7: 664579,
	1e8: 5761455,
}

// Sieve variable represents the sieve of Eratosthenes
// for all odd numbers bigger than 3.
// For variable sieve of type Sieve the value sieve[i]
// is false iff (2*i+3) is an odd prime.
type Sieve []bool

// NewSieve creates a new Sieve variable.
func NewSieve(size int) Sieve {
	// calculate the length of the Sieve slice
	N := In(size)
	// return all false sieve
	return Sieve(make([]bool, N, N))
}

// wg waits for all PrimeChercker() to finish
// before to move to the next segment [n+1, n^2].
var wg sync.WaitGroup

// Pr converts a sieve index to the corresponing prime number.
// This is also the size of the shift used by PrimeChecker().
func Pr(i int) int {
	return i*2 + 3
}

// In converts an odd prime number to sieve index.
// This is the inverse of Pr().
func In(p int) int {
	return (p - 3) / 2
}

// Sq calculate the index of the square of Pr(i).
// Sq = In(Pr(i)*Pr(i))
// It is used to establish the slices like [n+1, n^2]
// that are filled consecutively by Build().
func Sq(i int) int {
	return (i+3)*i*2 + 3
}

// PrimeChecker fill sieve[minT:maxT]
// using all primes found in sieve[0:maxP].
// So sieve[0:maxP] should be ready and maxP <= minT < maxT.
func (sieve Sieve) PrimeChecker(maxP, minT, maxT int) {
	var p, step, from, i int

	for p = 0; p < maxP; p++ {
		// if not a prime, check the next one
		if sieve[p] {
			continue
		}
		// prepare the loop variables
		step = Pr(p)
		from = Sq(p)
		if from < minT {
			from += ((minT - from + step - 1) / step) * step
		}
		// do the check
		for i = from; i < maxT; i += step {
			sieve[i] = true
		}
	}
}

// PrimeCheckerMulti split the slice sieve[from:to]
// to numProc (almost) equal parts and run numProc PrimeChecker workers
// to do the checking on this non overlaping slices.
// All workers read from the shared sieve[0:from], but writes to
// disjoint memory slices, so there is no cocnurency problem.
func (sieve Sieve) PrimeCheckerMulti(numProc, from, to int) {
	length := (to - from) / numProc
	// if the length is too small, non concurrency cheking is used
	// (replacing 1 by 1024 do not change the results for me)
	if length < 1 {
		sieve.PrimeChecker(from, from, to)
		return
	}

	// run the first numProc-1 workers on sieve[s:t]
	var s, t int
	for s, t = from, from+length; t < to; s, t = t, t+length {
		wg.Add(1)
		go func(s, t int) {
			sieve.PrimeChecker(from, s, t)
			wg.Done()
		}(s, t)
	}
	// run the last worker on sieve[s:to]
	wg.Add(1)
	go func(s, t int) {
		sieve.PrimeChecker(from, s, t)
		wg.Done()
	}(s, to)

	// wait for all workers to finish
	wg.Wait()
	// now sieve[0:to] is ready
}

// Build fill sieve with true for all non primes.
// We make no assumptions about the first odd primes.
// We do not suppose that 3,5,7 or 11 are primes !
// As the first odd prime is at least 3, so the base
// algorithme starts at least at 3**2 = 9.
// Using this we know that the sieve is ready upto 9 (excluded).
// As 9 is a square, it is not prime, the smallest prime
// above 9 is at least 11.
// So we can start filling the sieve using the odd pries inside
// [3,7] to build the sieve up to 11**2=121 (excluded).

func (sieve Sieve) Build(numProc int) {
	// evaluate the optimal first slice [9:q],
	// that is included in [9:121] and
	// such that q**(2k) is slightly bigger than sqrt(N).
	q := math.Sqrt(float64(Pr(len(sieve) - 1)))
	// to build the sieve we need to know all primes in [0:Pr(upto)]
	upto := In(int(q)) + 1
	for q > 121 {
		q = math.Sqrt(q)
	}
	// the sieve[0:3] corrsponing to [3,9) is ok
	// and we can start by filling a subslice of
	// sieve[3:59] corresponding to sub interval of [9,11^2).
	var from, to int = 3, In(int(q)) + 1
	for ; to < upto; from, to = to, Sq(to) {
		sieve.PrimeCheckerMulti(numProc, from, to)
	}
	sieve.PrimeCheckerMulti(numProc, from, upto)
	sieve.PrimeCheckerMulti(numProc, upto, len(sieve))
}

// CountPrimes return the number of primes + 1 encoded in sieve.
func (sieve Sieve) CountPrimes() int {
	count := 1 // 2 is the only non even prime
	for i := 0; i < len(sieve); i++ {
		if sieve[i] {
			continue
		}
		count++
	}
	return count
}

// PrintResults prints some detailed info.
func (sieve Sieve) PrintResults(duration time.Duration, passes int) {
	count := numPrimes[N]

	fmt.Printf("Passes: %d, Time: %d ms, Avg: %d ns/op, Limit: %d, Count1: %d, Count2: %d, Valid: %v\n",
		passes,
		duration.Milliseconds(),
		duration.Nanoseconds()/int64(passes),
		N,
		count,                        /* should be */
		sieve.CountPrimes(),          /* is */
		count == sieve.CountPrimes(), /* same ? */
	)
}

// Program parmaters
var (
	N        int           // the upper limit (by default 1000000)
	duration time.Duration // the sampling duration (by default 5s)
	routines int           // the number of go routines used (by default 3 × the number of CPU)
)

// Set the parameters using the command flags
func init() {
	flag.IntVar(&N, "limit", 1e6, "calculate primes up to limit")
	flag.DurationVar(&duration, "time", 5*time.Second, "sampling duration")
	flag.IntVar(&routines, "routines", 3*runtime.NumCPU(), "number of workers to use")
}

// The main function
func main() {
	// parse the command line parameter
	flag.Parse()
	fmt.Printf("Run for %.1f seconds using %d workers to builing a sieve up to %d...\n\n", duration.Seconds(), routines, N)

	// start the calculation
	passes := 0
	startClock := time.Now()
	for {
		sieve := NewSieve(N)
		sieve.Build(routines)
		passes++
		if timeSince := time.Since(startClock); timeSince.Seconds() >= 5 {
			sieve.PrintResults(timeSince, passes)
			fmt.Printf("\nkpym-go-multi;%d;%f;4;algorithm=base,faithful=yes\n", passes, timeSince.Seconds())
			break
		}
	}
}
