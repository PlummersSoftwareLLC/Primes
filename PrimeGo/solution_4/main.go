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

// Sieve represents the sieve of Eratosthenes for all odd numbers bigger than 3.
// Sieve[i] is false iff (2*i+3) is an odd prime.
// It is not nice to use a global variable for that,
// but this speeds up the program.
var Sieve []bool

// NewSieve inits the global Sieve variable.
func NewSieve(size int) {
	Sieve = make([]bool, size, size)
}

// wg waits for all PrimeChercker() to finish
// before to move to the next segment [n+1, n^2].
var wg sync.WaitGroup

// Pr converts a sieve index to the corresponing prime number.
// This is also the size of the shift used by PrimeChacker().
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
// that are filled consecutively by DoSieveMulti().
func Sq(i int) int {
	return (i+3)*i*2 + 3
}

// PrimeChacker fill Sieve[minT:maxT]
// using all primes found in Sieve[0:maxP].
// So Sieve[0:maxP] should be ready and maxP <= minT < maxT.
func PrimeChacker(maxP, minT, maxT int) {
	var p, step, from, i int

	for p = 0; p < maxP; p++ {
		// if not a prime, check the next one
		if Sieve[p] {
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
			Sieve[i] = true
		}
	}
	// the slice Sieve[minT:maxT] is ready
	wg.Done()
}

// PrimeChackerMulti split the slice Sieve Sieve[from:to]
// to numProc (almost) equal parts and run numProc PrimeChacker workers
// to do the checking on this non overlaping slices.
// All workers read from the shared Sieve[0:from], but writes to
// disjoint memory slices, so there is no cocnurency problem.
func PrimeChackerMulti(numProc, from, to int) {
	length := (to - from) / numProc
	// if the length is too small, non concurrency cheking is used
	// (replacing 1 by 1024 do not change the results for me)
	if length < 1 {
		wg.Add(1)
		PrimeChacker(from, from, to)
		return
	}

	// run the first numProc-1 workers on Sieve[s:t]
	var s, t int
	for s, t = from, from+length; t < to; s, t = t, t+length {
		wg.Add(1)
		go PrimeChacker(from, s, t)
	}
	// run the last worker on Sieve[s:to]
	wg.Add(1)
	go PrimeChacker(from, s, to)

	// wait for all workers to finish
	wg.Wait()
	// now Sieve[0:to] is ready
}

// DoSieveMulti create and fill Sieve corresponding
// to all natural numbers up to maxNum.
func DoSieveMulti(maxNum, numProc int) {
	// calculate the length of the Sieve slice
	N := In(maxNum)
	// allocate the global Sieve variable
	// which is init with false values by default
	NewSieve(N)

	// evaluate the optimal first slice [9:q],
	// that is included in [9:121] and
	// such that q**(2k) is slightly bigger than maxNum
	q := float64(maxNum)
	for q > 121 {
		q = math.Sqrt(q)
	}
	// as the first three odd numbers 3,5,7 are primes
	// the Sieve[0:3] is ok and we can start by filling
	// a subslice of Sieve[3:59] that corresponds to
	// a sub interval of [9,11^2).
	var from, to int = 3, In(int(q)) + 1
	for ; to < N; from, to = to, Sq(to) {
		PrimeChackerMulti(numProc, from, to)
	}
	PrimeChackerMulti(numProc, from, N)
}

// CountPrimes return the number of primes + 1 encoded in Sieve.
func CountPrimes() int {
	count := 1 // 2 is the only non even prime
	for i := 0; i < len(Sieve); i++ {
		if Sieve[i] {
			continue
		}
		count++
	}
	return count
}

// PrintResults prints some detailed info.
func PrintResults(duration time.Duration, passes int) {
	count := numPrimes[N]

	fmt.Printf("Passes: %d, Time: %d ms, Avg: %d ns/op, Limit: %d, Count1: %d, Count2: %d, Valid: %v\n",
		passes,
		duration.Milliseconds(),
		duration.Nanoseconds()/int64(passes),
		N,
		count,                  /* should be */
		CountPrimes(),          /* is */
		count == CountPrimes(), /* same ? */
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
	flag.Parse()
}

// The main function
func main() {
	// parse the command line parameter
	fmt.Printf("Run for %.1f seconds using %d workers to builing a sieve up to %d...\n\n", duration.Seconds(), routines, N)

	// start the calculation
	passes := 0
	startClock := time.Now()
	for {
		DoSieveMulti(N, routines)
		passes++
		if timeSince := time.Since(startClock); timeSince.Seconds() >= 5 {
			PrintResults(timeSince, passes)
			fmt.Printf("\nkpym-go-multi;%d;%f;4;algorithm=base,faithful=yes\n", passes, timeSince.Seconds())
			break
		}
	}
}
