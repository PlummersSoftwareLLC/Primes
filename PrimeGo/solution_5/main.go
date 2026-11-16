package main

import (
	"flag"
	"fmt"
	"os"
	"runtime/debug"
	"time"
)

const (
	sieveSize      = 1_000_000
	expectedCount  = 78_498
	defaultSegment = 32 * 1024
	baseLabel      = "pbednarcik-go"

	// The only garbage is one sieve per pass. Collecting it less often is faster.
	defaultGC = 400
)

func main() {
	// For experiments. The default is what the benchmark runs.
	segmentBytes := flag.Int("segment", defaultSegment, "segment size in bytes, 0 = unsegmented")
	vecmax := flag.Int("vecmax", defaultVectorMax, "largest factor on the vector path, 0 = none")
	gcPercent := flag.Int("gc", defaultGC, "GC target percent, -1 = off")
	flag.Parse()
	vectorMax := min(*vecmax, 255)
	debug.SetGCPercent(*gcPercent)

	// A non-default setting gets its own label, so results can't be mixed up.
	label := baseLabel
	if *segmentBytes == 0 {
		label += "-unsegmented"
	} else if *segmentBytes != defaultSegment {
		label += fmt.Sprintf("-seg%dk", *segmentBytes/1024)
	}

	if !vectorSupported {
		label += "-novec"
	} else if vectorMax != defaultVectorMax {
		label += fmt.Sprintf("-vec%d", vectorMax)
	}
	if *gcPercent != defaultGC {
		label += fmt.Sprintf("-gc%d", *gcPercent)
	}

	// The timed run: a new sieve every pass, for 5 seconds.
	var sieve primeSieve
	passes := 0
	started := time.Now()
	for time.Since(started) < 5*time.Second {
		sieve = newSieve(sieveSize, vectorMax)
		if *segmentBytes == 0 {
			sieve.run()
		} else {
			sieve.runSegmented(*segmentBytes)
		}
		passes++
	}
	seconds := time.Since(started).Seconds()

	if count := sieve.countPrimes(); count != expectedCount {
		fmt.Fprintf(os.Stderr, "Invalid result: counted %d primes up to %d, expected %d.\n", count, sieveSize, expectedCount)
		os.Exit(1)
	}
	fmt.Printf("%s;%d;%f;1;algorithm=base,faithful=yes,bits=1\n", label, passes, seconds)
}
