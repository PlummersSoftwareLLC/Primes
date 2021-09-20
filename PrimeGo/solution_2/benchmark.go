package main

import (
	"fmt"
	"regexp"
	"sort"
	"time"
)

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

// Benchmark represents all necessary info to benchmark a sieve implementation
type Benchmark struct {
	Label      string
	Tags       Tags
	NewSieve   SieveFunc
	NewStorage StorageFunc
}

// BenchmarkResult represents all necessary info to output and validate
// benchmark results
type BenchmarkResult struct {
	Label    string
	Tags     Tags
	Duration float64
	Limit    uint64
	Count    uint64
	Passes   int
	Valid    bool
}

// NewBenchmark creates a benchmark from a config entry.
// It's used in the drag-race mode.
func NewBenchmark(ce RaceConfigEntry) (*Benchmark, error) {
	var ok bool
	var newStorage StorageFunc
	var newSieveFactory SieveFactoryFunc
	var dec StorageFactoryFunc

	if ce.BlockSize == 0 {
		ce.BlockSize = DefaultBlockSize
	}

	if newStorage, ok = storageTypes[ce.StorageName]; !ok {
		return nil, fmt.Errorf("storage type %v not found", ce.StorageName)
	}

	props := NewFactoryPropsFromConfig(ce)
	for _, mod := range ce.StorageMods {
		if dec, ok = storageMods[mod]; !ok {
			return nil, fmt.Errorf("storage decorator %v not found", mod)
		}
		newStorage = dec(newStorage, props)
	}

	if newSieveFactory, ok = sieves[ce.Sieve]; !ok {
		return nil, fmt.Errorf("sieve type %v not found", ce.Sieve)
	}

	return &Benchmark{
		ce.Label,
		MakeTags(ce),
		newSieveFactory(props),
		newStorage,
	}, nil
}

// MakeBenchmark creates a slice of benchmarks from a slice of config entries
func MakeBenchmarks(conf []RaceConfigEntry) ([]*Benchmark, error) {
	result := make([]*Benchmark, 0, len(conf))

	for _, ce := range conf {
		if bench, err := NewBenchmark(ce); err == nil {
			result = append(result, bench)
		} else {
			return nil, err
		}
	}

	return result, nil
}

// Run runs benchmark for an implementation specified in the Benchmark object,
// calculating primes up to a limit for a given duration.
func (b Benchmark) Run(limit uint64, duration time.Duration) BenchmarkResult {
	var sieve Sieve
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
			sieve = b.NewSieve(limit, b.NewStorage)
			sieve.RunSieve()
			passes++
		}
	}

	close(stop)

	count := sieve.CountPrimes()
	vcount, ok := primeCounts[limit]
	return BenchmarkResult{
		b.Label,
		b.Tags,
		time.Since(start).Seconds(),
		limit,
		count,
		passes,
		ok && vcount == count,
	}

}

// AllBenchmarks creates base sieve, segmented sieve, blocks and rblocks
// benchmarks for all registered storage types.
// It skips benchmarks that don't match the regexp provided in filter argument.
// Block size is passed through props argument.
func AllBenchmarks(filter *regexp.Regexp, props FactoryProps) []*Benchmark {
	//Sorting map keys to get a deterministic order
	types := make([]string, 0)
	for typename := range storageTypes {
		types = append(types, typename)
	}
	sort.Strings(types)

	tags := make(Tags) //empty tags

	var label string
	benchmarks := make([]*Benchmark, 0)
	for _, typename := range types {
		newStorage, ok := storageTypes[typename]
		if !ok {
			//If we're here, some *really* weird stuff has happend
			panic(fmt.Sprintf("type %v not found", typename))
		}

		label = typename
		if filter.MatchString(label) {
			benchmarks = append(benchmarks, &Benchmark{label, tags, NewBaseSieve, newStorage})
		}

		label = typename + "-Blk"
		if filter.MatchString(label) {
			benchmarks = append(benchmarks, &Benchmark{label, tags, NewBaseSieve, NewBlockStorageFactory(newStorage, props)})
		}

		label = typename + "-Rlk"
		if filter.MatchString(label) {
			benchmarks = append(benchmarks, &Benchmark{label, tags, NewBaseSieve, NewReverseBlockStorageFactory(newStorage, props)})
		}

		label = typename + "-Seg"
		if filter.MatchString(label) {
			benchmarks = append(benchmarks, &Benchmark{label, tags, NewSegmentedSieveFactory(props), newStorage})
		}
	}

	return benchmarks
}
