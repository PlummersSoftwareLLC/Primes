package main

import (
	"fmt"
	"testing"
)

func testPrimes(limit uint64, newSieve SieveFunc, newStorage StorageFunc) func(t *testing.T) {
	return func(t *testing.T) {
		if limit >= 100_000_000 {
			t.SkipNow()
		}
		sieve := newSieve(limit, newStorage)
		sieve.RunSieve()
		count := sieve.CountPrimes()
		if count != primeCounts[limit] {
			t.Errorf("count = %d, want %d", count, primeCounts[limit])
		}
	}
}

func TestPrimesBaseSieve(t *testing.T) {
	for limit := range primeCounts {
		for name, newStorage := range storageTypes {
			t.Run(
				fmt.Sprintf("%v, %d", name, limit),
				testPrimes(limit, NewBaseSieve, newStorage),
			)
		}
	}
}

func TestPrimesSegmentedSieve(t *testing.T) {
	for limit := range primeCounts {
		for name, newStorage := range storageTypes {
			newSieve := NewSegmentedSieveFactory(FactoryProps{128_000})
			t.Run(
				fmt.Sprintf("%v, %d, 128_000", name, limit),
				testPrimes(limit, newSieve, newStorage),
			)
			newSieve = NewSegmentedSieveFactory(FactoryProps{32_000})
			t.Run(
				fmt.Sprintf("%v, %d, 32_000", name, limit),
				testPrimes(limit, newSieve, newStorage),
			)
		}
	}
}

func TestPrimesBlockStorage(t *testing.T) {
	for limit := range primeCounts {
		for name, s := range storageTypes {
			newStorage := NewBlockStorageFactory(s, FactoryProps{128_000})
			t.Run(
				fmt.Sprintf("%v, %d, 128_000", name, limit),
				testPrimes(limit, NewBaseSieve, newStorage),
			)
			newStorage = NewBlockStorageFactory(s, FactoryProps{32_000})
			t.Run(
				fmt.Sprintf("%v, %d, 32_000", name, limit),
				testPrimes(limit, NewBaseSieve, newStorage),
			)
		}
	}
}

func TestPrimesReverseBlockStorage(t *testing.T) {
	for limit := range primeCounts {
		for name, s := range storageTypes {
			newStorage := NewReverseBlockStorageFactory(s, FactoryProps{128_000})
			t.Run(
				fmt.Sprintf("%v, %d, 128_000", name, limit),
				testPrimes(limit, NewBaseSieve, newStorage),
			)
			newStorage = NewReverseBlockStorageFactory(s, FactoryProps{32_000})
			t.Run(
				fmt.Sprintf("%v, %d, 32_000", name, limit),
				testPrimes(limit, NewBaseSieve, newStorage),
			)
		}
	}
}
