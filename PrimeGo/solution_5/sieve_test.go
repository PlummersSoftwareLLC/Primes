package main

import (
	"bytes"
	"math/rand"
	"testing"
	"unsafe"
)

const maxRandomSize = 3_000_000

var composite = referenceSieve(maxRandomSize)

var knownCounts = map[int]int{
	10: 4, 100: 25, 1_000: 168, 10_000: 1_229, 100_000: 9_592, 1_000_000: 78_498, 10_000_000: 664_579,
}

func referenceSieve(max int) []bool {
	composite := make([]bool, max+1)
	for f := 2; f*f <= max; f++ {
		if !composite[f] {
			for m := f * f; m <= max; m += f {
				composite[m] = true
			}
		}
	}
	return composite
}

func run(size, segmentBytes int) primeSieve {
	sieve := newSieve(size, defaultVectorMax)
	if segmentBytes == 0 {
		sieve.run()
	} else {
		sieve.runSegmented(segmentBytes)
	}
	return sieve
}

// compare checks every number up to size against the reference sieve.
func compare(t *testing.T, size, segmentBytes int) {
	t.Helper()
	sieve := run(size, segmentBytes)
	expectedCount := 0
	for n := 0; n <= size; n++ {
		expected := n >= 2 && !composite[n]
		if expected {
			expectedCount++
		}
		if sieve.isPrime(n) != expected {
			t.Fatalf("size %d, segment %d: isPrime(%d) is %v", size, segmentBytes, n, !expected)
		}
	}
	if count := sieve.countPrimes(); count != expectedCount {
		t.Fatalf("size %d, segment %d: counted %d, expected %d", size, segmentBytes, count, expectedCount)
	}
}

func TestKnownCounts(t *testing.T) {
	for _, segment := range []int{0, 8, 64, 1_000, 16_384, 32_768} {
		for size, expected := range knownCounts {
			sieve := run(size, segment)
			if count := sieve.countPrimes(); count != expected {
				t.Errorf("size %d, segment %d: counted %d, expected %d", size, segment, count, expected)
			}
		}
	}
}

// TestAgainstReference compares every number with a plain sieve. With tiny segments
// nearly every loop gets cut by a segment boundary, and the factor
// search crosses segments too.
func TestAgainstReference(t *testing.T) {
	random := rand.New(rand.NewSource(20261115))
	for _, segment := range []int{0, 8, 64, 1_000, 16_384, 32_768} {
		for size := 0; size <= 3_000; size++ {
			compare(t, size, segment)
		}
		for i := 0; i < 100; i++ {
			compare(t, 3_000+random.Intn(maxRandomSize-3_000+1), segment)
		}
	}
}

func BenchmarkPass(b *testing.B) {
	for i := 0; i < b.N; i++ {
		sieve := newSieve(1_000_000, defaultVectorMax)
		sieve.runSegmented(32 * 1024)
	}
}

func naiveMark(buffer []byte, start, stop, p int) {
	for i := start; i < stop; i += p {
		buffer[i>>3] |= 1 << (i & 7)
	}
}

// TestSparseLoops runs every (p & 7, start bit) pair, so a loop the sieve never reaches
// at this size is still checked.
func TestSparseLoops(t *testing.T) {
	const limit = 400_000
	for p := 3; p < 1_200; p += 2 {
		for start := 0; start < 16; start++ {
			actual := make([]byte, (limit+63)/64*8)
			naive := make([]byte, len(actual))
			next := sparse(p, actual, start, limit)
			if next < start || (next-start)%p != 0 || next+8*p+8 < limit {
				t.Fatalf("sparse p=%d start=%d: returned next index %d", p, start, next)
			}
			naiveMark(naive, start, next, p)
			if !bytes.Equal(actual, naive) {
				t.Fatalf("sparse p=%d start=%d: marked bits differ from naive marking", p, start)
			}
		}
	}
}

func TestDenseLoops(t *testing.T) {
	for p := 3; p <= denseMax; p += 2 {
		// Blocks 1 and 2 only: block k covers bits [64pk, 64p(k+1)).
		words := make([]uint64, p*4)
		actual := unsafe.Slice((*byte)(unsafe.Pointer(&words[0])), len(words)*8)
		naive := make([]byte, len(actual))
		denseBlocks(p, words, 1, 3)
		naiveMark(naive, 64*p+p/2, 3*64*p, p)
		if !bytes.Equal(actual, naive) {
			t.Fatalf("dense p=%d: marked bits differ from naive marking", p)
		}
	}
}
