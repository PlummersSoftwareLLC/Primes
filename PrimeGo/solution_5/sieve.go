package main

//go:generate go run ./gen markers_gen.go

import (
	"math"
	"math/bits"
	"unsafe"
)

// primeSieve is a sieve of Eratosthenes over the odd numbers. Bit i is the number 2i+1
// and a set bit means composite, so a new buffer starts with everything prime.
type primeSieve struct {
	size      int
	halfLimit int // how many odd numbers: 1, 3, 5, ... up to size
	vectorMax int // largest factor on the vector path, 0 where the build has none
	words     []uint64
	bytes     []byte // the same memory as words
}

const (
	defaultVectorMax = 127

	// Room on the stack for the patterns of the primes from 3 to maskRoomMax. They add
	// up to 1718, and a pattern takes four words per unit of factor.
	maskRoomMax = 127
	maskRoom    = 4 * 1718
)

func init() {
	// The same buffer is read as bytes and as 64-bit words.
	one := uint16(1)
	if *(*byte)(unsafe.Pointer(&one)) != 1 {
		panic("the sieve assumes a little-endian machine")
	}
}

// newSieve allocates the buffer for the numbers up to size. Factors up to vectorMax
// are marked with vectors.
func newSieve(size, vectorMax int) primeSieve {
	if size < 0 {
		panic("negative sieve size")
	}
	if !vectorSupported {
		vectorMax = 0
	}
	half := (size + 1) / 2
	// Rounded up to whole 256-bit vectors.
	words := make([]uint64, (half+255)>>8<<2)
	return primeSieve{
		size:      size,
		halfLimit: half,
		vectorMax: vectorMax,
		words:     words,
		bytes:     unsafe.Slice((*byte)(unsafe.Pointer(unsafe.SliceData(words))), len(words)*8),
	}
}

// run sieves the whole buffer one factor at a time, without vectors.
func (s *primeSieve) run() {
	bytes, words, limit := s.bytes, s.words, s.halfLimit
	if limit < 2 {
		return // no odd number above 1
	}
	for h := 1; ; h++ {
		// Next factor: the first bit that is still clear.
		for bytes[h>>3]&(1<<(h&7)) != 0 {
			h++
		}
		factor := 2*h + 1
		start := 2 * h * (h + 1) // index of factor * factor
		if start >= limit {
			break
		}
		mark(bytes, words, factor, start, limit)
	}
}

// runSegmented is the same sieve, one segment at a time so the marking stays in L1.
// In each segment the factors found so far carry on from where they stopped, then the
// search for new ones goes on. By then every smaller factor has marked the segment.
func (s *primeSieve) runSegmented(segmentBytes int) {
	if segmentBytes < 1 {
		panic("segment size must be positive")
	}
	bytes, words, limit := s.bytes, s.words, s.halfLimit
	if limit < 2 {
		return
	}

	// Room for every prime up to sqrt(size): pi(x) < 1.26 x / ln x.
	root := math.Sqrt(float64(s.size))
	maxFactors := int(1.26*root/math.Log(root+2)) + 8
	var factorBuf, nextBuf [256]int
	factors, next := factorBuf[:], nextBuf[:]
	if maxFactors > len(factorBuf) {
		factors, next = make([]int, maxFactors), make([]int, maxFactors)
	}
	// The vector patterns are built once per pass, 4 * factor words each.
	cutoff := s.vectorMax
	var maskBuf [maskRoom + 7]uint64
	var maskAt [64]int
	// Start on a cache line, so no vector of a pattern is split over two.
	masks := maskBuf[(-uintptr(unsafe.Pointer(&maskBuf))>>3)&7:]
	if half := (cutoff + 1) / 2; cutoff > maskRoomMax {
		masks = make([]uint64, 4*(half*half-1)+7)
	}
	vectorCount, maskUsed := 0, 0

	found := 0
	h := 1 // next candidate to check
	searching := true
	segmentBits := segmentBytes * 8

	for segmentStart := 0; segmentStart < limit; segmentStart += segmentBits {
		end := min(segmentStart+segmentBits, limit)

		for f := 0; f < vectorCount; f++ {
			next[f] = markVector(bytes, words, factors[f], next[f], end, masks[maskAt[f]:])
		}
		for f := vectorCount; f < found; f++ {
			next[f] = mark(bytes, words, factors[f], next[f], end)
		}

		for searching {
			for h < end && bytes[h>>3]&(1<<(h&7)) != 0 {
				h++
			}
			if h >= end {
				break // the search continues in the next segment
			}
			factor := 2*h + 1
			start := 2 * h * (h + 1)
			if start >= limit {
				searching = false
				break
			}
			factors[found] = factor
			if factor <= cutoff {
				maskAt[vectorCount] = maskUsed
				buildMasks(masks[maskUsed:], factor)
				maskUsed += 4 * factor
				next[found] = markVector(bytes, words, factor, start, end, masks[maskAt[vectorCount]:])
				vectorCount++
			} else {
				next[found] = mark(bytes, words, factor, start, end)
			}
			found++
			h++
		}
	}
}

// buildMasks writes the pattern of factor into 4 * factor zeroed words: 64 marks, then
// two copies.
func buildMasks(masks []uint64, factor int) {
	for j := factor >> 1; j < 64*factor; j += factor {
		masks[j>>6] |= 1 << (j & 63)
	}
	copy(masks[factor:], masks[:factor])
	copy(masks[2*factor:], masks[:2*factor])
}

// mark sets the multiples of factor from idx up to limit and returns the index of the
// next multiple.
func mark(bytes []byte, words []uint64, factor, idx, limit int) int {
	if factor <= denseMax {
		return markDense(bytes, words, factor, idx, limit)
	}
	idx = sparse(factor, bytes, idx, limit)
	return markEach(bytes, factor, idx, limit)
}

// markDense is the word loop for small factors: whole blocks of `factor` words with
// 64 multiples each, and the rest bit by bit.
func markDense(bytes []byte, words []uint64, factor, idx, limit int) int {
	blockBits := factor << 6
	k0 := (idx + blockBits - 1) / blockBits
	k1 := limit / blockBits
	if k0 < k1 {
		markEach(bytes, factor, idx, k0*blockBits)
		denseBlocks(factor, words, k0, k1)
		idx = k1*blockBits + factor>>1 // first multiple of the next block
	}
	return markEach(bytes, factor, idx, limit)
}

// markEach marks one multiple at a time.
func markEach(bytes []byte, factor, idx, limit int) int {
	for ; idx < limit; idx += factor {
		bytes[idx>>3] |= 1 << (idx & 7)
	}
	return idx
}

// isPrime is only valid after the sieve has run.
func (s *primeSieve) isPrime(n int) bool {
	if n < 2 || n > s.size {
		return false
	}
	if n&1 == 0 {
		return n == 2
	}
	i := n >> 1
	return s.bytes[i>>3]&(1<<(i&7)) == 0
}

// countPrimes counts the primes up to size.
func (s *primeSieve) countPrimes() int {
	if s.size < 2 {
		return 0
	}
	composites := 0
	full := s.halfLimit >> 6
	for _, word := range s.words[:full] {
		composites += bits.OnesCount64(word)
	}
	if rest := s.halfLimit & 63; rest != 0 {
		composites += bits.OnesCount64(s.words[full] & (1<<rest - 1))
	}
	// 1 is counted but is not prime, 2 is prime but not counted. They cancel out.
	return s.halfLimit - composites
}
