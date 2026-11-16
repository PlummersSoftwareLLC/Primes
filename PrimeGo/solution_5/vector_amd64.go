//go:build goexperiment.simd && amd64

package main

import (
	"simd/archsimd"
	"unsafe"
)

// vectorSupported says whether this CPU can run the vector loop.
var vectorSupported = archsimd.X86.AVX2()

// markVector marks the multiples of a small factor 256 bits at a time. masks holds the
// pattern of the factor, which repeats after `factor` vectors.
func markVector(bytes []byte, words []uint64, factor, idx, limit int, masks []uint64) int {
	v0 := (idx + 255) >> 8 // first whole vector at or after idx
	v1 := limit >> 8       // end of the whole vectors below limit
	if v0 >= v1 {
		return markEach(bytes, factor, idx, limit)
	}
	idx = markEach(bytes, factor, idx, v0<<8)

	// One range check for the whole loop.
	_ = words[4*v1-1]
	_ = masks[4*factor-1]
	target := unsafe.Pointer(unsafe.SliceData(words))
	pattern := unsafe.Pointer(unsafe.SliceData(masks))

	orPattern(unsafe.Add(target, v0*32), pattern, v0%factor*32, factor*32, (v1-v0)*32)

	// Skip to the first multiple after the vectors and finish bit by bit.
	gap := v1<<8 - idx
	idx += (gap + factor - 1) / factor * factor
	return markEach(bytes, factor, idx, limit)
}

// orPattern ORs the pattern into size bytes at block. It starts `at` bytes into the
// pattern and starts over at its end. Inlined into markVector this loop was 6% slower,
// so it stays a function of its own.
//
//go:noinline
func orPattern(block, pattern unsafe.Pointer, at, period, size int) {
	for ; size > 0; at = 0 {
		n := min(period-at, size)
		mask := unsafe.Add(pattern, at)
		i := 0
		for ; i+64 <= n; i += 64 {
			orVector(unsafe.Add(block, i), unsafe.Add(mask, i))
			orVector(unsafe.Add(block, i+32), unsafe.Add(mask, i+32))
		}
		if i < n {
			orVector(unsafe.Add(block, i), unsafe.Add(mask, i))
		}
		block = unsafe.Add(block, n)
		size -= n
	}
}

// orVector ORs one 256-bit vector of the pattern into the sieve.
func orVector(block, mask unsafe.Pointer) {
	b := (*[4]uint64)(block)
	archsimd.LoadUint64x4Array(b).Or(archsimd.LoadUint64x4Array((*[4]uint64)(mask))).StoreArray(b)
}
