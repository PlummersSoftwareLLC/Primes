//go:build goexperiment.simd && amd64

package main

import (
	"bytes"
	"testing"
	"unsafe"
)

// TestVectorLoop checks every factor with starts and limits around the vector edges
// against bit by bit marking.
func TestVectorLoop(t *testing.T) {
	if !vectorSupported {
		t.Skip("no AVX2")
	}
	for p := 3; p <= 255; p += 2 {
		masks := make([]uint64, 4*p)
		buildMasks(masks, p)
		for _, limit := range []int{0, 1, 255, 256, 257, 1_000, 256 * p, 256*p + 700, 3*256*p + 13} {
			words := make([]uint64, (limit+255)>>8<<2+4)
			actual := unsafe.Slice((*byte)(unsafe.Pointer(&words[0])), len(words)*8)
			naive := make([]byte, len(actual))
			for start := p / 2; start < p/2+3*p && start < 2_000; start += p {
				clear(words)
				clear(naive)
				next := markVector(actual, words, p, start, limit, masks)
				if next < limit || next < start || (next-start)%p != 0 || (next >= limit+p && next != start) {
					t.Fatalf("vector p=%d start=%d limit=%d: returned %d", p, start, limit, next)
				}
				naiveMark(naive, start, limit, p)
				if !bytes.Equal(actual, naive) {
					t.Fatalf("vector p=%d start=%d limit=%d: marked bits differ", p, start, limit)
				}
			}
		}
	}
}
