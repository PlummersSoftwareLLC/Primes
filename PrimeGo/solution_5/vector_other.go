//go:build !(goexperiment.simd && amd64)

package main

// No vectors in this build: every factor uses the generated loops.
var vectorSupported = false

func markVector(bytes []byte, words []uint64, factor, idx, limit int, masks []uint64) int {
	panic("no vector path in this build")
}
