package main

var sieves map[string]SieveFactoryFunc = map[string]SieveFactoryFunc{
	"base": NewBaseSieveFactory,
	"seg":  NewSegmentedSieveFactory,
}

type Sieve interface {
	RunSieve()
	CountPrimes() uint64
	Size() uint64
}
type SieveFunc func(uint64, StorageFunc) Sieve
type SieveFactoryFunc func(FactoryProps) SieveFunc

type BaseSieve struct {
	bits Storage
	size uint64
}

func NewBaseSieve(size uint64, newStorage StorageFunc) Sieve {
	return BaseSieve{newStorage((size + 1) / 2), size}
}

func NewBaseSieveFactory(_ FactoryProps) SieveFunc {
	return NewBaseSieve
}

func (s BaseSieve) RunSieve() {
	if s.size < 2 {
		return
	}

	var factor, start, stop, step uint64
	stop = (s.size + 1) / 2
	for {
		factor = s.bits.Find(false, factor+1, stop)

		start = 2 * factor * (factor + 1)
		step = factor*2 + 1

		// start is factor squared, so it's the same as factor <= q
		if start >= stop {
			break
		}

		s.bits.SetRange(start, stop, step)
	}
}

func (s BaseSieve) CountPrimes() uint64 {
	if s.size < 2 {
		return 0
	}
	return s.bits.Count(false, 0, (s.size+1)/2)
}

func (s BaseSieve) Size() uint64 {
	return s.size
}

type SegmentedSieve struct {
	BaseSieve
	blockSize uint64
	end       uint64
}

func NewSegmentedSieveFactory(props FactoryProps) SieveFunc {
	return func(size uint64, newStorage StorageFunc) Sieve {
		length := ((size + 1) / 2)
		return SegmentedSieve{
			BaseSieve{newStorage(length), size},
			props.blockSize,
			length,
		}
	}
}

func (s SegmentedSieve) clrMults(factor, blockStart, blockEnd uint64) bool {
	start := 2 * factor * (factor + 1)
	if start >= blockEnd {
		return true
	}

	step := factor*2 + 1

	if start < blockStart {
		start += ((blockStart - start + step - 1) / step) * step
	}

	s.bits.SetRange(start, blockEnd, step)
	return false
}

func (s SegmentedSieve) RunSieve() {
	if s.size < 2 {
		return
	}

	var factor, blockStart, blockEnd uint64
	factors := make([]uint64, 0, 128)

loop:
	for blockStart = 0; blockStart < s.end; blockStart += s.blockSize {

		blockEnd = blockStart + s.blockSize
		if blockEnd > s.end {
			blockEnd = s.end
		}

		for _, factor = range factors {
			if s.clrMults(factor, blockStart, blockEnd) {
				continue loop
			}
		}

		for {
			factor = s.bits.Find(false, factor+1, blockEnd)
			factors = append(factors, factor)
			if s.clrMults(factor, blockStart, blockEnd) {
				continue loop
			}
		}

	}
}
