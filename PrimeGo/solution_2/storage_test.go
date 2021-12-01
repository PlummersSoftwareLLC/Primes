package main

import (
	"sort"
	"strconv"
	"testing"
)

type recStorage struct {
	srcalls []setRangeCall
	fcalls  []findCall
	ccalls  []countCall
	length  uint64
	index   uint64
	count   uint64
	primes  []uint64
}

type setRangeCall struct {
	start uint64
	stop  uint64
	step  uint64
}

type findCall struct {
	val   bool
	start uint64
	stop  uint64
}

type countCall struct {
	val   bool
	start uint64
	stop  uint64
}

func newRecStorageFactory() (StorageFunc, *recStorage) {
	var s recStorage
	return func(length uint64) Storage {
		s.reset(length)
		return &s
	}, &s
}

func (s *recStorage) reset(length uint64) {
	s.srcalls = make([]setRangeCall, 0)
	s.fcalls = make([]findCall, 0)
	s.ccalls = make([]countCall, 0)
	s.length = length
	s.index = 0
	s.count = 0
	s.primes = []uint64{1, 2, 3, 5, 6, 8, 9, 11} //primes' indices actually
}

func (s *recStorage) At(index uint64) bool {
	return false
}

func (s *recStorage) Set(index uint64, val bool) {

}

func (s *recStorage) SetRange(start, stop, step uint64) {
	s.srcalls = append(s.srcalls, setRangeCall{start, stop, step})
}

func (s *recStorage) Find(val bool, start, stop uint64) uint64 {
	s.fcalls = append(s.fcalls, findCall{val, start, stop})
	r := s.primes[s.index]
	s.index++
	return r
}

func (s *recStorage) Count(val bool, start, stop uint64) uint64 {
	s.ccalls = append(s.ccalls, countCall{val, start, stop})
	return s.count
}

func (s *recStorage) Length() uint64 {
	return s.length
}

type stubStorage struct{}

func (s stubStorage) At(index uint64) bool {
	return false
}

func (s stubStorage) Set(index uint64, val bool) {
}

func (s stubStorage) SetRange(_, _, _ uint64) {
}

func (s stubStorage) Find(_ bool, _, _ uint64) uint64 {
	return 0
}

func (s stubStorage) Count(_ bool, _, _ uint64) uint64 {
	return 0
}

func (s stubStorage) Length() uint64 {
	return 0
}

func newStubStorageFactory() StorageFunc {
	return func(uint64) Storage {
		return stubStorage{}
	}
}

func TestBlockStorage(t *testing.T) {
	var bsize uint64 = 1000
	fn, ptr := newRecStorageFactory()
	fmock := NewBlockStorageFactory(fn, FactoryProps{bsize})

	tests := []struct {
		length uint64
		args   setRangeCall
		want   []setRangeCall
	}{
		{
			100,
			setRangeCall{1, 100, 3},
			[]setRangeCall{{1, 100, 3}},
		},
		{
			1000,
			setRangeCall{0, 1000, 100},
			[]setRangeCall{{0, 1000, 100}},
		},
		{
			1000,
			setRangeCall{0, 999, 100},
			[]setRangeCall{{0, 999, 100}},
		},
		{
			1000,
			setRangeCall{0, 345, 100},
			[]setRangeCall{{0, 345, 100}},
		},
		{
			1000,
			setRangeCall{0, 1001, 100},
			[]setRangeCall{{0, 1000, 100}},
		}, {
			1000,
			setRangeCall{0, 2500, 100},
			[]setRangeCall{{0, 1000, 100}},
		},
		{
			1000,
			setRangeCall{0, 50, 100},
			[]setRangeCall{{0, 50, 100}},
		},
		{
			1000,
			setRangeCall{0, 1000, 9999},
			[]setRangeCall{{0, 1000, 9999}},
		},
		{
			1000,
			setRangeCall{60, 1000, 11},
			[]setRangeCall{{60, 1000, 11}},
		},
		{
			1000,
			setRangeCall{999, 1000, 100},
			[]setRangeCall{{999, 1000, 100}},
		},
		{
			1000,
			setRangeCall{1000, 1000, 100},
			[]setRangeCall{{1000, 1000, 100}},
		},
		{
			1000,
			setRangeCall{1001, 1000, 100},
			[]setRangeCall{{1001, 1000, 100}},
		},
		{
			2000,
			setRangeCall{0, 2000, 10},
			[]setRangeCall{{0, 1000, 10}, {1000, 2000, 10}},
		},
		{
			2000,
			setRangeCall{0, 2500, 10},
			[]setRangeCall{{0, 1000, 10}, {1000, 2000, 10}},
		},
		{
			2333,
			setRangeCall{0, 2500, 10},
			[]setRangeCall{{0, 1000, 10}, {1000, 2000, 10}, {2000, 2333, 10}},
		},
		{
			1001,
			setRangeCall{0, 2500, 10},
			[]setRangeCall{{0, 1000, 10}, {1000, 1001, 10}},
		},
		{
			2000,
			setRangeCall{5, 2000, 10},
			//start is also the first block's start, that's why 1005 is in the first call
			[]setRangeCall{{5, 1005, 10}, {1005, 2000, 10}},
		},
		{
			2100,
			setRangeCall{10, 1800, 13},
			[]setRangeCall{{10, 1010, 13}, {1011, 1800, 13}},
		},
		{
			1700,
			setRangeCall{10, 1800, 13},
			[]setRangeCall{{10, 1010, 13}, {1011, 1700, 13}},
		},
		{
			2000,
			setRangeCall{2010, 3000, 13},
			[]setRangeCall{{2010, 2000, 13}},
		},
		{
			2000,
			setRangeCall{1800, 1500, 13},
			[]setRangeCall{{1800, 1500, 13}},
		},
	}

	for _, test := range tests {
		args := test.args
		want := test.want
		length := test.length

		s := fmock(length)
		s.SetRange(args.start, args.stop, args.step)
		if len(ptr.srcalls) != len(want) {
			t.Errorf("%T(%d).Storage.SetRange called %d time(s), want %d (Args: %v, Calls: %v)",
				s, length, len(ptr.srcalls), len(want), args, ptr.srcalls)
		} else {
			for i := range want {
				have := ptr.srcalls[i]
				if have != want[i] {
					t.Errorf("%T(%d).SetRange(%d, %d, %d) calls: %v, want %v",
						s, length, args.start, args.stop, args.step, ptr.srcalls, want)
				}
			}
		}
	}
}

func TestBlockStoragePanicsOnZero(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("NewBlockStorageFactory did not panic on 0 block size")
		}
	}()
	fn, _ := newRecStorageFactory()
	_ = NewBlockStorageFactory(fn, FactoryProps{})
}

func BenchmarkBlockStorageConstruction(b *testing.B) {
	fn := newStubStorageFactory()
	s := NewBlockStorageFactory(fn, FactoryProps{1000})
	for i := 0; i < b.N; i++ {
		_ = s(1000)
	}
}

func BenchmarkBlockStorageSetRangeOneBlock(b *testing.B) {
	fn := newStubStorageFactory()
	s := NewBlockStorageFactory(fn, FactoryProps{500_000})(500_000)
	for i := 0; i < b.N; i++ {
		s.SetRange(60, 500_000, 11)
	}
}

func BenchmarkBlockStorageSetRangeFourBlocks(b *testing.B) {
	fn := newStubStorageFactory()
	s := NewBlockStorageFactory(fn, FactoryProps{128_000})(500_000)
	for i := 0; i < b.N; i++ {
		s.SetRange(60, 500_000, 11)
	}
}

func BenchmarkBlockStorageSetRangeSixteenBlocks(b *testing.B) {
	fn := newStubStorageFactory()
	s := NewBlockStorageFactory(fn, FactoryProps{32_000})(500_000)
	for i := 0; i < b.N; i++ {
		s.SetRange(60, 500_000, 11)
	}
}

var primes []uint64 = []uint64{3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997}

func storageBenchmark(newStorage StorageFunc) func(*testing.B) {
	return func(b *testing.B) {
		arr := newStorage(500_000)
		for _, p := range primes {
			b.Run(strconv.Itoa(int(p)), func(b *testing.B) {
				for i := 0; i < b.N; i++ {
					arr.SetRange((p*p)/2, 500_000, p)
				}
			})
		}
	}
}

func BenchmarkStorage(b *testing.B) {
	keys := make([]string, 0, len(storageTypes))
	for key := range storageTypes {
		keys = append(keys, key)
	}
	sort.Strings(keys)

	for _, key := range keys {
		b.Run(
			key,
			storageBenchmark(storageTypes[key]),
		)
	}
}
