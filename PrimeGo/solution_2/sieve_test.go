package main

import "testing"

type sieveSizeTestCase uint64

type sieveCountTestCase struct {
	sieveSize uint64
	storCount uint64
	props     FactoryProps
	want      uint64
}

type sieveRunTestCase struct {
	sieveSize    uint64
	props        FactoryProps
	wantSetRange []setRangeCall
	wantFind     []findCall
}

func testSieveCount(t *testing.T, tests []sieveCountTestCase, newFactory SieveFactoryFunc) {
	newStorage, mock := newRecStorageFactory() //storage mock types are defined in storage_test.go

	for _, test := range tests {
		newSieve := newFactory(test.props)
		sieve := newSieve(test.sieveSize, newStorage)

		mock.count = test.storCount
		have := sieve.CountPrimes()

		if have != test.want {
			t.Errorf("%T(%d).Count() = %d, want %d", sieve, test.sieveSize, have, test.want)
		}
		if test.sieveSize > 1 && len(mock.ccalls) == 0 {
			t.Errorf("%T(%d).Storage.Count() not called", sieve, test.sieveSize)
		}
	}
}

func testSieveRun(t *testing.T, tests []sieveRunTestCase, newFactory SieveFactoryFunc) {
	newStorage, mock := newRecStorageFactory()

	for _, test := range tests {
		srwant := test.wantSetRange
		fcwant := test.wantFind

		newSieve := newFactory(test.props)
		sieve := newSieve(test.sieveSize, newStorage)
		sieve.RunSieve()

		srhave := mock.srcalls
		fchave := mock.fcalls

		if len(srhave) != len(srwant) {
			t.Errorf("%T(%d).Storage.SetRange called %d time(s), want %d. Calls: %v",
				sieve, test.sieveSize, len(srhave), len(srwant), mock.srcalls)
		} else {
			for i := range srhave {
				if srhave[i] != srwant[i] {
					t.Errorf("%T(%d).Storage.SetRange call #%d args %v, want %v. Calls: %v",
						sieve, test.sieveSize, i, srhave[i], srwant[i], mock.srcalls)
				}
			}
		}

		if len(fchave) != len(fcwant) {
			t.Errorf("%T(%d).Storage.Find called %d time(s), want %d. Calls: %v",
				sieve, test.sieveSize, len(fchave), len(fcwant), mock.fcalls)
		} else {
			for i := range fchave {
				if fchave[i] != fcwant[i] {
					t.Errorf("%T(%d).Storage.Find call #%d args %v, want %v. Calls: %v",
						sieve, test.sieveSize, i, fchave[i], fcwant[i], mock.fcalls)
				}
			}
		}
	}
}

func testSieveSize(t *testing.T, tests []sieveSizeTestCase, newFactory SieveFactoryFunc) {
	storFunc := newStubStorageFactory()
	newSieve := newFactory(FactoryProps{32_000})

	for _, size := range tests {
		s := newSieve(uint64(size), storFunc)
		have := s.Size()
		if have != uint64(size) {
			t.Errorf("%T.Size() = %d, want %d", s, have, size)
		}
	}
}

func TestSegmentedSieve(t *testing.T) {
	runTests := []sieveRunTestCase{
		{
			0,
			FactoryProps{25},
			[]setRangeCall{},
			[]findCall{},
		},
		{
			1,
			FactoryProps{25},
			[]setRangeCall{},
			[]findCall{},
		},
		{
			100,
			FactoryProps{25},
			[]setRangeCall{
				{4, 25, 3},
				{12, 25, 5},
				{24, 25, 7},
				{25, 50, 3},
				{27, 50, 5},
				{31, 50, 7},
			},
			[]findCall{
				{false, 1, 25},
				{false, 2, 25},
				{false, 3, 25},
				{false, 4, 25},
			},
		},
		{
			100,
			FactoryProps{1000},
			[]setRangeCall{
				{4, 50, 3},
				{12, 50, 5},
				{24, 50, 7},
			},
			[]findCall{
				{false, 1, 50},
				{false, 2, 50},
				{false, 3, 50},
				{false, 4, 50},
			},
		},
	}
	sizeTests := []sieveSizeTestCase{0, 1, 2, 50, 997, 1000, 1_000_000, 1_000_000_000_000}
	countTests := []sieveCountTestCase{
		{0, 10, FactoryProps{25}, 0},
		{1, 10, FactoryProps{25}, 0},
		{2, 1, FactoryProps{25}, 1},
		{100, 25, FactoryProps{25}, 25},
		{10, 4, FactoryProps{1000}, 4},
	}

	testSieveRun(t, runTests, NewSegmentedSieveFactory)
	testSieveSize(t, sizeTests, NewSegmentedSieveFactory)
	testSieveCount(t, countTests, NewSegmentedSieveFactory)
}

func TestBaseSieve(t *testing.T) {
	runTests := []sieveRunTestCase{
		{
			0,
			FactoryProps{25},
			[]setRangeCall{},
			[]findCall{},
		},
		{
			1,
			FactoryProps{25},
			[]setRangeCall{},
			[]findCall{},
		},
		{
			100,
			FactoryProps{25},
			[]setRangeCall{
				{4, 50, 3},
				{12, 50, 5},
				{24, 50, 7},
			},
			[]findCall{
				{false, 1, 50},
				{false, 2, 50},
				{false, 3, 50},
				{false, 4, 50},
			},
		},
	}
	sizeTests := []sieveSizeTestCase{0, 1, 2, 50, 997, 1000, 1_000_000, 1_000_000_000_000}
	countTests := []sieveCountTestCase{
		{0, 10, FactoryProps{25}, 0},
		{1, 10, FactoryProps{25}, 0},
		{2, 1, FactoryProps{25}, 1},
		{100, 25, FactoryProps{25}, 25},
		{10, 4, FactoryProps{1000}, 4},
	}

	testSieveRun(t, runTests, NewBaseSieveFactory)
	testSieveSize(t, sizeTests, NewBaseSieveFactory)
	testSieveCount(t, countTests, NewBaseSieveFactory)
}
