package main

var storageMods map[string]StorageFactoryFunc = map[string]StorageFactoryFunc{
	"blocks":  NewBlockStorageFactory,
	"rblocks": NewReverseBlockStorageFactory,
}

// Interface Storage provides some of the methods one would expect from a
// bit storage type
type Storage interface {
	At(index uint64) bool
	Set(index uint64, val bool)
	SetRange(start, stop, step uint64)
	Find(val bool, start, stop uint64) uint64
	Count(val bool, start, stop uint64) uint64
	Length() uint64
}

type StorageFunc func(uint64) Storage

type StorageFactoryFunc func(StorageFunc, FactoryProps) StorageFunc

// BlockStorage wraps a storage type to make the SetRange method
// more cpu cache-friendly
type BlockStorage struct {
	Storage
	blockSize uint64
}

func NewBlockStorageFactory(storage StorageFunc, props FactoryProps) StorageFunc {
	if props.blockSize == 0 {
		panic("Block size 0 passed to NewBlockStorageFactory")
	}
	return func(length uint64) Storage {
		return BlockStorage{
			storage(length),
			props.blockSize,
		}
	}
}

// SetRange splits (start, stop, step) range into blocks and then calls
// the underlying Storage's SetRange for every block.
// Note that some implementations of SetRange have a constant overhead
// on each call, so a block size shouldn't be too small.
func (b BlockStorage) SetRange(start, stop, step uint64) {
	if stop > b.Length() {
		stop = b.Length()
	}
	for block := start + b.blockSize; block < stop; block += b.blockSize {
		b.Storage.SetRange(start, block, step)
		start += ((block - start + step - 1) / step) * step
	}
	b.Storage.SetRange(start, stop, step)
}

// ReverseBlockStorage works like a BlockStorage, but reverses the order
// of blocks on each call, so that, except on the first call, one of the blocks
// being processed is still cached.
type ReverseBlockStorage struct {
	Storage
	blockSize uint64
	reverse   bool
}

func NewReverseBlockStorageFactory(storage StorageFunc, props FactoryProps) StorageFunc {
	if props.blockSize == 0 {
		panic("Block size 0 passed to NewBlockStorageFactory")
	}
	return func(length uint64) Storage {
		return &ReverseBlockStorage{
			storage(length),
			props.blockSize,
			false,
		}
	}
}

// SetRange splits (start, stop, step) range into blocks and then calls
// the underlying Storage's SetRange for every block, reversing the order
// of blocks after that
func (b *ReverseBlockStorage) SetRange(start, stop, step uint64) {

	if !b.reverse {
		for block := start + b.blockSize; block < stop; block += b.blockSize {
			b.Storage.SetRange(start, block, step)

			start += ((block - start + step - 1) / step) * step
		}
		b.Storage.SetRange(start, stop, step)
	} else {
		var rstart uint64
		rstop := stop - ((stop-start)/b.blockSize)*b.blockSize
		for block := stop; block > rstop; block -= b.blockSize {
			rstart = start + ((block-b.blockSize-start)/step)*step
			b.Storage.SetRange(rstart, block, step)
		}
		b.Storage.SetRange(start, rstop, step)
	}

	b.reverse = !b.reverse
}
