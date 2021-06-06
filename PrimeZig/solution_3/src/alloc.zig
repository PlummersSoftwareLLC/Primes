//! implements a "somewhat dumb" but threadsafe and fast allocator.  It will
//! allocate "blocks" of a particular size.  The first "block" allocated will
//! be backed by a large memory space assigned at compile-time, that is usually
//! set aside in the .bss segment of the executable.  Subsequent allocations,
//! if desired, are performed by the backing allocator.
//!
//! this allocator only allows freeing, no other resizes.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Mutex = std.Thread.Mutex;

pub fn SnappyAllocator(comptime size: usize) type {
    return struct {
        allocator: Allocator,
        backing_allocator: *Allocator,
        mutex: Mutex,
        root_block: *[size]u8,
        root_block_available: bool = true,

        const Self = @This();

        pub fn init(a: *Allocator, root_block: *[size]u8) Self {
            return Self{
                .allocator = Allocator{ .allocFn = alloc, .resizeFn = resize },
                .root_block = root_block,
                .backing_allocator = a,
                .mutex = Mutex{},
            };
        }

        // tests if the slice is bound to the root block
        pub fn is_root(self: *Self, slice: []u8) bool {
            return @ptrToInt(slice.ptr) == @ptrToInt(self.root_block);
        }

        fn alloc(
            allocator: *Allocator,
            n: usize,
            ptr_align: u29,
            len_align: u29,
            ret_addr: usize,
        ) Allocator.Error![]u8 {
            const this = @fieldParentPtr(Self, "allocator", allocator);
            const hold = this.mutex.acquire();
            defer hold.release();

            if (this.root_block_available and (n <= size)) {
                // note that the root_block is guaranteed to be correctly aligned no matter
                // what the desired alignment is (up to std.mem.page_size)
                this.root_block_available = false;
                return this.root_block.*[0..n];
            } else {
                // punt to the backup.
                return this.backing_allocator.allocAdvancedWithRetAddr(u8, 1, n, .at_least, ret_addr);
            }
        }

        fn resize(
            allocator: *Allocator,
            buf: []u8,
            buf_align: u29,
            new_len: usize,
            len_align: u29,
            ret_addr: usize,
        ) Allocator.Error!usize {
            if (new_len != 0) return error.OutOfMemory;
            const this = @fieldParentPtr(Self, "allocator", allocator);
            const hold = this.mutex.acquire();
            defer hold.release();

            if (is_root(this, buf)) {
                this.root_block_available = true;
                return 0;
            } else {
                return (try this.backing_allocator.resize(buf, 0)).len;
            }
        }
    };
}

const scratchpad_t = if (std.builtin.is_test) [256]u8 else u0;

var scratchpad: scratchpad_t align(std.mem.page_size) = undefined;

const TestAllocator = SnappyAllocator(256);
const backing_allocator = std.testing.allocator;
const assert = std.debug.assert;

test "allocator works when you allocate stuff smaller than the blocksize" {
    var test_allocator = TestAllocator.init(backing_allocator, &scratchpad);
    defer assert(test_allocator.root_block_available);

    var allocator = &test_allocator.allocator;

    var slice1 = try allocator.alloc(u8, 128);
    defer allocator.free(slice1);

    var slice2 = try allocator.alloc(u8, 128);
    defer allocator.free(slice2);

    assert(test_allocator.is_root(slice1));
    assert(!test_allocator.is_root(slice2));
    assert(!test_allocator.root_block_available);
}

test "allocator works when you allocate stuff of the same size as the blocksize" {
    var test_allocator = TestAllocator.init(backing_allocator, &scratchpad);
    defer assert(test_allocator.root_block_available);

    var allocator = &test_allocator.allocator;

    var slice1 = try allocator.alloc(u8, 256);
    defer allocator.free(slice1);

    var slice2 = try allocator.alloc(u8, 256);
    defer allocator.free(slice2);

    assert(test_allocator.is_root(slice1));
    assert(!test_allocator.is_root(slice2));
    assert(!test_allocator.root_block_available);
}

test "allocator works when you allocate stuff bigger than the blocksize" {
    var test_allocator = TestAllocator.init(backing_allocator, &scratchpad);
    defer assert(test_allocator.root_block_available);

    var allocator = &test_allocator.allocator;

    var slice1 = try allocator.alloc(u8, 512);
    defer allocator.free(slice1);

    var slice2 = try allocator.alloc(u8, 512);
    defer allocator.free(slice2);

    assert(!test_allocator.is_root(slice1));
    assert(!test_allocator.is_root(slice2));
    assert(test_allocator.root_block_available);

    var slice3 = try allocator.alloc(u8, 128);
    defer allocator.free(slice3);

    assert(test_allocator.is_root(slice3));
    assert(!test_allocator.root_block_available);
}
