//! implements a "somewhat dumb" but threadsafe and FAAAST allocator.  It will
//! allocate "blocks" of memory using a backing allocator (page allocator is a good
//! choice, but the world is your oyster).  Doesn't ever really release that memory
//! back to the OS, because we want to be greedy jerks.  Marks freed memory bits as
//! recycling (hence environmentally friendly).
//!
//! this shouldn't be objectionable, because 1) the C spec says returning free'd
//! memory to the OS is *optional*, and GoLang is even worse, it *never* returns
//! memory to the OS, which is why we call GoLang "the CLAM" at the poker table.
//!
//! this allocator only allows freeing, no other resizes.

const std = @import("std");
const Allocator = std.mem.Allocator;

const Config = struct { single_threaded: bool = false };

pub fn EnvironmentallyFriendlyBlockAllocator(config: Config) type {
    const Mutex = if (config.single_threaded) std.Thread.Mutex.Dummy else std.Thread.Mutex;

    return struct {
        allocator: Allocator,
        backing_allocator: *Allocator,
        mutex: Mutex,

        // no WAY we'll need more than 256 of these!
        used_blocks: [256]?[]u8 = [1]?[]u8{null} ** 256,
        free_blocks: [256]?[]u8 = [1]?[]u8{null} ** 256,
        total_blocks: usize = 0,

        const Self = @This();

        pub fn init(a: *Allocator) Self {
            return Self{
                .allocator = Allocator{ .allocFn = alloc, .resizeFn = resize },
                .backing_allocator = a,
                .mutex = Mutex{}, // substitute for std.Thread.Mutex.Dummy in the single-threaded case.
            };
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

            // search the previously allocated blocks for an available free slot.
            for (this.free_blocks) |block, index| {
                if (block) |b| {
                    if (b.len == n) {
                        this.mark_as_used(index);
                        return b;
                    }
                }
            } else {
                var new_block = try this.backing_allocator.alloc(u8, n);
                if (this.total_blocks < 256) {
                    this.save(new_block);
                }
                return new_block;
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
            // filter out attempts to do anything except free.
            if (new_len != 0) return error.OutOfMemory;

            const this = @fieldParentPtr(Self, "allocator", allocator);
            const hold = this.mutex.acquire();
            defer hold.release();

            // search the existing blocks for this particular memory location.
            // it's possible that it was allocated instead by the backing allocator.
            for (this.used_blocks) |block, index| {
                if (block) |b| {
                    if (b.ptr == buf.ptr) {
                        this.mark_as_free(index);
                        return 0;
                    }
                }
            } else {
                // not in there means it was done by the backing allocator.
                return (try this.backing_allocator.resize(buf, 0)).len;
            }
        }

        /// it's like a ledger.  This moves some memory from accounts receivable to accounts payable.
        /// aka asset -> liability.
        fn mark_as_used(self: *Self, free_index: usize) void {
            for (self.used_blocks) |block, used_index| {
                if (block == null) {
                    self.used_blocks[used_index] = self.free_blocks[free_index];
                    self.free_blocks[free_index] = null;
                    return;
                }
            } else unreachable; // mathematically impossible to get here without cosmic rays.
        }

        /// it's like a ledger.  This moves some memory from accounts payable to accounts receviable.
        /// aka liability -> asset.
        fn mark_as_free(self: *Self, used_index: usize) void {
            for (self.free_blocks) |block, free_index| {
                if (block == null) {
                    self.free_blocks[free_index] = self.used_blocks[used_index];
                    self.used_blocks[used_index] = null;
                    return;
                }
            } else unreachable; // mathematically impossible to get here without cosmic rays.
        }

        /// this one is a bit like the federal reserve bank.  It prints money, I mean, memory out of nowhere
        fn save(self: *Self, new_buffer: []u8) void {
            for (self.used_blocks) |block, used_index| {
                if (block == null) {
                    self.used_blocks[used_index] = new_buffer;
                    self.total_blocks += 1;
                    return;
                }
            } else unreachable; // mathematically impossible to get here without cosmic rays.
        }

        /// cleans up all stuff that hasn't been released.  Note that we want this because there's
        /// probably a bunch of free blocks (at least) that the main code *thought* was gone because
        /// it had called free (but we lied to it tee hee).  Don't freak out!  Most industrial-strength
        /// allocators also lie to the code, it's really none of the code's business whether the OS got
        /// the memory back;  At least, the C standard says so.
        pub fn deinit(self: *Self) void {
            for (self.used_blocks) |block| {
                if (block) |b| {
                    self.backing_allocator.free(b);
                }
            }

            for (self.free_blocks) |block| {
                if (block) |b| {
                    self.backing_allocator.free(b);
                }
            }
        }
    };
}

const backing_allocator = std.testing.allocator;
const assert = std.debug.assert;

test "allocator can allocate stuff and it winds up in used_blocks" {
    var a = EnvironmentallyFriendlyBlockAllocator(.{}).init(backing_allocator);
    defer a.deinit();

    var alloc = &a.allocator;

    var mem1 = try alloc.alloc(u8, 256);

    // verify that it's in used_blocks
    try std.testing.expectEqual(mem1.ptr, a.used_blocks[0].?.ptr);
}

test "allocator can deallocate stuff and it winds up in free_blocks" {
    var a = EnvironmentallyFriendlyBlockAllocator(.{}).init(backing_allocator);
    defer a.deinit();

    var alloc = &a.allocator;

    var mem1 = try alloc.alloc(u8, 256);

    alloc.free(mem1);

    // verify that it's in free_blocks
    // verify that it's not in used_blocks
    try std.testing.expectEqual(a.used_blocks[0], null);
    try std.testing.expectEqual(mem1.ptr, a.free_blocks[0].?.ptr);
}

test "allocator can allocate and deallocate stuff out of order" {
    var a = EnvironmentallyFriendlyBlockAllocator(.{}).init(backing_allocator);
    defer a.deinit();

    var alloc = &a.allocator;
    var mem1 = try alloc.alloc(u8, 256);
    var mem2 = try alloc.alloc(u8, 256);
    alloc.free(mem1);

    // verify that it's in used_blocks
    try std.testing.expectEqual(a.used_blocks[0], null);
    try std.testing.expectEqual(mem2.ptr, a.used_blocks[1].?.ptr);
    try std.testing.expectEqual(mem1.ptr, a.free_blocks[0].?.ptr);
}

test "also for the singlethreaded case." {
    var a = EnvironmentallyFriendlyBlockAllocator(.{ .single_threaded = true }).init(backing_allocator);
    defer a.deinit();

    var alloc = &a.allocator;
    var mem1 = try alloc.alloc(u8, 256);
    var mem2 = try alloc.alloc(u8, 256);
    alloc.free(mem1);

    // verify that it's in used_blocks
    try std.testing.expectEqual(a.used_blocks[0], null);
    try std.testing.expectEqual(mem2.ptr, a.used_blocks[1].?.ptr);
    try std.testing.expectEqual(mem1.ptr, a.free_blocks[0].?.ptr);
}

test "allocator can more than 256 things" {
    var a = EnvironmentallyFriendlyBlockAllocator(.{}).init(backing_allocator);
    defer a.deinit();

    var alloc = &a.allocator;
    var idx: usize = 0;
    while (idx < 256) : (idx += 1) {
        // allocate the page and throw it away BYEE
        _ = try alloc.alloc(u8, 256);
    }

    // all good.  Don't forget to manually clear this one, because it can't be
    // handled by deinit.
    var last_memory = try alloc.alloc(u8, 1024);
    defer alloc.free(last_memory);
}

test "allocator can create arrays" {
    var a = EnvironmentallyFriendlyBlockAllocator(.{}).init(backing_allocator);
    defer a.deinit();
    var alloc = &a.allocator;
    var idx: usize = 0;
    while (idx < 10) : (idx += 1) {
        _ = try alloc.alloc(u8, 500000);
    }
    _ = try alloc.create([15625]u32);
}
