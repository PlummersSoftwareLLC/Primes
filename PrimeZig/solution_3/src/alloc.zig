//! It turns out that calloc has some dark magic speed tricks that it makes
//! sense to try to use instead of rebuilding noah's ark.  For simplicity, we
//! only allow completely freeing the memory space.

const std = @import("std");
const builtin = std.builtin;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const mem = std.mem;
const c_stdlib = @cImport({
    @cInclude("stdlib.h");
});

fn CAlloc(c: anytype) type {
    return struct {
        allocator: Allocator = .{.allocFn = alloc, .resizeFn = resize},

        // shamelessly copied from https://github.com/ziglang/zig/blob/master/lib/std/heap.zig
        comptime {
            if (!builtin.link_libc) {
                @compileError("ClearingAllocator is only available when linking against libc");
            }
        }

        fn getHeader(ptr: [*]u8) *[*]u8 {
            return @intToPtr(*[*]u8, @ptrToInt(ptr) - @sizeOf(usize));
        }

        fn alignedAlloc(len: usize, alignment: usize) ?[*]u8 {
            // Thin wrapper around regular calloc, overallocate to account for
            // alignment padding.
            const length_with_padding = len + alignment - 1 + @sizeOf(usize);
            var index: usize = 0;
            var unaligned_ptr = @ptrCast([*]u8, c.calloc(length_with_padding, 1) orelse return null);
            const unaligned_addr = @ptrToInt(unaligned_ptr);
            const aligned_addr = mem.alignForward(unaligned_addr + @sizeOf(usize), alignment);
            var aligned_ptr = unaligned_ptr + (aligned_addr - unaligned_addr);
            getHeader(aligned_ptr).* = unaligned_ptr;
            return aligned_ptr;
        }

        fn alignedFree(ptr: [*]u8) void {
            const unaligned_ptr = getHeader(ptr).*;
            c.free(unaligned_ptr);
        }

        fn alloc(
            allocator: *Allocator,
            len: usize,
            alignment: u29,
            len_align: u29,
            return_address: usize,
        ) error{OutOfMemory}![]u8 {
            _ = allocator;
            _ = return_address;
            _ = len_align;
            assert(len > 0);
            assert(std.math.isPowerOfTwo(alignment));

            var ptr = alignedAlloc(len, alignment) orelse return error.OutOfMemory;
            return ptr[0..len];
        }

        fn resize(
            allocator: *Allocator,
            buf: []u8,
            buf_align: u29,
            new_len: usize,
            len_align: u29,
            return_address: usize,
        ) Allocator.Error!usize {
            _ = allocator;
            _ = buf_align;
            _ = return_address;
            _ = len_align;

            if (new_len == 0) {
                alignedFree(buf.ptr);
                return 0;
            }

            return error.OutOfMemory;
        }
    };
}

// instantiate calloc, as wrapped by zig.
var calloc: CAlloc(c_stdlib) = .{};

pub fn calloc_aligned(comptime T: type, count: usize, comptime alignment: u28) ![]T {
    return try calloc_aligned_gen(T, &calloc.allocator, count, alignment);
}

pub fn free(slice: anytype) void {
    free_gen(slice, &calloc.allocator);
}

// generalized aligned, clearing allocator implementation.
fn calloc_aligned_gen(comptime T: type, allocator: *Allocator, count: usize, comptime alignment: u28) ![]T {
    const slice = try allocator.allocAdvanced(T, alignment, count, .exact);
    if (getRuntimeSafety()) {
        // zig drops 0xaa in all of the data slots in the case of release-safe modes.
        const sliceptr = @ptrCast([*]u8, slice.ptr);
        @memset(sliceptr, 0, count * @sizeOf(T));
    }
    return slice;
}

// generalized clearing allocator.
fn free_gen(slice: anytype, allocator: *Allocator) void {
    allocator.free(slice);
}

fn getRuntimeSafety() comptime bool {
    return switch(std.builtin.mode) {
        std.builtin.Mode.Debug => true,
        std.builtin.Mode.ReleaseSafe => true,
        std.builtin.Mode.ReleaseFast => false,
        std.builtin.Mode.ReleaseSmall => false,
    };
}

test "produces an aligned memory slot that is empty" {
    const datatypes = .{u8, u32, u64};

    inline for (datatypes) |T| {
        const alignment = @alignOf(T);
        var slice = try calloc_aligned(T, 10, @alignOf(T));
        defer free(slice);

        try std.testing.expectEqual(@ptrToInt(slice.ptr) % alignment, 0);

        for (slice) |value| {
            try std.testing.expectEqual(value, 0);
        }
    }
}

test "can produce higher alignments" {
    const datatypes = .{u8, u32, u64};

    inline for (datatypes) |T| {
        const alignment = @alignOf(T);
        var slice = try calloc_aligned(T, 10, std.mem.page_size);
        defer free(slice);

        try std.testing.expectEqual(@ptrToInt(slice.ptr) % alignment, 0);

        for (slice) |value| {
            try std.testing.expectEqual(value, 0);
        }
    }
}

const testing_allocator = std.testing.allocator;

var last_allocation: usize = undefined;
// instantiate a mock calloc, which instead uses test_allocator, to double check
// we aren't leaking memory.  Definitely NOT threadsafe, only one at a time plz.
var mock_calloc: CAlloc(struct {
    pub fn calloc(count: usize, bytes: usize) ?[*]u8 {
        last_allocation = count * bytes;
        const slice = testing_allocator.alloc(u8, last_allocation) catch |_| return null;
        return @ptrCast([*]u8, slice.ptr);
    }
    pub fn free(pointer: [*]u8) void {
        const slice = pointer[0..last_allocation];
        testing_allocator.free(slice);
    }
}) = .{};

fn mock_calloc_aligned(comptime T: type, count: usize, comptime alignment: u28) ![]T {
    return try calloc_aligned_gen(T, &mock_calloc.allocator, count, alignment);
}

fn mock_free(slice: anytype) void {
    free_gen(slice, &mock_calloc.allocator);
}

test "can produce higher alignments" {
    const datatypes = .{u8, u32, u64};

    inline for (datatypes) |T| {
        const alignment = @alignOf(T);
        var slice = try mock_calloc_aligned(T, 10, std.mem.page_size);
        defer mock_free(slice);

        try std.testing.expectEqual(@ptrToInt(slice.ptr) % alignment, 0);

        for (slice) |value| {
            try std.testing.expectEqual(value, 0);
        }
    }
}