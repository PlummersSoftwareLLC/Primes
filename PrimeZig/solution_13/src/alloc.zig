//! It turns out that calloc has some dark magic speed tricks that it makes
//! sense to try to use instead of rebuilding noah's ark.  For simplicity, we
//! only allow completely freeing the memory space.

const std = @import("std");
pub const c_std_lib = @cImport({@cInclude("stdlib.h");});
pub const default_allocator = CAlloc(c_std_lib);
const page_size = std.mem.page_size;

// common getHeader function that is going to be used by both calloc and malloc.
fn getHeader(ptr: [*] u8) *[*]u8 {
    return @intToPtr(*[*]u8, @ptrToInt(ptr) - @sizeOf(usize));
}

/// shims c standard library's "calloc" function.
pub fn CAlloc(c: anytype) type {
    return struct {
        pub fn calloc_pages(comptime init: anytype, count: usize) ![*]u8 {
            // CAlloc can only be used with 0-ish initialization values.
            const init_val = if (@TypeOf(init) == bool) @boolToInt(init) else init;
            std.debug.assert(init_val == 0);

            // calculate how many pages we'll need.
            const pages = count / page_size + if (count % page_size == 0) @as(usize, 0) else @as(usize, 1);
            const pages_len = pages * page_size;
            const length_with_padding = pages_len + page_size + @sizeOf(usize);

            // fetch the desired memory with c's calloc function, calculate aligned pointer.
            // it's possible calloc can try to optimize by using word-length allocations, so use those.
            var unaligned_ptr = @ptrCast([*]u8,
              c.calloc(length_with_padding / @sizeOf(usize), @sizeOf(usize)) orelse return error.OutOfMemory);

            const unaligned_addr = @ptrToInt(unaligned_ptr);
            const aligned_addr = std.mem.alignForward(unaligned_addr + @sizeOf(usize), page_size);
            var aligned_ptr = unaligned_ptr + (aligned_addr - unaligned_addr);

            // assert that we have the desired alignment.
            std.debug.assert(aligned_addr % page_size == 0);

            getHeader(aligned_ptr).* = unaligned_ptr;
            return @ptrCast([*]u8, aligned_ptr);
        }

        pub fn free(ptr: [*]u8) void {
            const unaligned_ptr = getHeader(ptr).*;
            c.free(unaligned_ptr);
        }

        pub fn allocator_deinit() void {}
    };
}

const SAllocOpts = struct {
    should_clear: bool = true,
};

/// shims c standard library's "malloc" function, followed by an (optional) memset operation.
pub fn SAlloc(c: anytype, _opts: anytype) type {
    const opts: SAllocOpts = _opts;
    return struct {
        pub fn calloc_pages(comptime init: anytype, count: usize) ![*]u8 {
            // calculate how many pages we'll need.
            const pages = count / page_size + if (count % page_size == 0) @as(usize, 0) else @as(usize, 1);
            const pages_len = pages * page_size;
            const length_with_padding = pages_len + page_size + 1;

            // fetch the desired memory with c's calloc function, calculate aligned pointer.
            var unaligned_ptr = @ptrCast([*]u8, c.malloc(length_with_padding) orelse return error.OutOfMemory);
            const unaligned_addr = @ptrToInt(unaligned_ptr);
            const aligned_addr = std.mem.alignForward(unaligned_addr + @sizeOf(usize), page_size);
            var aligned_ptr = unaligned_ptr + (aligned_addr - unaligned_addr);

            // memset, but only memset the bytes we ask for!
            if (opts.should_clear) {
                const init_val = if (@TypeOf(init) == bool) @boolToInt(init) else init;
                @memset(aligned_ptr, init_val, count);
            }

            // assert that we have the desired alignment.
            std.debug.assert(aligned_addr % page_size == 0);

            getHeader(aligned_ptr).* = unaligned_ptr;
            return @ptrCast([*]u8, aligned_ptr);
        }

        pub fn free(ptr: [*]u8) void {
            const unaligned_ptr = getHeader(ptr).*;
            c.free(unaligned_ptr);
        }

        pub fn allocator_deinit() void {}
    };
}

threadlocal var cached_ptr: ?[*]u8 = null;
threadlocal var cached_len: usize = undefined;

const V = std.meta.Vector(8, u64);
const VAllocOpts = struct{
    allocator: *std.mem.Allocator = std.heap.page_allocator,
    should_clear: bool = true,
};

/// for super-fast threadsafe "single-use" allocations.
/// vector-allocates memory, using the direct page allocator and a per-thread cache.  Internally remembers
/// how much you allocated the last time, so each thread can only hold onto one allocation at a time.
pub fn VAlloc(comptime opts: VAllocOpts) type {
    const allocator = opts.allocator;
    return struct {
        pub fn calloc_pages(comptime init: anytype, count: usize) ![*]u8 {
            // align the number of total bytes allocated to page_size.
            const overhang = count % page_size;
            const bytes = if (overhang == 0) count else count + page_size - overhang;

            const result = ptr: {
                if (cached_ptr) |cache| {
                    if (cached_len == count) {
                        break :ptr cache;
                    } else {
                        allocator.free(cache[0..cached_len]);
                        const new_slice = try allocator.allocAdvanced(u8, page_size, bytes, .at_least);
                        break :ptr @ptrCast([*]u8, new_slice.ptr);
                    }
                } else {
                    const new_slice = try allocator.allocAdvanced(u8, page_size, bytes, .at_least);
                    break :ptr @ptrCast([*]u8, new_slice.ptr);
                }
            };

            cached_len = bytes;

            if (opts.should_clear) {
                const extra_bytes = count % @sizeOf(V);
                const total_vectors = count / @sizeOf(V) + if (extra_bytes == 0) @as(usize, 0) else @as(usize, 1);
                const vector_ptr = @ptrCast([*]V, @alignCast(@alignOf(V), result));
                vector_clear(vector_ptr[0..total_vectors], init);
            }

            return result;
        }

        pub fn free(ptr: [*]u8) void {
            // save it as "cached".  cached_len should be remembered from previous invocation.
            cached_ptr = ptr;
        }

        pub fn allocator_deinit() void {
            if (cached_ptr) | cache | {
                allocator.free(cache[0..cached_len]);
            }
            // set the cached_ptr to nil in case we want to reuse it (as in, in tests)
            cached_ptr = null;
        }

        fn vector_clear(slice: []V, content: anytype) void {
            var byte_buffer: [64]u8 align(@alignOf(V)) = undefined;

            //initialize the byte buffer
            for (byte_buffer) | *byte | {
                byte.* = if (@TypeOf(content) == bool) @boolToInt(content) else content;
            }

            const membuff: V = @ptrCast(*V, &byte_buffer[0]).*;
            for (slice) | *vector | {
                vector.* = membuff;
            }
        }
    };
}

// for testing purposes only, a mock of the c stdlib that provides calloc and malloc backed by the
// testing allocator.  Stores last length in a global: Definitely not threadsafe.
var test_allocator_total_length: usize = undefined;

const MockedLibC = struct {
    pub fn calloc(count: usize, size: usize) ?[*]u8 {
        test_allocator_total_length = count * size;
        var slice = std.testing.allocator.alloc(u8, test_allocator_total_length) catch return null;
        const result_pointer = @ptrCast([*]u8, slice.ptr);
        @memset(result_pointer, 0, test_allocator_total_length);
        return result_pointer;
    }

    pub fn malloc(byte_length: usize) ?[*]u8 {
        test_allocator_total_length = byte_length;
        var slice = std.testing.allocator.alloc(u8, test_allocator_total_length) catch return null;
        return @ptrCast([*]u8, slice.ptr);
    }

    pub fn free(memory: [*]u8) void {
        std.testing.allocator.free(memory[0..test_allocator_total_length]);
        test_allocator_total_length = undefined;
    }
};

const TWOPAGES = page_size * 2;

fn expectSliceAligned(slice: anytype) !void {
    try std.testing.expectEqual(@ptrToInt(slice.ptr) % page_size, 0);
}

fn basic_test(a: anytype, size: usize, comptime expected_value: anytype) !void {
    const expected = if (@TypeOf(expected_value) == bool) @boolToInt(expected_value) else expected_value;
    var ptr = try a.calloc_pages(expected_value, size);
    var slice = ptr[0..size];
    defer a.free(ptr);

    try expectSliceAligned(slice);

    for (slice) |value| {
        try std.testing.expectEqual(value, expected);
    }
}

// tests on CAlloc
test "CAlloc produces an aligned memory slot that is filled with zeros." {
    const a = CAlloc(c_std_lib);
    defer a.allocator_deinit();
    try basic_test(a, 10, 0);
}

test "CAlloc produces an aligned memory slot that is filled with bools." {
    const a = CAlloc(c_std_lib);
    defer a.allocator_deinit();
    try basic_test(a, 10, false);
}

test "CAlloc can produce multi-page allocations" {
    const a = CAlloc(c_std_lib);
    defer a.allocator_deinit();
    try basic_test(a, TWOPAGES, 0);
}

test "CAlloc doesn't leak on small allocations" {
    const a = CAlloc(MockedLibC);
    defer a.allocator_deinit();
    try basic_test(a, 10, 0);
}

test "CAlloc doesn't leak on multi-page allocations" {
    const a = CAlloc(MockedLibC);
    defer a.allocator_deinit();
    try basic_test(a, TWOPAGES, 0);
}

// tests on SAlloc
test "SAlloc produces an aligned memory slot that is filled with zeros." {
    const a = SAlloc(c_std_lib, .{});
    defer a.allocator_deinit();
    try basic_test(a, 10, 0);
}

test "SAlloc produces an aligned memory slot that is filled with false." {
    const a = SAlloc(c_std_lib, .{});
    defer a.allocator_deinit();
    try basic_test(a, 10, false);
}

test "SAlloc produces an aligned memory slot that is filled with true." {
    const a = SAlloc(c_std_lib, .{});
    defer a.allocator_deinit();
    try basic_test(a, 10, true);
}

test "SAlloc produces an aligned memory slot that is filled with arbitrary values." {
    const a = SAlloc(c_std_lib, .{});
    defer a.allocator_deinit();
    try basic_test(a, 10, 0xFF);
}

test "SAlloc can produce multi-page allocations" {
    const a = SAlloc(c_std_lib, .{});
    defer a.allocator_deinit();
    try basic_test(a, TWOPAGES, 0);
}

test "SAlloc can produce multi-page allocations with arbitrary values" {
    const a = SAlloc(c_std_lib, .{});
    defer a.allocator_deinit();
    try basic_test(a, TWOPAGES, 0xFF);
}

test "SAlloc doesn't leak on small allocations" {
    const a = SAlloc(MockedLibC, .{});
    defer a.allocator_deinit();
    try basic_test(a, 10, 0);
}

test "SAlloc doesn't leak on multi-page allocations" {
    const a = SAlloc(MockedLibC, .{});
    defer a.allocator_deinit();
    try basic_test(a, TWOPAGES, 0);
}

// tests on VAlloc
test "VAlloc produces an aligned memory slot that is filled with zeros." {
    const a = VAlloc(.{});
    defer a.allocator_deinit();
    try basic_test(a, 10, 0);
}

test "VAlloc produces an aligned memory slot that is filled with arbitrary values." {
    const a = VAlloc(.{});
    defer a.allocator_deinit();
    try basic_test(a, 10, 0xFF);
}

test "VAlloc produces an aligned memory slot that is filled with false." {
    const a = VAlloc(.{});
    defer a.allocator_deinit();
    try basic_test(a, 10, false);
}

test "VAlloc produces an aligned memory slot that is filled with true." {
    const a = VAlloc(.{});
    defer a.allocator_deinit();
    try basic_test(a, 10, true);
}

test "VAlloc can produce multi-page allocations" {
    const a = VAlloc(.{});
    defer a.allocator_deinit();
    try basic_test(a, TWOPAGES, 0);
}

test "VAlloc can produce multi-page allocations with arbitrary values" {
    const a = VAlloc(.{});
    defer a.allocator_deinit();
    try basic_test(a, TWOPAGES, 0xFF);
}

test "VAlloc doesn't leak on small allocations" {
    const a = VAlloc(.{.allocator = std.testing.allocator});
    defer a.allocator_deinit();
    try basic_test(a, 10, 0);
}

test "VAlloc doesn't leak on multi-page allocations" {
    const a = VAlloc(.{.allocator = std.testing.allocator});
    defer a.allocator_deinit();
    try basic_test(a, TWOPAGES, 0);
}

// comptime allocator, used by the wheel.  Uses the same duck-typed interface
// as the other allocators.
pub fn comptimeAlloc(comptime tag: anytype, comptime T: type, comptime size: usize) *[size]T {
    _ = tag;
    return @ptrCast(*[size]T, &struct {
        // this is a dirty zig trick that is very likely not going to be a thing in the future.
        var buffer: [size]u8 align(std.mem.page_size) = undefined;
    }.buffer);
}