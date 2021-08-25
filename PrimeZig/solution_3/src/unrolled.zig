//! implements fully unrolled functions.
//!
//! Dense functions:
//! One dense function will be generated per "odd possibility" for any given
//! datatype, even if it's not prime.  This function will tag each bit "down
//! the line", one by one, with a stride corresponding to the odd possibility.
//! E.G. for u16, there will be a function each of strides {1, 3, 5, 7, 9, 11,
//! 13, 15}.  Obviously strides 1, 9, and 15 are composite, so they won't be
//! ever be called, but we don't need any pre-knowledge of which numbers are
//! prime in order to take advantage of this optimization, and these
//! pre-computed functions will be available even if the prime sieve length
//! is too short to take advantage of them.
//!
//! Sparse functions:
//! One sparse function will be generated per "odd possibility".  These are
//! to be used when the size of the stride is greater than the number of bits
//! in the representation.

const std = @import("std");

pub fn isDense(comptime T: type, comptime half_extent: bool, factor: usize) bool {
    if (half_extent) {
        return factor < @bitSizeOf(T);
    } else {
        return factor / 2 < @bitSizeOf(T);
    }
}

fn lutCount(comptime T: type, comptime half_extent: bool) usize {
    if (half_extent) {
        return @bitSizeOf(T) / 2;
    } else {
        return @bitSizeOf(T);
    }
}

///////////////////////////////////////////////////////////////////////////////
// DENSE FUNCTIONS
// these functions have more than one bit flip per datatype and thus they cache
// the bit flipping operations in a (possibly SSE) register, condensing the
// read-modify-write cycle.

/// the type of a function that fills out multiples of an array-pointer for a
/// given type, for dense values.  First parameter is an array of the type; second
/// parameter is the number of integers in this array.
fn DenseFn(comptime T: type) type {
    return fn([*]T, usize) void;
}

const UnrolledOpts = struct {
    primeval: u1 = 0,          // are 1s or 0's prime?
    max_vector: ?u32 = null,   // should we use vectors?  what's the biggest vector size?
    half_extent: bool = false, // how many lookup entries?
};

/// returns a function wrapped in an struct; this is the simplest variadic way
/// of assigning  a function "identifier" in zig to an integer value which
/// preserves the integer semantics of the funciton.
fn DenseFnFactory(comptime T: type, comptime num: usize, opts: UnrolledOpts) type {
    return struct{
        fn fill(field: [*]T, field_count: usize) void {
            @setAlignStack(256);
            fillOneChunk(field, true);
            var offset : usize = num;
            while (offset < field_count) : (offset += num) {
                fillOneChunk(field + offset, false);
            }
            fillOneChunk(field + offset, false);
        }

        /// a function that is expected to fill *num* ints with bits flagged starting
        /// from index num/2, spaced out by *num* bits.
        inline fn fillOneChunk(chunk: [*]T, comptime start_at_square: bool) void {
            comptime var int_offset: usize = 0;
            comptime var count: usize = 0; // for sanity checking purposes only.
            comptime var bit_location: usize = num / 2;
            comptime var next_boundary: usize = 0;
            inline while (int_offset < num) {
                comptime const V = BestVectorFor(num - int_offset, opts.max_vector);
                comptime const ua_ptr_t = Unalign(V);
                var mem: ua_ptr_t = @ptrCast(ua_ptr_t, chunk + int_offset);
                next_boundary += comptime intsConsumed(V) * @bitSizeOf(T);

                // runtime
                var cache: V = mem.*;
                inline while (bit_location < next_boundary) : (bit_location += num) {

                    if ((!start_at_square) or ((2 * bit_location + 1) >= num * num)) {
                        setBit(V, &cache, bit_location - int_offset * @bitSizeOf(T));
                    }

                    count += 1;
                }
                mem.* = cache;
                // end runtime

                int_offset += comptime intsConsumed(V);
            }

            // this should be true due to theoretical algebra, but let's add this
            // check in to ensure sanity.
            std.debug.assert(count == @bitSizeOf(T));
        }

        inline fn setBit(comptime V: type, cache: *V, comptime bit_index: usize) void {
            const shift_t = ShiftTypeFor(T);
            comptime const shift = @intCast(shift_t, bit_index % @bitSizeOf(T));
            switch (V) {
                T =>
                    if (opts.primeval == 0) {
                        comptime const mask = @as(T, 1) << shift;
                        cache.* |= mask;
                    } else {
                        comptime const mask = ~(@as(T, 1) << shift);
                        cache.* &= mask;
                    },
                else => |Vector| {
                    comptime const int_index = bit_index / @bitSizeOf(T);
                    if (opts.primeval == 0) {
                        comptime const mask = @as(T, 1) << shift;
                        cache.*[int_index] |= mask;
                    } else {
                        comptime const mask = ~(@as(T, 1) << shift);
                        cache.*[int_index] &= mask;
                    }
                }
            }
        }

        // VECTOR/INT UTILITY FUNCTIONS
        fn BestVectorFor(comptime ints_left: usize, comptime max_dimension: ?u32) type {
            if (max_dimension) |max_dim| {
                std.debug.assert(std.math.isPowerOfTwo(max_dim));
                return if (max_dim < ints_left) std.meta.Vector(max_dim, T) else T;
                //var try_dim = max_dim;
                //while (try_dim > ints_left) : (try_dim /= 2) {}
                //return if (try_dim == 1) T else std.meta.Vector(try_dim, T);
            } else {
                return T;
            }
        }

        fn Unalign(comptime V: type) type {
            switch(@typeInfo(V)) {
                .Int => return *V,
                .Vector => return * align(@alignOf(T)) V,
                else => @compileError("unreachable")
            }
        }

        fn intsConsumed(comptime V: type) usize {
            return switch (@typeInfo(V)) {
                .Int => 1,
                .Vector => |vec| vec.len,
                else => @compileError("unreachable")
            };
        }
    };
}

/// builds a function LUT.  Must be run at comptime.
pub fn makeDenseLUT(comptime T: type, opts: UnrolledOpts) [lutCount(T, opts.half_extent)]DenseFn(T) {
    // we only need odd factors.
    const count = lutCount(T, opts.half_extent);
    var myFuns: [count]DenseFn(T) = undefined;
    for (myFuns) | *f, index | {
        f.* = DenseFnFactory(T, 2 * index + 1, opts).fill;
    }
    return myFuns;
}

/// based on how big the field is, and the factor, how many times do you have to
/// loop the unrolled function to cover the field.  This is an expensive integer
/// division but it only has to happen once.  Note that you might think it would be
/// +1, but it will be preceded by a run with the "squares"
fn denseCycles(comptime T: type, field_count: usize, factor: usize) usize {
    const result = (field_count / (factor * @bitSizeOf(T)));
    return result;
}

fn clear(slice: anytype) void{
    for (slice) |*item| {
        switch (@typeInfo(@TypeOf(item.*))) {
            .Vector => | vector | {
                comptime var index: usize = 0;
                inline while (index < vector.len) : (index += 1) {
                    item.*[index] = 0;
                }
            },
            .Int => item.* = 0,
            else => {
                @panic("unsupported");
            }
        }
    }
}

const allocator = std.testing.allocator;
test "it is possible to fill a u8 memory segment with factors of 3" {
    const fillChunk = DenseFnFactory(u8, 3, .{}).fillOneChunk;
    var seg = try allocator.alloc(u8, 3);
    defer allocator.free(seg);
    clear(seg);

    fillChunk(@ptrCast([*]u8, seg), false);

    const result = [_]u8{0b1001_0010, 0b0010_0100, 0b0100_1001};
    try std.testing.expectEqualSlices(u8, seg, result[0..]);
}

test "it is possible to fill a u8 memory segment with factors of 5" {
    const fillChunk = DenseFnFactory(u8, 5, .{}).fillOneChunk;
    var seg = try allocator.alloc(u8, 5);
    defer allocator.free(seg);
    clear(seg);

    fillChunk(@ptrCast([*]u8, seg), false);

    const result = [_]u8{0b1000_0100, 0b0001_0000, 0b0100_0010, 0b0000_1000, 0b0010_0001};
    try std.testing.expectEqualSlices(u8, seg, result[0..]);
}

test "it is possible to fill a u16 memory segment with factors of 11" {
    const fillChunk = DenseFnFactory(u16, 11, .{}).fillOneChunk;
    var seg = try allocator.alloc(u16, 11);
    defer allocator.free(seg);
    clear(seg);

    fillChunk(@ptrCast([*]u16, seg), false);

    const result = [_]u16{0b0000_0000_0010_0000, 0b0000_1000_0000_0001, 0b0000_0000_0100_0000, 0b0001_0000_0000_0010,
                          0b0000_0000_1000_0000, 0b0010_0000_0000_0100, 0b0000_0001_0000_0000, 0b0100_0000_0000_1000,
                          0b0000_0010_0000_0000, 0b1000_0000_0001_0000, 0b0000_0100_0000_0000};

    try std.testing.expectEqualSlices(u16, seg, result[0..]);
}

test "it is possible to fill a u8 vector segment with factors of 5" {
    const fillChunk = DenseFnFactory(u8, 5, .{.max_vector = 2}).fillOneChunk;
    var seg = try allocator.alloc(u8, 5);
    defer allocator.free(seg);
    clear(seg);

    fillChunk(@ptrCast([*]u8, seg), false);

    const result = [_]u8{0b1000_0100, 0b0001_0000, 0b0100_0010, 0b0000_1000, 0b0010_0001};
    try std.testing.expectEqualSlices(u8, seg, result[0..]);
}

test "it is possible to fill a u8 memory segment with factors of 3 that starts at the square" {
    const fillChunk = DenseFnFactory(u8, 3, .{}).fillOneChunk;
    var seg = try allocator.alloc(u8, 3);
    defer allocator.free(seg);
    clear(seg);

    fillChunk(@ptrCast([*]u8, seg), true);

    const result = [_]u8{0b1001_0000, 0b0010_0100, 0b0100_1001};
    try std.testing.expectEqualSlices(u8, seg, result[0..]);
}

test "it is possible to fill a u8 memory segment with factors of 5 that starts at the square" {
    const fillChunk = DenseFnFactory(u8, 5, .{}).fillOneChunk;
    var seg = try allocator.alloc(u8, 5);
    defer allocator.free(seg);
    clear(seg);

    fillChunk(@ptrCast([*]u8, seg), true);

    const result = [_]u8{0b0000_0000, 0b0001_0000, 0b0100_0010, 0b0000_1000, 0b0010_0001};
    try std.testing.expectEqualSlices(u8, seg, result[0..]);
}

test "it is possible to fill a u8 vector segment with factors of 5 that starts at the square" {
    const fillChunk = DenseFnFactory(u8, 5, .{.max_vector = 2}).fillOneChunk;
    var seg = try allocator.alloc(u8, 5);
    defer allocator.free(seg);
    clear(seg);

    fillChunk(@ptrCast([*]u8, seg), true);

    const result = [_]u8{0b0000_0000, 0b0001_0000, 0b0100_0010, 0b0000_1000, 0b0010_0001};
    try std.testing.expectEqualSlices(u8, seg, result[0..]);
}

///////////////////////////////////////////////////////////////////////////////
// SPARSE FUNCTIONS
// these functions are for when the odd number is large enough that there is
// less than one bit flip per integer chunk.  For the sparse functions, you
// specify the field_size and it will run the loop automatically.

fn SparseFn(comptime T: type) type {
    return fn([*]T, usize, usize) void;
}

pub fn makeSparseLUT(comptime T: type, opts: UnrolledOpts) [lutCount(T, opts.half_extent)]SparseFn(T) {
    var myFuns: [lutCount(T, opts.half_extent)]SparseFn(T) = undefined;
    for (myFuns) | *f, index | {
        f.* = SparseFnFactory(T, 2 * index + 1, opts).fill;
    }
    return myFuns;
}

var my_factor: usize = undefined;
var my_index: usize = undefined;
var my_field: usize = undefined;

fn SparseFnFactory(comptime T: type, comptime progressive_shift: usize, opts: UnrolledOpts) type {
    return struct{
        fn fill(field: [*]T, field_ints: usize, factor: usize) void {
            @setAlignStack(256);
            const square_offset = (factor * factor) / (2 * @bitSizeOf(T));
            const stride = factor / @bitSizeOf(T) - 1;
            // one-time, expensive division.
            const chunk_count = (field_ints - square_offset) / factor + 1;

            var index: usize = 0;
            var chunk = field + square_offset;

            my_factor = factor;
            my_field = @ptrToInt(field);

            while (index < chunk_count) : (index += 1) {
                my_index = index;
                fillOneChunk(chunk, stride);
                chunk = chunk + factor;  // move the chunk pointer over.
            }
        }

        /// note: chunk should be aligned to contain the "first position", which
        /// is the naturalSquareBitIndex of the functional index number.
        inline fn fillOneChunk(chunk: [*]T, stride: usize) void {
            comptime var position = naturalSquareBitIndex();
            comptime var index = 0;
            inline while (index < @bitSizeOf(T)) : (index += 1) {
                setBit(chunk, position, index, stride);
                position += progressive_shift + @bitSizeOf(T);
            }
        }

        inline fn naturalSquareBitIndex() usize {
            // create an example of a number that has this progressive_shift
            const example = progressive_shift + @bitSizeOf(T);
            // figure out what the square index is
            return ((example * example) / 2) % @bitSizeOf(T);
        }

        inline fn setBit(chunk: [*]T, comptime chunk_bit_index: usize, comptime iteration: usize, stride: usize) void {
            const shift_t = ShiftTypeFor(T);
            comptime const shift = @intCast(shift_t, chunk_bit_index % @bitSizeOf(T));
            comptime const unstrided_index = chunk_bit_index / @bitSizeOf(T);
            const strided_index = unstrided_index + iteration * stride;

            if (opts.primeval == 0) { // NB this if is evaluated at comptime.
                comptime const mask = @as(T, 1) << shift;
                chunk[strided_index] |= mask;
            } else {
                comptime const mask = ~(@as(T, 1) << shift);
                chunk[strided_index] &= mask;
            }
        }
    };
}

test "the `naturalSquareBitIndex` results make sense" {
    // all odd squares are 1 or 9 (mod 16)
    try std.testing.expectEqual(SparseFnFactory(u8, 1, .{}).naturalSquareBitIndex(), 0);
    try std.testing.expectEqual(SparseFnFactory(u8, 3, .{}).naturalSquareBitIndex(), 4);
    try std.testing.expectEqual(SparseFnFactory(u8, 5, .{}).naturalSquareBitIndex(), 4);
    try std.testing.expectEqual(SparseFnFactory(u8, 7, .{}).naturalSquareBitIndex(), 0);
    // all odd squares are 1, 9, 25, or 17 (mod 32)
    try std.testing.expectEqual(SparseFnFactory(u16, 1, .{}).naturalSquareBitIndex(), 0);
    try std.testing.expectEqual(SparseFnFactory(u16, 3, .{}).naturalSquareBitIndex(), 4);
    try std.testing.expectEqual(SparseFnFactory(u16, 5, .{}).naturalSquareBitIndex(), 12);
    try std.testing.expectEqual(SparseFnFactory(u16, 7, .{}).naturalSquareBitIndex(), 8);
    try std.testing.expectEqual(SparseFnFactory(u16, 9, .{}).naturalSquareBitIndex(), 8);
    try std.testing.expectEqual(SparseFnFactory(u16, 11, .{}).naturalSquareBitIndex(), 12);
    try std.testing.expectEqual(SparseFnFactory(u16, 13, .{}).naturalSquareBitIndex(), 4);
    try std.testing.expectEqual(SparseFnFactory(u16, 15, .{}).naturalSquareBitIndex(), 0);
}

test "it is possible to fill a u8 memory segment with factors of 11" {
    const fillSpan = SparseFnFactory(u8, 3, .{}).fillOneChunk;
    var seg = try allocator.alloc(u8, 11);
    defer allocator.free(seg);
    clear(seg);

    fillSpan(@ptrCast([*]u8, seg), 0);

    const result = [_]u8{0b0001_0000, 0b1000_0000, 0b0000_0000, 0b0000_0100,
                         0b0010_0000, 0b0000_0000, 0b0000_0001, 0b0000_1000,
                         0b0100_0000, 0b0000_0000, 0b0000_0010};

    try std.testing.expectEqualSlices(u8, seg, result[0..]);
}

test "it is possible to fill a u8 memory segment with factors of 17" {
    const fillSpan = SparseFnFactory(u8, 1, .{}).fillOneChunk;
    var seg = try allocator.alloc(u8, 17);
    defer allocator.free(seg);
    clear(seg);

    fillSpan(@ptrCast([*]u8, seg), 1);

    const result = [_]u8{0b0000_0001, 0b0000_0000, 0b0000_0010, 0b0000_0000,
                         0b0000_0100, 0b0000_0000, 0b0000_1000, 0b0000_0000,
                         0b0001_0000, 0b0000_0000, 0b0010_0000, 0b0000_0000,
                         0b0100_0000, 0b0000_0000, 0b1000_0000, 0b0000_0000,
                         0b0000_0000};

    try std.testing.expectEqualSlices(u8, seg, result[0..]);
}

///////////////////////////////////////////////////////////////////////////////
// general stuff

fn ShiftTypeFor(comptime T: type) type {
    const t_size = @bitSizeOf(T);
    return @Type(
        std.builtin.TypeInfo{
            .Int = .{
                .signedness = .unsigned,
                .bits = std.math.log2(t_size)
            }
        }
    );
}

test "ShiftType analysis" {
    try std.testing.expectEqual(ShiftTypeFor(u8), u3);
    try std.testing.expectEqual(ShiftTypeFor(u16), u4);
    try std.testing.expectEqual(ShiftTypeFor(u32), u5);
    try std.testing.expectEqual(ShiftTypeFor(u64), u6);
}