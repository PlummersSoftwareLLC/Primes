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

// general options for the unrolled
pub const UnrolledOpts = struct {
    PRIME: u1 = 0,               // are 1s or 0's prime?
    max_vector: u32 = 1,         // should we use vectors?  (no == 1) what's the biggest vector size?
    half_extent: bool = true,    // how many lookup entries for the dense phase.
    unroll_sparse: bool = true,  // should we unroll sparse factors?
    SparseType: type = u8,       // what type should sparse unroll using?
    use_sparse_LUT: bool = true,
    use_dense_LUT: bool = true,
};

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

/// returns a function wrapped in an struct; this is the simplest variadic way
/// of assigning  a function "identifier" in zig to an integer value which
/// preserves the integer semantics of the funciton.
pub fn DenseFnFactory(comptime T: type, comptime num: usize, opts: UnrolledOpts) type {
    return struct{
        pub fn fill(field: [*]T, field_count: usize) void {
            const last_chunk = field + field_count + num;
            var chunk = field;
            fillOneChunk(chunk, true);
            chunk += num;
            while (@ptrToInt(chunk) < @ptrToInt(last_chunk)) : (chunk += num) {
                fillOneChunk(chunk, false);
            }
        }

        /// a function that is expected to fill *num* ints with bits flagged starting
        /// from index num/2, spaced out by *num* bits.
        inline fn fillOneChunk(chunk: [*]T, comptime start_at_square: bool) align(std.mem.page_size) void {
            comptime const total_vecs = if (opts.max_vector == 1) num else num / opts.max_vector + 1;
            comptime const total_ints = num;

            comptime var current_vec: usize = 0;
            comptime var current_int: usize = 0;
            comptime var current_bit: usize = num / 2;

            comptime var count: usize = 0; // for sanity checking purposes only.

            var cache_int: T = undefined; // this is register cache, size of one integer.

            inline while (current_vec < total_vecs) {
                const V = VectorFor(total_ints, current_vec, opts.max_vector);
                const Up = UnalignPtr(V);

                // assign the vector location in memory.
                var vector_ptr: Up = @ptrCast(Up, chunk + current_int);
                // pull the memory down into a vector register.
                var cache_vec: V = vector_ptr.*;

                vec_loop: inline while (true) {
                    // pull from the vector register to the int register
                    getCache(cache_vec, &cache_int, current_int);
                    inline while (current_bit / @bitSizeOf(T) <= current_int) : (current_bit += num) {

                        if ((!start_at_square) or ((2 * current_bit + 1) >= num * num)) {
                            setBit(&cache_int, current_bit);
                        }

                        count += 1;
                    }
                    // send the int register to the vector register
                    setCache(&cache_vec, cache_int, current_int);

                    current_int += 1;
                    comptime const index_int = current_int % opts.max_vector;
                    if ((index_int == 0) or (current_int == total_ints)) break :vec_loop;
                }
                // send the vector register back up to memory.
                vector_ptr.* = cache_vec;

                current_vec += 1;
            }

            // this should be true due to theoretical algebra, but let's add this
            // check in to ensure sanity.
            std.debug.assert(count == @bitSizeOf(T));
        }

        inline fn getCache(cache_vec: anytype, cache_int: *T, comptime current_int: usize) void {
            if (@TypeOf(cache_vec) == T) {
                cache_int.* = cache_vec;
            } else {
                cache_int.* = cache_vec[current_int % opts.max_vector];
            }
        }

        inline fn setCache(cache_vec: anytype, cache_int: T, comptime current_int: usize) void {
            if (@TypeOf(cache_vec) == *T) {
                cache_vec.* = cache_int;
            } else {
                cache_vec.*[current_int % opts.max_vector] = cache_int;
            }
        }

        inline fn setBit(cache_int: *T, comptime current_bit: usize) void {
            const shift_t = ShiftTypeFor(T);
            comptime const shift = @intCast(shift_t, current_bit % @bitSizeOf(T));
            if (opts.PRIME == 0) {
                comptime const mask = @as(T, 1) << shift;
                cache_int.* |= mask;
            } else {
                comptime const mask = ~(@as(T, 1) << shift);
                cache_int.* &= mask;
            }
        }

        // VECTOR/INT UTILITY FUNCTIONS
        fn VectorFor(comptime total_ints: usize, comptime current_vec: usize, comptime max_vector: u32) type {
            if (current_vec == total_ints / max_vector) {
                const last_size = total_ints % max_vector;
                return if (last_size == 1) T else std.meta.Vector(last_size, T);
            } else {
                return std.meta.Vector(max_vector, T);
            }
        }

        fn UnalignPtr(comptime V: type) type {
            return if (V == T) *T else *align(@alignOf(T))V;
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
    return fn([*]T, usize, usize) callconv(.Inline) void;
}

pub fn makeSparseLUT(comptime T: type, opts: UnrolledOpts) [lutCount(T, true)]SparseFn(T) {
    var myFuns: [lutCount(T, true)]SparseFn(T) = undefined;
    for (myFuns) | *f, index | {
        f.* = SparseFnFactory(T, 2 * index + 1, opts).fill;
    }
    return myFuns;
}

var my_factor: usize = undefined;
var my_index: usize = undefined;
var my_field: usize = undefined;

pub fn SparseFnFactory(comptime T: type, comptime progressive_shift: usize, opts: UnrolledOpts) type {
    return struct{
        pub inline fn fill(field: [*]T, field_ints: usize, factor: usize) void {
            const square_offset = (factor * factor) / (2 * @bitSizeOf(T));
            const stride = factor / @bitSizeOf(T) - 1;
            const field_end = field + field_ints;
            var chunk = field + square_offset;
            
            while (@ptrToInt(chunk) < @ptrToInt(field_end)) : (chunk += factor) {
                fillOneChunk(chunk, stride); // move the chunk pointer over.
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

            if (opts.PRIME == 0) { // NB this if is evaluated at comptime.
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