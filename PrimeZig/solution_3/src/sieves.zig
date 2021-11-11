const std = @import("std");
const default_allocator = @import("alloc.zig").default_allocator;
const unrolled = @import("unrolled.zig");
const wheel = @import("wheel.zig");

const IntSieveOpts = comptime struct {
    PRIME: anytype = @as(u8, 0),
    allocator: type = default_allocator,
    wheel_primes: u8 = 0,
    note: anytype = "",
};

pub fn IntSieve(comptime opts_: anytype) type {
    const opts: IntSieveOpts = opts_;

    const Wheel: ?type = if (opts.wheel_primes > 0)
        wheel.Wheel(.{ .num_primes = opts.wheel_primes, .PRIME = opts.PRIME })
    else
        null;

    const wheel_name = if (Wheel) |W| "-" ++ W.name else "";

    const PRIME = opts.PRIME;
    const COMPOSITE = if (@TypeOf(PRIME) == bool) !PRIME else 1 - PRIME;

    return struct {
        pub const T = @TypeOf(PRIME);
        pub const STARTING_FACTOR: usize = if (Wheel) |W| W.STARTING_FACTOR else 3;

        // informational content.
        pub const name = "sieve-" ++ @typeName(T) ++ wheel_name ++ opts.note;
        pub const algo = if (Wheel) |_| "wheel" else "base";
        pub const bits = if (T == bool) 8 else @bitSizeOf(T);

        // type helpers
        const Self = @This();
        usingnamespace opts.allocator;

        // storage
        field: [*]T,
        field_count: usize,

        // member functions
        pub fn init(sieve_size: usize) !Self {
            // allocates an array of data.  We only need half as many slots because
            // we are only going to operate on odd values.
            const field_count = sieve_size / 2;
            const field = try calloc_pages(PRIME, field_count);

            if (Wheel) |W| {
                W.roll(field, field_count);
            }

            return Self{
                .field = field,
                .field_count = field_count,
            };
        }

        pub fn deinit(self: *Self) void {
            free(self.field);
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 0;
            var idx: usize = 0;

            for (self.field[0..self.field_count]) |value| {
                if (value == PRIME) {
                    count += 1;
                }
            }

            return count;
        }

        pub fn findNextFactor(self: *Self, factor: usize) usize {
            const field = self.field;
            var slot = factor / 2 + 1;
            while (slot < self.field_count) : (slot += 1) {
                if (field[slot] == PRIME) {
                    return slot * 2 + 1;
                }
            }
            return slot * 2 + 1;
        }

        pub fn runFactor(self: *Self, factor: usize) void {
            const field = self.field;
            var slot = (factor * factor) / 2;
            while (slot < self.field_count) : (slot += factor) {
                field[slot] = COMPOSITE;
            }
        }

        // testing convenience
        pub fn dump(self: *Self) void {
            std.debug.print("\n{any}\n", .{self.field});
        }
    };
}

const FindFactorMode = enum { naive, advanced };
const UnrolledOpts = unrolled.UnrolledOpts;

const arch_word_t = switch (std.builtin.target.cpu.arch.ptrBitWidth()) {
    32 => u32,
    64 => u64,
    else => @compileError("architecture not supported")
};

const BitSieveOpts = comptime struct {
    PRIME: anytype = @as(u8, 0),
    allocator: type = default_allocator,
    wheel_primes: u8 = 0,
    RunFactorChunk: type = arch_word_t,
    cached_masks: bool = false, // used cached mask lookup instead?
    FindFactorChunk: type = u32,
    unrolled: ?UnrolledOpts = null,
    find_factor: FindFactorMode = .naive,
    note: anytype = "",
    foo_bar: bool = true,
    wheel_copy_vector: usize = 0,
};

pub fn BitSieve(comptime opts_: anytype) type {
    const opts: BitSieveOpts = opts_;
    const PRIME = opts.PRIME;
    const RunFactorChunk = opts.RunFactorChunk;
    const FindFactorChunk = opts.FindFactorChunk;

    const unrolled_opts : ?UnrolledOpts = if (opts.unrolled) |unrolled_tmp| uo: {
        comptime var adjusted_opts = unrolled_tmp;
        adjusted_opts.PRIME = PRIME;
        break :uo adjusted_opts;
    } else null;

    const Wheel: ?type = if (opts.wheel_primes > 0)
        wheel.Wheel(.{
            .num_primes = opts.wheel_primes,
            .PRIME = opts.PRIME,
            .bits = true,
            .copy_vector = opts.wheel_copy_vector,
        })
    else
        null;

    const base_name = (if (PRIME == 1) "inverted-" else "") ++ "bitSieve" ++ if (opts.unrolled) |_| "-unrolled" else "";
    const run_name = "-run-" ++ @typeName(RunFactorChunk);
    const find_name = "-find-" ++ @typeName(FindFactorChunk) ++ if (opts.find_factor == .advanced) "-advanced" else "";

    var vectornamebuf: [20]u8 = undefined; // 20 ought to be enough!
    var buf = if (unrolled_opts) |uo| buf: {
        if (uo.max_vector > 1) {
            const half_extent_suffix = if (uo.half_extent) "h" else "";
            const splut_suffix = if (uo.use_sparse_LUT) "-spLUT" else "";
            const delut_suffix = if (uo.use_dense_LUT) "-deLUT" else "";
            break :buf std.fmt.bufPrint(vectornamebuf[0..], "v{}{s}{s}{s}", .{uo.max_vector, half_extent_suffix, delut_suffix, splut_suffix})
                catch @compileError("can't make the vector name");
        }
    } else vectornamebuf[0..];

    const wheel_name = if (Wheel) |W| "-" ++ W.name else "";

    // static assertions that we are working with int types.
    _ = @typeInfo(RunFactorChunk).Int.bits;
    _ = @typeInfo(FindFactorChunk).Int.bits;

    return struct {
        // informational content.
        const vector_name = if (unrolled_opts) | uo | (
            if (uo.max_vector > 1) vectornamebuf[0..buf.len] else ""
        ) else "";

        pub const name = base_name ++ run_name ++ vector_name ++ find_name ++ wheel_name ++ opts.note;
        pub const algo = if (Wheel) |_| "wheel" else "base";
        pub const bits = 1;
        pub const STARTING_FACTOR: usize = if (Wheel) |W| W.STARTING_FACTOR else 3;

        // type helpers
        const Self = @This();
        usingnamespace opts.allocator;

        // stateful storage.  Use a naked bytes pointer for the field!  This will make it
        // easier to switch up the type for the findNextFactor and runFactor operations.
        field: [*]u8,
        field_count: usize = undefined,
        field_bytes: usize = undefined,

        const init_fill_unit: u8 = if (PRIME == 0) 0 else @as(u8, 0) -% @as(u8, 1);

        pub fn init(sieve_size: usize) !Self {
            // allocates an array of data.  We only need half as many slots because
            // we are only going to operate on odd values.  Moreover, the total number
            // of slots is divided by the number of bits in the value.
            const field_count = sieve_size / 2;
            const field_bytes = divRoundUp(field_count, 8);
            const trailing_masks = make_trailing_masks(u8, PRIME);

            // if we use loop-unrolling we will need extra padding because our prime
            // number function will overrun the end.  It's worth the speed!
            //const extra_padding = if (opts.unrolled) | _ |  else 0;

            // allocates the field slice.  Note that this will *always* allocate at least a page.
            var field = try calloc_pages(init_fill_unit, field_bytes + extraPadding(sieve_size));

            // roll out the wheel, if used.
            if (Wheel) |W| {
                W.roll(field, field_bytes);
            }

            // mask out the last bits of the field if they are needed.
            if (field_count % 8 != 0) {
                const padding = (field_count % 8);
                const last_slot = field_bytes - 1;
                if (PRIME == 1) {
                    field[last_slot] &= ~trailing_masks[padding];
                } else {
                    field[last_slot] |= ~trailing_masks[padding];
                }
            }

            return Self{
                .field = field,
                .field_count = field_count,
                .field_bytes = field_bytes,
            };
        }

        fn extraPadding(sieve_size: usize) usize {
            const ChunkType = if (opts.unrolled) | u | u.SparseType else return 0;
            const biggest_possible_factor = @floatToInt(usize, @sqrt(@intToFloat(f64, sieve_size)));
            return biggest_possible_factor * @bitSizeOf(ChunkType);
        }

        pub fn deinit(self: *Self) void {
            free(self.field);
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 0;
            var idx: usize = 0;

            for (self.field[0..self.field_bytes]) |value| {
                count += if (PRIME == 1) @popCount(u8, value) else 8 - @popCount(u8, value);
            }

            return count;
        }

        pub inline fn findNextFactor(self: *Self, factor: usize) usize {
            if (opts.find_factor == .naive) {
                return findNextFactorNaive(self, factor);
            } else {
                return findNextFactorAdvanced(self, factor);
            }
        }

        inline fn findNextFactorNaive(self: *Self, factor: usize) usize {
            // naive implementation
            const T = FindFactorChunk;
            const shift_t = ShiftTypeFor(T);
            const max_index = self.field_count;
            const field = @ptrCast([*]T, @alignCast(@alignOf(T), self.field));
            // start on the next odd number after the supplied factor.
            var search_index = factor / 2 + 1;
            while (search_index < max_index) : (search_index += 1) {
                const mask = @as(T, 1) << (@intCast(shift_t, search_index % @bitSizeOf(T)));
                const bit: T = (field[search_index / @bitSizeOf(T)] & mask);
                if (PRIME == 0) {
                    if (bit == 0) {
                        break;
                    }
                } else {
                    if (bit != 0) {
                        break;
                    }
                }
            }
            return search_index * 2 + 1;
        }

        inline fn findNextFactorAdvanced(self: *Self, factor: usize) usize {
            // advanced implementation
            const T = opts.FindFactorChunk;
            const max_word_index = self.field_count / @bitSizeOf(T);
            const field = @ptrCast([*]T, @alignCast(@alignOf(T), self.field))[0..max_word_index + 1];
            const trailing_masks = make_trailing_masks(T, PRIME);

            var search_index = factor / 2 + 1;
            var word_index = search_index / @bitSizeOf(T);
            if (PRIME == 1) {
                var slot = field[word_index] & trailing_masks[search_index % @bitSizeOf(T)];
                if (slot == 0) {
                    for (field[word_index + 1 ..]) |s| {
                        word_index += 1;
                        slot = s;
                        if (s != 0) {
                            break;
                        }
                    }
                }
                return (((word_index * @bitSizeOf(T)) + @ctz(T, slot)) << 1) + 1;
            } else {
                const max_T: T = @as(T, 0) -% 1;
                var slot = field[word_index] | trailing_masks[search_index % @bitSizeOf(T)];
                if (slot == max_T) {
                    for (field[word_index + 1 ..]) |s| {
                        word_index += 1;
                        slot = s;
                        if (s != max_T) {
                            break;
                        }
                    }
                }
                return (((word_index * @bitSizeOf(T)) + @ctz(T, ~slot)) << 1) + 1;
            }
        }

        pub fn runFactor(self: *Self, factor: usize) void {
            const T = opts.RunFactorChunk;
            if (opts.unrolled) |_| {
                runFactorUnrolled(self, factor);
            } else {
                runFactorConventional(self, factor);
            }
        }

        fn runFactorConventional(self: *Self, factor: usize) void {
            const T = opts.RunFactorChunk;
            const field = @ptrCast([*]T, @alignCast(@alignOf(T), self.field));
            // naive factoring algorithm.  calculate mask each time.
            const max_index = self.field_count;
            var multiple_index = (factor * factor) / 2;
            while (multiple_index < max_index) : (multiple_index += factor) {
                if (PRIME == 1) {
                    const mask = mask_for(T, multiple_index);
                    field[multiple_index / @bitSizeOf(T)] &= mask;
                } else {
                    const mask = mask_for(T, multiple_index);
                    field[multiple_index / @bitSizeOf(T)] |= mask;
                }
            }
        }

        inline fn runFactorUnrolled(self: *Self, factor: usize) void {
            const D = opts.RunFactorChunk; // Type for dense values
            if (unrolled.isDense(D, unrolled_opts.?.half_extent, factor)) {
                if (unrolled_opts.?.use_dense_LUT) {
                    runFactorDenseUnrolledLUT(D, self, factor);
                } else {
                    runFactorDenseUnrolled(D, self, factor);
                }
            } else {
                if (unrolled_opts.?.unroll_sparse) {
                    if (unrolled_opts.?.use_sparse_LUT) {
                        runFactorSparseUnrolledLUT(self, factor);
                    } else {
                        runFactorSparseUnrolled(self, factor);
                    }
                } else {
                    runFactorConventional(self, factor);
                }
            }
        }

        inline fn runFactorDenseUnrolled(comptime D: type, self: *Self, factor: usize) void {
            const field = @ptrCast([*]D, @alignCast(@alignOf(D), self.field));
            const fun_index = factor / 2;
            // select the function to use, using a compile-time unrolled loop over odd modulo values.
            comptime var fun_number = 0;
            comptime const fun_count = @bitSizeOf(D) / (if (unrolled_opts.?.half_extent) 2 else 1);
            inline while (fun_number < fun_count) : (fun_number += 1) {
                if (fun_index == fun_number) {
                    const DenseType = unrolled.DenseFnFactory(D, 2 * fun_number + 1, unrolled_opts.?);
                    DenseType.fill(field, self.field_bytes / @sizeOf(D));
                    return;
                }
            }
        }

        inline fn runFactorDenseUnrolledLUT(comptime D: type, self: *Self, factor: usize) void {
            const runfactorFns = comptime unrolled.makeDenseLUT(D, unrolled_opts.?);
            const fn_index = if (unrolled_opts.?.half_extent) (factor % @bitSizeOf(D)) / 2 else (factor / 2) % @bitSizeOf(D);
            const field = @ptrCast([*]D, @alignCast(@alignOf(D), self.field));
            runfactorFns[fn_index](field, self.field_bytes / @sizeOf(D));
        }

        inline fn runFactorSparseUnrolled(self: *Self, factor: usize) void {
            const S = unrolled_opts.?.SparseType; // Type for sparse values
            const field = @ptrCast([*]S, @alignCast(@alignOf(S), self.field));

            const fun_index = (factor % @bitSizeOf(S)) / 2;
            // select the function to use, using a compile-time unrolled loop over odd modulo values.
            comptime var fun_number = 0;
            inline while (fun_number < @bitSizeOf(S)) : (fun_number += 1) {
                if (fun_index == fun_number) {
                    const SparseType = unrolled.SparseFnFactory(S, 2 * fun_number + 1, unrolled_opts.?);
                    SparseType.fill(field, self.field_bytes / @sizeOf(S), factor);
                    return;
                }
            }
        }

        fn runFactorSparseUnrolledLUT(self: *Self, factor: usize) void {
            const S = unrolled_opts.?.SparseType; // Type for sparse values
            const runfactorFns = comptime unrolled.makeSparseLUT(S, unrolled_opts.?);
            const fn_index = (factor % @bitSizeOf(S)) / 2;
            const field = @ptrCast([*]S, @alignCast(@alignOf(S), self.field));
            runfactorFns[fn_index](field, self.field_bytes / @sizeOf(S), factor);
        }

        inline fn mask_for(comptime T: type, index: usize) T {
            if (opts.cached_masks) {
                // reduces the number of cycles per dispatch to 2 but whether it's worth it may be
                // architecture-dependent.
                comptime const bit_masks = make_bit_masks(T, PRIME);
                return bit_masks[index % @bitSizeOf(T)];
            } else {
                const shift_t = ShiftTypeFor(T);
                if (PRIME == 1) {
                    return ~(@as(T, 1) << (@intCast(shift_t, index % @bitSizeOf(T))));
                } else {
                    return @as(T, 1) << (@intCast(shift_t, index % @bitSizeOf(T)));
                }
            }
        }

        // testing convenience
        pub fn dump(self: *Self) void {
            std.debug.print("\n{b:0>8}\n", .{self.field[0..self.field_bytes]});
        }

        pub fn describe(self: *Self) void {
            var index: usize = 1;
            while (index < self.field_count) : (index += 1) {
                const byte = index / 8;
                const bit = @intCast(u3, index % 8);
                if ((self.field[byte] >> bit) & 0x01 == PRIME) {
                    std.debug.print("{} is prime\n", .{2 * index + 1});
                }
            }
        }
    };
}

const VecSieveOpts = struct {
    allocator: type = default_allocator,
    PRIME: u1 = 0x0,
};

pub fn VecSieve(comptime opts_: anytype) type {
    const opts: VecSieveOpts = opts_;

    return struct {
        // values
        const Self = @This();
        const PRIME = opts.PRIME;

        // storage
        field: []align(8) u8,
        field_count: usize,

        usingnamespace opts.allocator;

        const PatternChunk = std.meta.Vector(64, u8);
        const small_factor_max = @sizeOf(PatternChunk) * 8;

        const bit_masks = blk: {
            var masks: [8]u8 = undefined;
            for (masks) |*value, index| {
                const bit = @as(u8, 1) << index;
                value.* = if (PRIME == 1) ~bit else bit;
            }
            break :blk masks;
        };

        const trailing_masks = blk: {
            var masks: [8]u8 = undefined;
            for (masks) |*value, index| {
                const bit = @as(u8, 1) << index;
                value.* = if (PRIME == 1) (0 -% bit) else bit - 1;
            }
            break :blk masks;
        };

        // member functions
        pub fn init(field_count: usize) !Self {
            const bit_size = field_count / 2;  // REUSE ME?
            const field_count_bytes = (bit_size + @bitSizeOf(u8) - 1) / @bitSizeOf(u8);

            // allocates an array of data.
            // Heisenberg compensator:  Overallocate so we can do unchecked writes at the end.
            const compensated_length = field_count_bytes + @sizeOf(PatternChunk) - 1;
            const field_raw = @alignCast(8, try calloc_pages(PRIME, compensated_length));
            const field = field_raw[0..compensated_length];

            if (PRIME == 1) {
                @memset(field.ptr, 0xFF, field.len);
            }

            const num_bits = field_count / 2;
            const final_index = num_bits / 8;
            const final_bits = num_bits % 8;

            // there might be some stray bits at the end.
            // we need to mask those bits off.
            const final_mask = (@as(u8, 1) << @intCast(u3, final_bits)) - 1;

            if (PRIME == 1) {
                field[final_index] &= final_mask;
            } else {
                field[final_index] |= ~final_mask;
            }

            for (field[final_index + 1 ..]) |*v| v.* = if (PRIME == 1) 0x00 else 0xFF;

            return Self{ .field = field, .field_count = field_count };
        }

        pub fn deinit(self: *Self) void {
            free(@ptrCast([*]u8, self.field.ptr));
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 0;
            var idx: usize = 0;

            for (self.field) |value| {
                count += if (PRIME == 1) @popCount(u8, value) else 8 - @popCount(u8, value);
            }

            return count;
        }

        pub fn findNextFactor(self: *Self, factor: usize) usize {
            const field = self.field;
            const num = (factor + 2) / 2;
            var index = num / 8;
            if (PRIME == 1) {
                var slot = field[index] & trailing_masks[num % 8];
                if (slot == 0) {
                    for (field[index + 1 ..]) |s| {
                        index += 1;
                        slot = s;
                        if (s != 0) {
                            break;
                        }
                    }
                }
                return (((index * 8) + @ctz(u8, slot)) << 1) + 1;
            } else {
                var slot = field[index] | trailing_masks[num % 8];
                if (slot == 0xFF) {
                    for (field[index + 1 ..]) |s| {
                        index += 1;
                        slot = s;
                        if (s != 0xFF) {
                            break;
                        }
                    }
                }
                return (((index * 8) + @ctz(u8, ~slot)) << 1) + 1;
            }
        }

        pub inline fn runFactor(self: *Self, factor: usize) void {
            if (factor < small_factor_max) {
                runSmallFactor(self, @intCast(u32, factor));
            } else {
                runSparseFactor(self, factor);
            }
        }

        inline fn runSparseFactor(self: *Self, factor: usize) void {
            const field = self.field;
            const limit = self.field_count / 2;
            var num = (factor * factor) / 2;
            while (num < limit) : (num += factor) {
                if (PRIME == 1) {
                    field[num / 8] &= bit_masks[num % 8];
                } else {
                    field[num / 8] |= bit_masks[num % 8];
                }
            }
        }

        inline fn runSmallFactor(self: *Self, factor: u32) void {
            std.debug.assert(factor < small_factor_max);
            const field = self.field;
            const limit = self.field_count >> 1;

            // Fill in a pattern buffer
            var pattern_raw: [small_factor_max + @sizeOf(PatternChunk) - 1]u8 = undefined;
            const pattern = pattern_raw[0 .. factor + @sizeOf(PatternChunk) - 1];

            @memset(pattern.ptr, (if (PRIME == 1) 0xFF else 0x00), pattern.len);

            var num: u32 = 0;
            while (num < pattern.len * @bitSizeOf(u8)) : (num += factor) {
                if (PRIME == 1) {
                    pattern[num / 8] &= bit_masks[num % 8];
                } else {
                    pattern[num / 8] &= bit_masks[num % 8];
                }
            }

            // Fill normally until we start a byte
            // This loop will execute at most 7 times
            num = (factor * factor) >> 1;
            while (num % @bitSizeOf(u8) != 0) : (num += factor) {
                if (PRIME == 1) {
                    field[num / 8] &= bit_masks[num % 8];
                } else {
                    field[num / 8] |= bit_masks[num % 8];
                }
            }

            // Apply pattern across the field
            const byte_index = num / @bitSizeOf(u8);
            const bulk_ptr = @ptrCast([*]align(1) PatternChunk, field.ptr + byte_index);
            const bulk_len = (field.len - byte_index) / @sizeOf(PatternChunk);
            const fetch_increment = @sizeOf(PatternChunk) % factor;
            var fetch_index: usize = 0;

            for (bulk_ptr[0..bulk_len]) |*item, i| {
                // phase transition:  we insist that the PatternChunk vector pointer has
                // bytewise alignment which will cause it to engage the correct unaligned
                // SSE operations.
                if (PRIME == 1) {
                    item.* &= @ptrCast(*align(1) PatternChunk, &pattern[fetch_index]).*;
                } else {
                    item.* |= @ptrCast(*align(1) PatternChunk, &pattern[fetch_index]).*;
                }

                fetch_index += fetch_increment;
                if (fetch_index >= factor) fetch_index -= factor;
            }
        }

        pub const name = "vecSieve";
        pub const STARTING_FACTOR: usize = 3;
        pub const algo = "other";
        pub const bits = 1;

        // testing convenience
        pub fn dump(self: *Self) void {
            std.debug.print("\n{b:0>8}\n", .{self.field});
        }
    };
}

// common tools
fn divRoundUp(number: anytype, divisor: anytype) @TypeOf(number) {
    const T = @TypeOf(number);
    std.debug.assert(std.math.isPowerOfTwo(divisor));
    return number / divisor + if (number % divisor == 0) @as(T, 0) else @as(T, 1);
}

test "divRoundUp" {
    try std.testing.expectEqual(divRoundUp(3, 8), 1);
    try std.testing.expectEqual(divRoundUp(8, 8), 1);
    try std.testing.expectEqual(divRoundUp(17, 16), 2);
}

fn ShiftTypeFor(comptime T: type) type {
    const t_size = @bitSizeOf(T);
    return @Type(std.builtin.TypeInfo{ .Int = .{ .signedness = .unsigned, .bits = std.math.log2(t_size) } });
}

test "ShiftType analysis" {
    try std.testing.expectEqual(ShiftTypeFor(u8), u3);
    try std.testing.expectEqual(ShiftTypeFor(u16), u4);
    try std.testing.expectEqual(ShiftTypeFor(u32), u5);
    try std.testing.expectEqual(ShiftTypeFor(u64), u6);
}

// creates a compile-time lookup table for masks which will blot out values
// before a given position in a single byte.
fn make_trailing_masks(comptime T: type, prime: u1) comptime [@bitSizeOf(T)]T {
    const t_size = @bitSizeOf(T);
    const shift_t = ShiftTypeFor(T);
    var masks = std.mem.zeroes([t_size]T);
    for (masks) |*value, index| {
        value.* = @as(T, 0) -% (@as(T, 1) << @intCast(shift_t, index));
        if (prime == 0) {
            value.* = ~value.*;
        }
    }
    return masks;
}

test "trailing mask generation" {
    try std.testing.expectEqual(make_trailing_masks(u8, 0), [_]u8{ 0x00, 0x01, 0x03, 0x07, 0x0F, 0x1F, 0x3F, 0x7F });

    try std.testing.expectEqual(make_trailing_masks(u8, 1), [_]u8{ 0xFF, 0xFE, 0xFC, 0xF8, 0xF0, 0xE0, 0xC0, 0x80 });

    try std.testing.expectEqual(make_trailing_masks(u16, 0), [_]u16{ 0x0000, 0x0001, 0x0003, 0x0007, 0x000F, 0x001F, 0x003F, 0x007F, 0x00FF, 0x01FF, 0x03FF, 0x07FF, 0x0FFF, 0x1FFF, 0x3FFF, 0x7FFF });

    try std.testing.expectEqual(make_trailing_masks(u16, 1), [_]u16{ 0xFFFF, 0xFFFE, 0xFFFC, 0xFFF8, 0xFFF0, 0xFFE0, 0xFFC0, 0xFF80, 0xFF00, 0xFE00, 0xFC00, 0xF800, 0xF000, 0xE000, 0xC000, 0x8000 });
}

// creates a compile-time lookup table for bit masks for each
// slot.  There should be one mask for each bit-positional index
// e.g. for u8, there are 8 u8 masks; for u64, there are 64 u64 masks.
fn make_bit_masks(comptime T: type, prime: u1) comptime [@bitSizeOf(T)]T {
    const shift_t = ShiftTypeFor(T);
    var masks = std.mem.zeroes([@bitSizeOf(T)]T);
    for (masks) |*value, bit_index| {
        value.* = (@as(T, 1) << @intCast(shift_t, bit_index));
        if (prime == 1) {
            value.* ^= (@as(T, 0) -% @as(T, 1));
        }
    }
    return masks;
}

test "bit mask generation" {
    try std.testing.expectEqual(make_bit_masks(u8, 0), [_]u8{ 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80 });

    try std.testing.expectEqual(make_bit_masks(u8, 1), [_]u8{ 0xFE, 0xFD, 0xFB, 0xF7, 0xEF, 0xDF, 0xBF, 0x7F });

    try std.testing.expectEqual(make_bit_masks(u16, 0), [_]u16{ 0x0001, 0x0002, 0x0004, 0x0008, 0x0010, 0x0020, 0x0040, 0x0080, 0x0100, 0x0200, 0x0400, 0x0800, 0x1000, 0x2000, 0x4000, 0x8000 });

    try std.testing.expectEqual(make_bit_masks(u16, 1), [_]u16{ 0xFFFE, 0xFFFD, 0xFFFB, 0xFFF7, 0xFFEF, 0xFFDF, 0xFFBF, 0xFF7F, 0xFEFF, 0xFDFF, 0xFBFF, 0xF7FF, 0xEFFF, 0xDFFF, 0xBFFF, 0x7FFF });
}
