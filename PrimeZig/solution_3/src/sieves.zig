const std = @import("std");
const Allocator = std.mem.Allocator;
const WheelFn = @import("wheel.zig").Wheel;

const SieveOpts = struct { pregen: ?comptime_int = null, bulksearch: bool = false };

pub fn IntSieve(comptime T: type, opts: SieveOpts) type {
    // store the wheel type
    const Wheel: ?type = if (opts.pregen) | pregen | WheelFn(pregen, .byte) else null;
    const bulksearch = opts.bulksearch;

    return struct {
        pub const TRUE = if (T == bool) true else @as(T, 1);
        pub const FALSE = if (T == bool) false else @as(T, 0);

        const Self = @This();

        // storage
        field: []T,
        allocator: *Allocator,

        // member functions

        pub fn init(allocator: *Allocator, sieve_size: usize) !Self {
            // allocates an array of data.
            const field_size = sieve_size >> 1;
            var field = try allocator.alloc(T, pad(field_size));
            return Self{ .field = field[0..field_size], .allocator = allocator };
        }

        // adds a little bit of extra padding onto the allocation, to make safer fast-searches.
        // 4-byte alignment of slice on 32-bit arches, 8-byte on 64-bit arches.
        const arch_width = std.builtin.target.cpu.arch.ptrBitWidth();
        const pad_factor = arch_width >> 3;
        const pad_mask: usize = pad_factor - 1;

        inline fn pad(field_size: usize) usize {
            const rem = (field_size & pad_mask);
            return if (rem != 0) field_size + rem else field_size;
        }

        pub fn deinit(self: *Self) void {
            self.allocator.free(self.field);
        }

        pub fn reset(self: *Self) usize {
            if (Wheel) |W| {
                var index: usize = 0;

                for (self.field) |*slot| {
                    slot.* = W.template[index];
                    index += 1;
                    if (index == W.template.len) {
                        index = 0;
                    }
                }

                inline for (W.primes) |prime| {
                    const prime_index = prime >> 1;
                    if (prime_index < self.field.len) {
                        self.field[prime_index] = 1;
                    }
                }

                return W.first_prime;
            } else {
                fill_memory(self.field);
                return 3;
            }
        }

        fn fill_memory(field: []T) void {
            if (T == u8) {
                @memset(@ptrCast([*]u8, field), TRUE, field.len);
            } else {
                for (field) |*elem| {
                    elem.* = TRUE;
                }
            }
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 0;
            var idx: usize = 0;

            for (self.field) |value| {
                if (T == bool) {
                    count += @boolToInt(value);
                } else {
                    count += value;
                }
            }

            return count;
        }

        // uses a higher-order integer type (32- or 64-bit) to speed up searches
        const search_t = switch (arch_width) {
            32 => u32,
            64 => u64,
            else => unreachable
        };

        const bit_shift = switch(arch_width) {
            32 => 2,
            64 => 3,
            else => unreachable
        };

        pub fn findNextFactor(self: *Self, factor: usize) usize {
            if (bulksearch) {
                // uses a "bulk search" mechanism that is similar to what bitsieve uses.
                comptime const masks = trailing_masks();
                const limit = pad(self.field.len) >> bit_shift;
                const field = @ptrCast([*] const search_t, @alignCast(pad_factor, self.field.ptr))[0..limit];  // danger, will robinson!!
                const num = (factor + 2) >> 1;
                var index = num >> bit_shift;

                var slot = field[index] & masks[num & pad_mask];

                if (slot == 0) {
                    for (field[index + 1 ..]) |s| {
                        index += 1;
                        slot = s;
                        if (s != 0) {
                            break;
                        }
                    }
                }

                return (((index << bit_shift) + (@ctz(search_t, slot) >> 3)) * 2) + 1;
            } else {
                const field = self.field;
                var num = factor + 2;
                while (num < self.field.len) : (num += 2) {
                    if (T == bool) {
                        if (field[num >> 1]) {
                            return num;
                        }
                    } else {
                        if (field[num >> 1] == TRUE) {
                            return num;
                        }
                    }
                }
                return num;
            }
        }

        pub fn runFactor(self: *Self, factor: usize) void {
            const field = self.field;
            var num = (factor * factor) >> 1;
            while (num < self.field.len) : (num += factor) {
                field[num] = FALSE;
            }
        }

        const shift_t = switch (arch_width) {
            32 => u2,
            64 => u3,
            else => unreachable
        };
        const mask_count = arch_width >> 3;
        const filled_byte:u8 = 0xFF;
        fn trailing_masks() comptime [mask_count]search_t {
            @setEvalBranchQuota(10000);
            var masks = std.mem.zeroes([mask_count]search_t);
            for (masks) |*mask, index| {
                var as_u8 = @ptrCast(*[pad_factor]u8, mask);
                for (as_u8.*) |*foo, u8_index| {
                    if (u8_index >= index) {
                        foo.* = 0xFF;
                    }
                }
            }
            return masks;
        }

        pub const wheel_name = if (Wheel) |W| ("-" ++ W.name) else "";
        pub const name = "sieve-" ++ @typeName(T) ++ wheel_name;
    };
}

pub fn BitSieve(comptime T: type, opts: SieveOpts) type {
    // store the wheel type
    const Wheel: ?type = if (opts.pregen) | pregen | WheelFn(pregen, .bit) else null;

    return struct {
        // values
        const Self = @This();
        const bit_width = @bitSizeOf(T);
        const one: T = 1;
        const bit_shift = switch (T) {
            u64 => 6,
            u32 => 5,
            u16 => 4,
            u8 => 3,
            else => unreachable,
        };
        const smallint_t = std.meta.Int(.unsigned, bit_shift);
        const bit_mask: T = (1 << bit_shift) - 1;

        // storage
        field: []T,
        allocator: *Allocator,
        field_size: usize,
        needs_pad: bool,

        // member functions
        pub fn init(allocator: *Allocator, field_size: usize) !Self {
            const needs_pad: bool = ((field_size >> 1) & bit_mask) != 0;
            const padding: usize = (if (needs_pad) 1 else 0);
            const field_units: usize = (field_size >> (bit_shift + 1)) + padding;

            // allocates an array of data.
            var field = try allocator.alloc(T, field_units);
            return Self{ .field = field[0..], .allocator = allocator, .field_size = field_size, .needs_pad = needs_pad };
        }

        pub fn deinit(self: *Self) void {
            self.allocator.free(self.field);
        }

        pub fn reset(self: *Self) usize {
            const finalmask: T = (one << @intCast(smallint_t, (self.field_size >> 1) & bit_mask)) - 1;

            var starting_point: usize = 0;

            if (Wheel) |W| {
                var index: usize = 0;

                copyBits(self.field, &W.template);

                // make the primes prime
                inline for (W.primes) |prime| {
                    const mask = (@as(T, 1) << bit_shift) - @as(T, 1);
                    const prime_index = prime >> 1;
                    const byte_index = prime_index >> bit_shift;
                    const bit_index = prime_index & mask;
                    const bit_flip = @as(T, 1) << bit_index;

                    self.field[byte_index] |= bit_flip;
                }

                starting_point = W.first_prime;
            } else {
                fill_memory(self.field);
                starting_point = 3;
            }

            if (self.needs_pad) {
                // point of finalmask is that there might be some stray bits at the end.
                // we need to mask those bits off.
                self.field[self.field.len - 1] &= finalmask;
            }
            return starting_point;
        }

        fn fill_memory(field: []T) void {
            @memset(@ptrCast([*]u8, field.ptr), 0xFF, @sizeOf(T) * field.len);
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 0;
            var idx: usize = 0;


            for (self.field) |value| {
                count += @popCount(T, value);
            }

            return count;
        }

        const src_units = if (Wheel) |W| W.template.len else 1;
        // TODO: move this to Wheel?

        fn copyBits(bit_field: []T, src: *const *[src_units]u8) void {
            const field_len = bit_field.len;
            if (T == u8) {
                const dest_len = field_len * @sizeOf(T);
                var start: usize = 0;
                var finish: usize = src_units;
                while (finish < dest_len) : ({
                    start += src_units;
                    finish += src_units;
                }) {
                    copy(bit_field[start..finish], src.*[0..]);
                }
                copy(bit_field[start..field_len], src.*[0 .. field_len - start]);
            } else {
                var src_index: usize = 0;
                for (bit_field) |*dest| {
                    dest.* = findBits(src, &src_index);
                }
            }
        }

        fn copy(dst: []u8, src: []const u8) void {
            std.debug.assert(dst.len == src.len);
            std.mem.copy(u8, dst, src);
        }

        inline fn findBits(src: *const *[src_units]u8, src_index: *usize) T {
            var buf: [@sizeOf(T)]u8 = undefined;
            var next_index = src_index.* + @sizeOf(T);
            if (next_index < src_units) {
                std.mem.copy(u8, &buf, src.*[(src_index.*)..next_index]);
                src_index.* = next_index;
            } else {
                copyWrapping(&buf, src, src_index);
            }

            var result = @bitCast(T, buf);

            comptime const should_swap = comptime blk: {
                break :blk (std.builtin.cpu.arch.endian() == .Big);
            };

            return if (should_swap) @byteSwap(T, result) else result;
        }

        inline fn copyWrapping(buf: []u8, src: *const *[src_units]u8, src_index: *usize) void {
            var buf_index: usize = 0;
            while (src_index.* < src_units) : (src_index.* += 1) {
                buf[buf_index] = src.*[src_index.*];
                buf_index += 1;
            }
            src_index.* = 0;
            while (buf_index < @sizeOf(T)) : (buf_index += 1) {
                buf[buf_index] = src.*[src_index.*];
                src_index.* += 1;
            }
        }

        // a mask that is usable to obtain the residual (remainder) from the
        // bitshift operation.  This is the bit position within the datastructure
        // that represents the primeness of the requested number.
        const residue_mask = (1 << bit_shift) - 1;

        pub fn findNextFactor(self: *Self, factor: usize) usize {
            comptime const masks = trailing_masks();
            const field = self.field;
            const num = (factor + 2) >> 1;
            var index = num >> bit_shift;
            var slot = field[index] & masks[num & residue_mask];
            if (slot == 0) {
                for (field[index + 1 ..]) |s| {
                    index += 1;
                    slot = s;
                    if (s != 0) {
                        break;
                    }
                }
            }
            return (((index << bit_shift) + @ctz(T, slot)) << 1) + 1;
        }

        pub fn runFactor(self: *Self, factor: usize) void {
            comptime const masks = bit_masks();
            const field = self.field;
            const limit = self.field_size >> 1;
            var num = (factor * factor) >> 1;
            while (num < limit) : (num += factor) {
                var index = num >> bit_shift;
                field[index] &= masks[num & residue_mask];
            }
        }

        const shift_t = switch (T) {
            u8 => u3,
            u16 => u4,
            u32 => u5,
            u64 => u6,
            else => unreachable,
        };

        fn trailing_masks() comptime [bit_width]T {
            var masks = std.mem.zeroes([bit_width]T);
            for (masks) |*value, index| {
                value.* = @as(T, 0) -% (@as(T, 1) << @intCast(shift_t, index));
            }
            return masks;
        }

        fn bit_masks() comptime [bit_width]T {
            var masks = std.mem.zeroes([bit_width]T);
            for (masks) |*value, index| {
                value.* = (@as(T, 1) << @intCast(shift_t, index));
                value.* ^= (@as(T, 0) -% @as(T, 1));
            }
            return masks;
        }

        pub const wheel_name = if (Wheel) |W| ("-" ++ W.name) else "";
        pub const name = "bitSieve-" ++ @typeName(T) ++ wheel_name;
    };
}

pub const FastSieve = struct {
    // values
    const Self = @This();

    // storage
    field: []align(8) u8,
    field_size: usize,
    allocator: *Allocator,

    const PatternChunk = std.meta.Vector(64, u8);
    const small_factor_max = @sizeOf(PatternChunk) * 8;

    const bit_masks = blk: {
        var masks: [8]u8 = undefined;
        for (masks) |*value, index| {
            value.* = ~(@as(u8, 1) << index);
        }
        break :blk masks;
    };

    const trailing_masks = blk: {
        var masks: [8]u8 = undefined;
        for (masks) |*value, index| {
            value.* = -%(@as(u8, 1) << index);
        }
        break :blk masks;
    };

    // member functions
    pub fn init(allocator: *Allocator, field_size: usize) !Self {
        const bit_size = field_size / 2;
        const field_size_bytes = (bit_size + @bitSizeOf(u8) - 1) / @bitSizeOf(u8);

        // allocates an array of data.
        // Overallocate so we can do unchecked writes at the end.
        const field = try allocator.alignedAlloc(u8, 8, field_size_bytes + @sizeOf(PatternChunk) - 1);
        return Self{ .field = field, .allocator = allocator, .field_size = field_size };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.field);
    }

    pub fn reset(self: *Self) usize {
        const field = self.field;
        @memset(field.ptr, 0xFF, field.len);
        const num_bits = self.field_size >> 1;
        const final_index = num_bits / 8;
        const final_bits = num_bits % 8;

        // there might be some stray bits at the end.
        // we need to mask those bits off.
        const final_mask = (@as(u8, 1) << @intCast(u3, final_bits)) - 1;
        field[final_index] &= final_mask;
        for (field[final_index+1..]) |*v| v.* = 0;
        return 3;
    }

    pub fn primeCount(self: *Self) usize {
        var count: usize = 0;
        var idx: usize = 0;

        for (self.field) |value| {
            count += @popCount(u8, value);
        }

        return count;
    }

    pub inline fn findNextFactor(self: *Self, factor: usize) usize {
        const field = self.field;
        const num = (factor + 2) >> 1;
        var index = num / 8;
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
    }

    pub fn runFactor(self: *Self, factor: usize) void {
        if (factor < small_factor_max) {
            runSmallFactor(self, @intCast(u32, factor));
        } else {
            runSparseFactor(self, factor);
        }
    }

    pub inline fn runSparseFactor(self: *Self, factor: usize) void {
        const field = self.field;
        const limit = self.field_size >> 1;
        var num = (factor * factor) >> 1;
        while (num < limit) : (num += factor) {
            field[num / 8] &= bit_masks[num % 8];
        }
    }

    pub inline fn runSmallFactor(self: *Self, factor: u32) void {
        std.debug.assert(factor < small_factor_max);
        const field = self.field;
        const limit = self.field_size >> 1;

        // Fill in a pattern buffer
        var pattern_raw: [small_factor_max + @sizeOf(PatternChunk)-1]u8 = undefined;
        const pattern = pattern_raw[0..factor + @sizeOf(PatternChunk)-1];
        @memset(pattern.ptr, 0xFF, pattern.len);
        var num: u32 = 0;
        while (num < pattern.len * @bitSizeOf(u8)) : (num += factor) {
            pattern[num / 8] &= bit_masks[num % 8];
        }

        // Fill normally until we start a byte
        // This loop will execute at most 7 times
        num = (factor * factor) >> 1;
        while (num % @bitSizeOf(u8) != 0) : (num += factor) {
            field[num / 8] &= bit_masks[num % 8];
        }

        // Apply pattern in bulk
        const byte_index = num / @bitSizeOf(u8);
        const bulk_ptr = @ptrCast([*]align(1) PatternChunk, field.ptr + byte_index);
        const bulk_len = (field.len - byte_index) / @sizeOf(PatternChunk);
        const fetch_increment = @sizeOf(PatternChunk) % factor;
        var fetch_index: usize = 0;
        for (bulk_ptr[0..bulk_len]) |*item, i| {
            item.* &= @ptrCast(*align(1) PatternChunk, &pattern[fetch_index]).*;
            fetch_index += fetch_increment;
            if (fetch_index >= factor) fetch_index -= factor;
        }
    }

    pub const name = "fastSieve";
};


fn printBits(bytes: []const u8, start: usize, end: usize) void {
    var c = start;
    while (c < end) : (c += 1) {
        var bit = @as(u8, 1) << @truncate(u3, c);
        var byte = bytes[c / 8];
        const str = if (bit & byte == 0) "0" else "1";
        if (c % 8 == 0 and c != start) {
            std.debug.print("_", .{});
        }
        std.debug.print("{s}", .{str});
    }
    std.debug.print("\n", .{});
}

test {
    const print = std.debug.print;
    print("\n", .{});
    defer print("\n", .{});

    const Sieve = BitSieve(u8, .{});
    var gold_sieve = try Sieve.init(std.testing.allocator, 1_000_000);
    defer gold_sieve.deinit();
    var test_sieve = try FastSieve.init(std.testing.allocator, 1_000_000);
    defer test_sieve.deinit();
    var prime = gold_sieve.reset();
    try std.testing.expectEqual(prime, test_sieve.reset());
    while (true) {
        gold_sieve.runFactor(prime);
        test_sieve.runFactor(prime);
        for (test_sieve.field[gold_sieve.field.len..]) |v| {
            try std.testing.expectEqual(@as(u8, 0), v);
        }
        std.testing.expectEqualSlices(u8, gold_sieve.field, test_sieve.field[0..gold_sieve.field.len]) catch |err| {
            const mismatch_index = for (gold_sieve.field) |v, i| {
                if (v != test_sieve.field[i]) break i;
            } else unreachable;
            const print_start = if (mismatch_index < 8) 0 else mismatch_index - 8;
            const gold_print_end = if (mismatch_index + 8 >= gold_sieve.field.len) gold_sieve.field.len else mismatch_index + 8;
            const test_print_end = if (mismatch_index + 8 >= gold_sieve.field.len) test_sieve.field.len else mismatch_index + 8;
            print("@{}:{} prime={}\n", .{print_start / 8, print_start % 8, prime});
            printBits(gold_sieve.field, print_start * 8, gold_print_end * 8);
            printBits(test_sieve.field, print_start * 8, test_print_end * 8);
            return err;
        };
        const num_primes = gold_sieve.primeCount();
        try std.testing.expectEqual(num_primes, test_sieve.primeCount());
        const new_prime = gold_sieve.findNextFactor(prime);
        if (new_prime >= 1_000_000) break;
        try std.testing.expectEqual(new_prime, test_sieve.findNextFactor(prime));
        prime = new_prime;
    }
}
