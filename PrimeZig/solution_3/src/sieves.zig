const std = @import("std");
const default_allocator = @import("alloc.zig").default_allocator;

const IntSieveOpts = comptime struct {
    T: type = u8,
    primeval: anytype = 0,
    wheel: bool = false,
    allocator: type = default_allocator,
};

pub fn IntSieve(comptime opts: IntSieveOpts) type {
    // only allow 1-byte datatypes.
    std.debug.assert(@sizeOf(opts.T) == 1);
    const wheel_name = "";
    return struct {
        pub const T = opts.T;
        pub const PRIME: T = opts.primeval;
        pub const COMPOSITE: T = if (T == bool) (!opts.primeval) else (if (opts.primeval == 0) 1 else 0);

        // informational content.
        pub const name = "sieve-" ++ @typeName(T) ++ wheel_name;
        pub const algo = "base";
        pub const bits = if (T == bool) 8 else @bitSizeOf(T);

        // type helpers
        const Self = @This();
        usingnamespace opts.allocator;

        // storage
        field: [*]u8,

        // member functions

        pub fn init(sieve_size: usize) !Self {
            // allocates an array of data.  We only need half as many slots because
            // we are only going to operate on odd values.
            const field_size = sieve_size / 2;
            var field = try calloc_page(T, PRIME, field_size, std.mem.page_size);
            return Self{ .field = field[0..field_size] };
        }

        pub fn deinit(self: *Self) void {
            free(self.field);
        }

        pub fn reset(self: *Self) usize {
            return 3;
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 0;
            var idx: usize = 0;

            for (self.field) |value| {
                if (value == PRIME) { count += 1; }
            }

            return count;
        }

        pub fn findNextFactor(self: *Self, factor: usize) usize {
            const field = self.field;
            var slot = factor / 2 + 1;
            while (slot < self.field.len) : (slot += 1) {
                if (field[slot] == PRIME) {
                    return slot * 2 + 1;
                }
            }
            return slot * 2 + 1;
        }

        pub fn runFactor(self: *Self, factor: usize) void {
            const field = self.field;
            var slot = (factor * factor) / 2;
            while (slot < self.field.len) : (slot += factor) {
                field[slot] = COMPOSITE;
            }
        }

        // testing convenience
        pub fn dump(self: *Self) void {
            std.debug.print("\n{any}\n", .{self.field});
        }
    };
}

const BitSieveOpts = comptime struct {
    primeval: anytype = 0,
    wheel: bool = false,
    allocator: type = default_allocator,
};

pub fn BitSieve(comptime opts: BitSieveOpts) type {
    // std.debug.assert(@typeInfo(opts.T).Int.signedness == .unsigned);
    // std.debug.assert(std.math.isPowerOfTwo(@typeInfo(opts.T).Int.bits));
    // std.debug.assert(@typeInfo(opts.T).Int.bits >= 8);

    comptime const PRIME: u1 = opts.primeval;
    // store the wheel type
    const wheel_name = "";
    return struct {
        // informational content.
        pub const name = "bitSieve-" ++ wheel_name;
        pub const algo = "base";
        pub const bits = 1;

        // type helpers
        const Self = @This();
        usingnamespace opts.allocator;

        // stateful storage.  Use a naked bytes pointer for the field!  This will make it
        // easier to switch up the type for the findNextFactor and runFactor operations.
        field: [*]u8,
        field_size: usize = undefined,
        needs_pad: bool = undefined,

        const init_fill_unit: u8 = if (PRIME == 0) 0 else @as(u8, 0) -% @as(u8, 1);

        pub fn init(sieve_size: usize) !Self {
            // allocates an array of data.  We only need half as many slots because
            // we are only going to operate on odd values.  Moreover, the total number
            // of slots is divided by the number of bits in the value.
            const field_size = sieve_size / 2;
            const needs_pad = (field_size % 8) != 0;
            const field_len = field_size / 8 + if (needs_pad) @as(usize, 1) else @as(usize, 0);

            // allocates the field slice.  Note that this will *always* allocate at least a page.
            var field = try alloc_page(T, init_fill_unit, field_len);
            return Self{ .field = field, .field_size = field_size, .needs_pad = needs_pad };
        }

        pub fn reset(self: *Self) usize {
            comptime const trailing_masks = make_trailing_masks();
            // perform wheel operations if needed.
            // mask out the last bits of the field if they are needed.
            if (self.needs_pad) {
                const padding = (self.field_size % @bitSizeOf(T));
                const last_slot = self.field.len - 1;
                if (PRIME == 1) {
                    self.field[last_slot] &= ~trailing_masks[padding];
                } else {
                    self.field[last_slot] |= ~trailing_masks[padding];
                }
            }
            return 3;
        }

        pub fn deinit(self: *Self) void {
            free(self.field);
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 0;
            var idx: usize = 0;

            for (self.field) |value| {
                count += if (PRIME == 1) @popCount(T, value) else @bitSizeOf(T) - @popCount(T, value);
            }

            return count;
        }

        const EMPTY:T = if (PRIME == 1) @as(T, 0) else @as(T, 0) -% @as(T, 1);

        const shift_bits = @floatToInt(comptime_int, @log2(@intToFloat(f64, @bitSizeOf(T))));
        const shift_t = @Type(std.builtin.TypeInfo{.Int = .{.signedness = .unsigned, .bits = shift_bits}});

        pub fn findNextFactor(self: *Self, factor: usize) usize {
            comptime const trailing_masks = make_trailing_masks();

            const field = self.field;
            // start on the next odd number after the factor.
            const num = (factor + 2) / 2;
            // determine the location of
            var slot = num / @bitSizeOf(T);

            // mask out all values prior to the checked prime.
            var slot_primes = if (PRIME == 1) field[slot] & trailing_masks[num % @bitSizeOf(T)]
                              else field[slot] | trailing_masks[num % @bitSizeOf(T)];

            // if there are no primes in this slot, continue scanning until
            // a prime is found.
            if (slot_primes == EMPTY) {
                for (field[slot + 1 ..]) |s| {
                    slot += 1;
                    slot_primes = s;
                    if (s != EMPTY) {
                        break;
                    }
                }
            }

            const empty_bits = if (PRIME == 1) @ctz(T, slot_primes) else @ctz(T, ~slot_primes);

            return ((slot * @bitSizeOf(T) + empty_bits) * 2) + 1;
        }

        // creates a compile-time lookup table for masks which will blot out values
        // before a given position in a single slot.
        fn make_trailing_masks() comptime [@bitSizeOf(T)]T {
            var masks = std.mem.zeroes([@bitSizeOf(T)]T);
            for (masks) |*value, index| {
                value.* = @as(T, 0) -% (@as(T, 1) << @intCast(shift_t, index));
                if (PRIME == 0) {value.* = ~value.*; }
            }
            return masks;
        }

        pub fn runFactor(self: *Self, factor: usize) void {
            comptime const masks = make_bit_masks();
            const field = self.field;
            const limit = self.field_size;

            var num = (factor * factor) / 2;
            while (num < limit) : (num += factor) {
                var slot = num / @bitSizeOf(T);
                if (PRIME == 1) {
                    field[slot] &= masks[num % @bitSizeOf(T)];
                } else {
                    field[slot] |= masks[num % @bitSizeOf(T)];
                }
            }
        }

        // creates a compile-time lookup table for bit masks for each
        // slot.  There should be one mask for each bit-positional index
        // e.g. for u8, there are 8 u8 masks; for u64, there are 64 u64 masks.
        fn make_bit_masks() comptime [@bitSizeOf(T)]T {
            var masks = std.mem.zeroes([@bitSizeOf(T)]T);
            for (masks) |*value, bit_index| {
                value.* = (@as(T, 1) << @intCast(shift_t, bit_index));
                if (PRIME == 1) {
                    value.* ^= (@as(T, 0) -% @as(T, 1));
                }
            }
            return masks;
        }

        // testing convenience
        pub fn dump(self: *Self) void {
            std.debug.print("\n{b}\n", .{self.field});
        }
    };
}

pub fn BitSieveClassic(opts: anytype) type {
    // store the wheel type
    const Wheel: ?type = null;

    return struct {
        // values
        const T = opts.T;
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
        pub const algo = "base";
        pub const bits = 1;

        // storage
        field: []T,
        field_size: usize,
        needs_pad: bool,

        usingnamespace opts.allocator;

        // member functions
        pub fn init(field_size: usize) !Self {
            const needs_pad: bool = ((field_size >> 1) & bit_mask) != 0;
            const padding: usize = (if (needs_pad) 1 else 0);
            const field_units: usize = (field_size >> (bit_shift + 1)) + padding;

            // allocates an array of data.
            var field = try calloc_aligned(T, 0xFF, field_units, std.mem.page_size);
            return Self{ .field = field[0..], .field_size = field_size, .needs_pad = needs_pad };
        }

        pub fn deinit(self: *Self) void {
            free(self.field);
        }

        pub fn reset(self: *Self) usize {
            const finalmask: T = (one << @intCast(smallint_t, (self.field_size >> 1) & bit_mask)) - 1;

            var starting_point: usize = 0;

            fill_memory(self.field);
            starting_point = 3;

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
    pub fn init(field_size: usize) !Self {
        const bit_size = field_size / 2;
        const field_size_bytes = (bit_size + @bitSizeOf(u8) - 1) / @bitSizeOf(u8);

        // allocates an array of data.
        // Overallocate so we can do unchecked writes at the end.
        const field = try a.alignedAlloc(u8, 8, field_size_bytes + @sizeOf(PatternChunk) - 1);
        return Self{ .field = field, .allocator = allocator, .field_size = field_size };
    }

    pub fn deinit(self: *Self) void {
        a.free(self.field);
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