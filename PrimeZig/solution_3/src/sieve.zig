const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn Sieve(comptime T: type, sieve_size: comptime_int) type {
    return struct {
        // values
        pub const Type = T;
        pub const size = sieve_size;
        pub const TRUE = if (T == bool) true else 1;
        pub const FALSE = if (T == bool) false else 0;

        const Self = @This();
        const field_size = sieve_size >> 1;

        // storage
        field: *[field_size]T align(std.mem.page_size),
        allocator: *Allocator,

        // member functions

        pub fn create(allocator: *Allocator) !Self {
            // allocates an array of data.
            var field: *[field_size]Type = try allocator.create([field_size]Type);
            return Self{.field = field, .allocator = allocator};
        }

        pub fn destroy(self: *Self) void {
            self.allocator.destroy(self.field);
        }

        pub fn reset(self: *Self) void {
            for (self.field.*) |*number| { number.* = TRUE; }
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 0;
            var idx: usize = 0;

            for (self.field.*) | value | {
                if (T == bool) {
                    count += @boolToInt(value);
                } else {
                    count += value;
                }
            }

            return count;
        }

        pub fn findNextFactor(self: *Self, factor: usize) usize {
            const field = self.field;
            var num = factor + 2;
            while (num < field_size) : (num += 2) {
                if (Type == bool) {
                    if (field.*[num >> 1]) { return num; }
                } else {
                    if (field.*[num >> 1] == TRUE) { return num; }
                }
            }
            return num;
        }

        pub fn runFactor(self: *Self, factor: usize) void {
            const field = self.field;
            var num = (factor * factor) >> 1;
            while (num < field_size) : (num += factor) {
                field.*[num] = FALSE;
            }
        }
    };
}

pub fn BitSieve(comptime T: type, sieve_size: comptime_int) type {
    return struct {
        // values
        pub const Type = T;
        pub const size = sieve_size;
        const bit_width = @bitSizeOf(T);
        const bit_shift = @floatToInt(u6, @log2(@intToFloat(f64, bit_width)));

        const Self = @This();
        const field_size = sieve_size >> 1;
        const needs_pad = (field_size % bit_width) != 0;
        const field_units = @divTrunc(field_size, bit_width) + if (needs_pad) 1 else 0;

        // storage
        field: *[field_units]T align(std.mem.page_size),
        allocator: *Allocator,

        // member functions

        pub fn create(allocator: *Allocator) !Self {
            // allocates an array of data.
            var field: *[field_units]Type = try allocator.create([field_units]Type);
            return Self{.field = field, .allocator = allocator};
        }

        pub fn destroy(self: *Self) void {
            self.allocator.destroy(self.field);
        }

        pub fn reset(self: *Self) void {
            comptime const finalmask = (1 << (field_size % bit_width)) - 1;
            for (self.field.*) |*number| { number.* = @as(Type, 0) -% 1; }
            self.field.*[field_units - 1] = finalmask;
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 0;
            var idx: usize = 0;

            for (self.field.*) | value | {
                count += @popCount(Type, value);
            }

            return count;
        }

        // a mask that is usable to obtain the residual (remainder) from the
        // bitshift operation.  This is the bit position within the datastructure
        // that represents the primeness of the requested number.
        const residue_mask = (1 << bit_shift) - 1;

        pub fn findNextFactor(self: *Self, factor: usize) usize {
            comptime const masks = trailing_masks();
            const field = self.field;
            var num = factor + 2;
            var index = num >> bit_shift;
            var slot = field.*[index] & masks[num & residue_mask];
            while ((slot != 0) and index <= field.len) : (index += 1) {
                slot = field.*[index];
            }
            return (index << bit_shift) + @ctz(Type, slot);
        }

        pub fn runFactor(self: *Self, factor: usize) void {
            comptime const masks = bit_masks();
            const field = self.field;
            var num = (factor * factor) >> 1;
            while (num < field_size) : (num += factor) {
                var index = factor >> bit_shift;
                field.*[index] |= masks[num & residue_mask];
            }
        }

        const shift_t = switch (Type) {u8 => u3, u16 => u4, u32 => u5, u64 => u6, else => unreachable};

        fn trailing_masks() comptime [bit_width]Type {
            var masks = std.mem.zeroes([bit_width]Type);
            for (masks) | *value, index | {
                value.* = @as(Type, 0) -% (@as(Type, 1) << @intCast(shift_t, index));
            }
            return masks;
        }

        fn bit_masks() comptime [bit_width]Type{
            var masks = std.mem.zeroes([bit_width]Type);
            for (masks) | *value, index | {
                value.* = (@as(Type, 1) << @intCast(shift_t, index));
            }
            return masks;
        }
    };
}

pub fn SingleThreadedRunner(comptime SieveType: type, comptime _opt: anytype) type {
    const Type = SieveType.Type;
    const sieve_size = SieveType.size;
    const field_size = sieve_size >> 1;

    return struct{
        const Self = @This();
        sieve: SieveType = undefined,

        pub fn init(self: *Self, allocator: *Allocator) !void {
            self.sieve = try SieveType.create(allocator);
        }

        pub fn deinit(self: *Self) void { self.sieve.destroy(); }

        pub fn reset(self: *Self) void { self.sieve.reset(); }

        pub fn run(self: *Self, passes: *u64) void {
            @setAlignStack(256);
            comptime const stop = @floatToInt(usize, @sqrt(@intToFloat(f64, sieve_size)));
            var factor: usize = 3;

            while (factor <= stop) : (factor = self.sieve.findNextFactor(factor)) {
                self.sieve.runFactor(factor);
            }
            // increment the number of passes.
            passes.* += 1;
        }
    };
}