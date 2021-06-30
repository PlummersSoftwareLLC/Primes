const std = @import("std");
const Allocator = std.mem.Allocator;
const PreGenerated = @import("pregen.zig").PreGenerated;

const SieveOpts = struct { pregen: ?comptime_int = null };

pub fn IntSieve(comptime T: type, sieve_size: comptime_int, opts: SieveOpts) type {
    return struct {
        // values
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
            var field: *[field_size]T = try allocator.create([field_size]T);
            return Self{ .field = field, .allocator = allocator };
        }

        pub fn destroy(self: *Self) void {
            self.allocator.destroy(self.field);
        }

        pub fn reset(self: *Self) usize {
            if (opts.pregen) |pregen| {
                const Lookup = PreGenerated(pregen, .byte);
                var index: usize = 0;
                for (self.field.*) |*slot| {
                    slot.* = Lookup.template[index];
                    index += 1;
                    if (index == Lookup.template.len) {
                        index = 0;
                    }
                }

                inline for (Lookup.primes) |prime| {
                    self.field.*[prime >> 1] = 1;
                }

                return Lookup.first_prime;
            } else {
                for (self.field.*) |*slot| {
                    slot.* = TRUE;
                }
                return 3;
            }
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 0;
            var idx: usize = 0;

            for (self.field.*) |value| {
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
                if (T == bool) {
                    if (field.*[num >> 1]) {
                        return num;
                    }
                } else {
                    if (field.*[num >> 1] == TRUE) {
                        return num;
                    }
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

        pub const lookup_name = if (opts.pregen) |pregen| ("-" ++ PreGenerated(pregen, .byte).name) else "";
        pub const name = "sieve-" ++ @typeName(T) ++ lookup_name;
    };
}

pub fn BitSieve(comptime T: type, sieve_size: comptime_int, opts: SieveOpts) type {
    return struct {
        // values
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
            var field: *[field_units]T = try allocator.create([field_units]T);
            return Self{ .field = field, .allocator = allocator };
        }

        pub fn destroy(self: *Self) void {
            self.allocator.destroy(self.field);
        }

        pub fn reset(self: *Self) usize {
            comptime const finalmask = (1 << (field_size % bit_width)) - 1;
            var starting_point: usize = 0;

            if (opts.pregen) |pregen| {
                const Lookup = PreGenerated(pregen, .bit);
                var index: usize = 0;

                copyBits(self.field, &Lookup.template);

                // make the primes prime
                inline for (Lookup.primes) |prime| {
                    const shift = switch (T) {
                        u8 => 3,
                        u16 => 4,
                        u32 => 5,
                        u64 => 6,
                        else => unreachable,
                    };
                    const mask = (@as(T, 1) << shift) - @as(T, 1);
                    const prime_index = prime >> 1;
                    const byte_index = prime_index >> shift;
                    const bit_index = prime_index & mask;
                    const bit_flip = @as(T, 1) << bit_index;

                    self.field.*[byte_index] |= bit_flip;
                }

                starting_point = Lookup.first_prime;
            } else {
                for (self.field.*) |*number| {
                    number.* = @as(T, 0) -% 1;
                }
                starting_point = 3;
            }

            if (needs_pad) {
                self.field.*[field_units - 1] &= finalmask;
            }
            return starting_point;
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 0;
            var idx: usize = 0;

            for (self.field.*) |value| {
                count += @popCount(T, value);
            }

            return count;
        }

        const src_units = if (opts.pregen) |pregen| PreGenerated(pregen, .bit).template.len else 1;
        // TODO: move this to PreGenerated?

        fn copyBits(bit_field: *[field_units]T, src: *const *[src_units]u8) void {
            if (T == u8) {
                const dest_len = field_units * @sizeOf(T);
                var start: usize = 0;
                var u8_field = @ptrCast([*] u8, bit_field);
                while (start + src_units < dest_len) : (start += src_units) {
                    @memcpy(u8_field, @ptrCast([*] const u8, src), @sizeOf(T));
                }
                @memcpy(u8_field, @ptrCast([*] const u8, src), dest_len - start);
            } else {
                var src_index: usize = 0;
                for (bit_field.*) |*dest| {
                    dest.* = findBits(src, &src_index);
                }
            }
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
            var num = (factor + 2) >> 1;
            var index = num >> bit_shift;
            var slot = field.*[index] & masks[num & residue_mask];
            if (slot == 0) {
                for (field.*[index + 1 ..]) |s| {
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
            var num = (factor * factor) >> 1;
            while (num < field_size) : (num += factor) {
                var index = num >> bit_shift;
                field.*[index] &= masks[num & residue_mask];
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

        pub const lookup_name = if (opts.pregen) |pregen| ("-" ++ PreGenerated(pregen, .bit).name) else "";
        pub const name = "bitSieve-" ++ @typeName(T) ++ lookup_name;
    };
}
