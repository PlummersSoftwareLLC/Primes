const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn Sieve(comptime T: type, sieve_size: comptime_int) type {
    return struct {
        // values
        pub const TRUE = if (T == bool) true else 1;
        pub const FALSE = if (T == bool) false else 0;
        const Self = @This();
        const field_size = sieve_size >> 1;

        // storage
        field: *[field_size]T align(std.mem.page_size),

        // member functions

        pub fn Type() type { return T; }
        pub fn size() comptime_int { return sieve_size; }

        pub fn reset(self: *Self) void {
            for (self.field.*) |*number| {
                number.* = TRUE;
            }
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
    };
}

pub fn SingleThreadedRunner(comptime SieveType: type) type {
    const Type = SieveType.Type();
    const sieve_size = SieveType.size();
    const field_size = sieve_size >> 1;

    return struct{
        pub fn init(allocator: *Allocator) !SieveType {
            // allocate the memory and save as array
            var field: *[field_size]Type = (try allocator.alloc(Type, field_size))[0..field_size];
            return SieveType{.field = field};
        }

        pub fn deinit(allocator: *const Allocator, sieve: *SieveType) void {
            allocator.free(sieve.field.*);
        }

        pub fn run(sieve: *SieveType) void {
            @setAlignStack(256);
            comptime const q = @floatToInt(usize, @sqrt(@intToFloat(f64, sieve_size)));
            var factor: usize = 3;
            var field = sieve.field.*;

            while (factor <= q) : (factor += 2) {
                var num: usize = factor;
                factorSet: while (num < field_size) : (num += 2) {
                    if (field[num >> 1] == SieveType.TRUE) {
                        factor = num;
                        break :factorSet;
                    }
                }

                num = (factor * factor) >> 1;
                while (num < field_size) : (num += factor) {
                    field[num] = SieveType.FALSE;
                }
            }
        }
    };
}