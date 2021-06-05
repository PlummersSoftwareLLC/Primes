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

        pub fn init(allocator: *Allocator) !Self {
            // allocates an array of data.
            var field: *[field_size]Type = try allocator.create([field_size]Type);
            return Self{.field = field, .allocator = allocator};
        }

        pub fn deinit(self: *Self) void {
            self.allocator.destroy(self.field);
        }

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
    const Type = SieveType.Type;
    const sieve_size = SieveType.size;
    const field_size = sieve_size >> 1;

    return struct{
        const Self = @This();
        sieve: SieveType,

        pub fn init(allocator: *Allocator) !Self {
            return Self{.sieve = try SieveType.init(allocator)};
        }

        pub fn deinit(self: *Self) void { self.sieve.deinit(); }

        pub fn reset(self: *Self) void { self.sieve.reset(); }

        pub fn run(self: *Self) void {
            @setAlignStack(256);
            comptime const stop = @floatToInt(usize, @sqrt(@intToFloat(f64, sieve_size)));
            var factor: usize = 3;
            var field = self.sieve.field;

            while (factor <= stop) : (factor += 2) {
                var num: usize = factor;
                factorSet: while (num < field_size) : (num += 2) {
                    if (field.*[num >> 1] == SieveType.TRUE) {
                        factor = num;
                        break :factorSet;
                    }
                }

                num = (factor * factor) >> 1;
                while (num < field_size) : (num += factor) {
                    field.*[num] = SieveType.FALSE;
                }
            }
        }
    };
}