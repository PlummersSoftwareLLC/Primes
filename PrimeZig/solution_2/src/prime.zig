const std = @import("std");

/// Prime number sieve. Can be configured with any desired time
/// at compile time. So that switching between them would be easy.
pub fn Sieve(
    comptime Type: type,
    comptime true_val: Type,
    comptime false_val: Type,
    sieve_size: comptime_int,
) type {
    return struct {
        const Self = @This();
        const field_size = sieve_size >> 1;

        field: [field_size]Type align(std.mem.page_size),

        pub fn init(field: [field_size]Type) Self {
            return .{
                .field = field,
            };
        }

        pub fn run(self: *Self) void {
            @setAlignStack(256);
            comptime const q = @floatToInt(usize, @sqrt(@intToFloat(f64, sieve_size)));
            var factor: usize = 3;

            while (factor <= q) : (factor += 2) {
                var num: usize = factor;
                factorSet: while (num < field_size) : (num += 2) {
                    if (self.field[num >> 1] == true_val) {
                        factor = num;
                        break :factorSet;
                    }
                }

                num = (factor * factor) >> 1;
                while (num < field_size) : (num += factor) {
                    self.field[num] = false_val;
                }
            }
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 0;
            var idx: usize = 0;

            while (idx < field_size) : (idx += 1) {
                count += @boolToInt(self.field[idx] == true_val);
            }

            return count;
        }
    };
}

pub fn NewSieve(
    comptime Type: type,
    comptime true_val: Type,
    comptime false_val: Type,
    sieve_size: comptime_int,
) type {
    return struct {
        const Self = @This();
        const field_size = sieve_size >> 1;
        const steps = steps_blk: {
            @setEvalBranchQuota(~@as(u32, 0)); // no limits for me
            const divisors = [_]usize{2, 3, 5, 7, 11, 13};
            const lcm = lcm_blk: {
                comptime var lcm_acc = 1;
                inline for (divisors) |divisor| {
                    lcm_acc *= divisor;
                }
                break :lcm_blk lcm_acc;
            };
            comptime var steps_acc = [_]u8{undefined} ** (lcm >> 1);
            comptime var step_count = 0;
            comptime var step_size = 1;
            comptime var i = 3;
            inline while (i <= lcm + 2) : (i += 2) {
                inline for (divisors) | div | { // for some reason this breaks if not inline_for, maybe compiler bug?
                    if (i % div == 0) {
                        step_size += 1;
                        break;
                    } 
                } else {
                    steps_acc[step_count] = step_size;
                    step_size = 1;
                    step_count += 1;
                }
            }
            comptime var steps_arr: [step_count]u8 align(std.mem.page_size) = steps_acc[0..step_count].*;
            break :steps_blk steps_arr;
        };

        field: [field_size]Type align(std.mem.page_size),

        pub fn init(field: [field_size]Type) Self {
            return .{
                .field = field,
            };
        }

        pub fn run(self: *Self) void {
            @setAlignStack(256);
            comptime const q = @floatToInt(usize, @sqrt(@intToFloat(f64, sieve_size)));
            var factor: usize = 3;

            while (factor <= q) : (factor += 2) {
                var num: usize = factor;
                factorSet: while (num < field_size) : (num += 2) {
                    if (self.field[num >> 1] == true_val) {
                        factor = num;
                        break :factorSet;
                    }
                }

                num = (factor * factor) >> 1;
                while (num < field_size) : (num += factor) {
                    self.field[num] = false_val;
                }
            }
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 0;
            var idx: usize = 0;

            while (idx < field_size) : (idx += 1) {
                count += @boolToInt(self.field[idx] == true_val);
            }

            return count;
        }
    };
}

pub fn BitSieve( // slightly less performant but uses 8x less memory
    comptime sieve_size: comptime_int,
) type {
    return struct {
        const Self = @This();
        const field_size = sieve_size >> 1;
        const BitSet = std.StaticBitSet(field_size);

        field: BitSet align(std.mem.page_size),

        pub fn init() Self {
            return .{
                .field = BitSet.initEmpty(),
            };
        }

        pub fn run(self: *Self) void {
            @setAlignStack(256);
            comptime const q = @floatToInt(usize, @sqrt(@intToFloat(f64, sieve_size)));
            var factor: usize = 3;

            while (factor <= q) : (factor += 2) {
                var num: usize = factor;
                factorSet: while (num < field_size) : (num += 2) {
                    if (!self.field.isSet(num >> 1)) {
                        factor = num;
                        break :factorSet;
                    }
                }

                num = (factor * factor) >> 1;
                while (num < field_size) : (num += factor) {
                    self.field.set(num);
                }
            }
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 0;
            var idx: usize = 0;

            while (idx < field_size) : (idx += 1) {
                count += @boolToInt(!self.field.isSet(idx));
            }

            return count;
        }
    };
}

test "Test byte sieve" {
    const expected_results = .{
        .{             10,         4 },
        .{            100,        25 },
        .{          1_000,       168 },
        .{         10_000,      1229 },
        .{        100_000,      9592 },
        .{      1_000_000,     78498 },
        // Uncommenting the following tests make my compiler crash ¯\_(ツ)_/¯ Probably cause it's allocating huge memory sizes at compile time
        // .{     10_000_000,    664579 },
        // .{    100_000_000,   5761455 },
        // .{  1_000_000_000,  50847534 },
        // .{ 10_000_000_000, 455052511 },
    };

    inline for (expected_results) |result| {
        const size = result[0];
        const count = result[1];

        var field = [_]bool{true} ** (size >> 1);
        var sieve = Sieve(bool, true, false, size).init(field);

        sieve.run();
        std.testing.expectEqual(@as(usize, count), sieve.primeCount());
    }
}

test "Test new byte sieve" {
    const expected_results = .{
        .{             10,         4 },
        .{            100,        25 },
        .{          1_000,       168 },
        .{         10_000,      1229 },
        .{        100_000,      9592 },
        .{      1_000_000,     78498 },
        // Uncommenting the following tests make my compiler crash ¯\_(ツ)_/¯ Probably cause it's allocating huge memory sizes at compile time
        // .{     10_000_000,    664579 },
        // .{    100_000_000,   5761455 },
        // .{  1_000_000_000,  50847534 },
        // .{ 10_000_000_000, 455052511 },
    };

    inline for (expected_results) |result| {
        const size = result[0];
        const count = result[1];

        var field = [_]bool{true} ** (size >> 1);
        var sieve = NewSieve(bool, true, false, size).init(field);

        sieve.run();
        std.testing.expectEqual(@as(usize, count), sieve.primeCount());
    }
}

test "Test bit sieve" {
    const expected_results = .{
        .{             10,         4 },
        .{            100,        25 },
        .{          1_000,       168 },
        .{         10_000,      1229 },
        .{        100_000,      9592 },
        .{      1_000_000,     78498 },
        .{     10_000_000,    664579 },
        // Uncommenting the following tests make my compiler crash ¯\_(ツ)_/¯ Probably cause it's allocating huge memory sizes at compile time
        // .{    100_000_000,   5761455 },
        // .{  1_000_000_000,  50847534 },
        // .{ 10_000_000_000, 455052511 },
    };

    inline for (expected_results) |result| {
        const size = result[0];
        const count = result[1];

        var sieve = BitSieve(size).init();

        sieve.run();
        std.testing.expectEqual(@as(usize, count), sieve.primeCount());
    }
}
