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
        field: [sieve_size]Type align(std.mem.page_size),

        const Self = @This();

        pub fn init(field: [sieve_size]Type) Self {
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
                factorSet: while (num < sieve_size) : (num += 2) {
                    if (self.field[num] == true_val) {
                        factor = num;
                        break :factorSet;
                    }
                }

                num = factor * factor;
                while (num < sieve_size) : (num += factor * 2) {
                    self.field[num] = false_val;
                }
            }
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 1;
            var idx: usize = 3;

            while (idx < sieve_size) : (idx += 2) {
                count += if (self.field[idx] == true_val) @as(usize, 1) else @as(usize, 0);
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
        const BitSet = std.StaticBitSet(sieve_size);

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
                factorSet: while (num < sieve_size) : (num += 2) {
                    if (!self.field.isSet(num)) {
                        factor = num;
                        break :factorSet;
                    }
                }

                num = factor * factor;
                while (num < sieve_size) : (num += factor * 2) {
                    self.field.set(num);
                }
            }
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 1;
            var idx: usize = 3;

            while (idx < sieve_size) : (idx += 2) {
                count += if (!self.field.isSet(idx)) @as(usize, 1) else @as(usize, 0);
            }

            return count;
        }
    };
}

pub fn UnrolledSieve(
    comptime Type: type,
    comptime true_val: Type,
    comptime false_val: Type,
    sieve_size: comptime_int,
) type {
    return struct {
        field: [sieve_size]Type align(std.mem.page_size),

        const Self = @This();
        const unroll_count = 16;

        pub fn init(field: [sieve_size]Type) Self {
            return .{
                .field = field,
            };
        }

        pub fn run(self: *Self) void {
            @setAlignStack(256);
            comptime const q = @floatToInt(usize, @sqrt(@intToFloat(f64, sieve_size)));
            const iterations = (q - 3) / 2 + (q - 3) % 2;
            const chuncks = iterations / unroll_count;
            const remaining_iters = iterations % unroll_count;
            var factor: usize = 3;

            {var i: usize = 0; while (i < remaining_iters) : ({i += 1; factor += 2;}) {
                var num: usize = factor;
                factorSet: while (num < sieve_size) : (num += 2) {
                    if (self.field[num] == true_val) {
                        factor = num;
                        break :factorSet;
                    }
                }

                num = factor * factor;
                while (num < sieve_size) : (num += factor * 2) {
                    self.field[num] = false_val;
                }
            }}
            {var i: usize = 0; while (i < chuncks) : (i += 1) {
                {comptime var j: usize = 0; inline while (j < unroll_count) : ({j += 1; factor += 2;}) {
                    var num: usize = factor;
                    factorSet: while (num < sieve_size) : (num += 2) {
                        if (self.field[num] == true_val) {
                            factor = num;
                            break :factorSet;
                        }
                    }

                    num = factor * factor;
                    while (num < sieve_size) : (num += factor * 2) {
                        self.field[num] = false_val;
                    }
                }}
            }}
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 1;
            var idx: usize = 3;

            while (idx < sieve_size) : (idx += 2) {
                count += if (self.field[idx] == true_val) @as(usize, 1) else @as(usize, 0);
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

        var field = [_]bool{true} ** size;
        var sieve = Sieve(bool, true, false, size).init(field);

        sieve.run();
        std.testing.expectEqual(@as(usize, count), sieve.primeCount());
    }
}

test "Test unrolled byte sieve" {
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

        var field = [_]bool{true} ** size;
        var sieve = UnrolledSieve(bool, true, false, size).init(field);

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
