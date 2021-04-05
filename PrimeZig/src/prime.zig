const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

/// Interface for the prime numbers array.
/// Used to enable switching between different
/// types of primitives/arrays easier.
pub const PrimeField = struct {
    checkFn: fn (self: *Self, idx: usize) bool,
    storeFn: fn (self: *Self, idx: usize, val: bool) void,

    const Self = @This();

    pub fn check(self: *Self, idx: usize) bool {
        return self.checkFn(self, idx);
    }

    pub fn store(self: *Self, idx: usize, val: bool) void {
        self.storeFn(self, idx, val);
    }
};

/// A PrimeField that is backed by comptime configurable type ArrayList.
/// This enables use of any type you like, just specify the values that
/// will stand in for true and false.
pub fn ArrayListField(
    comptime Type: type,
    comptime trueVal: Type,
    comptime falseVal: Type,
) type {
    return struct {
        bits: ArrayList(Type),
        field: PrimeField,

        const Self = @This();

        pub fn init(allocator: *Allocator, size: usize) !Self {
            var self = Self{
                .bits = try ArrayList(Type).initCapacity(allocator, size),
                .field = .{
                    .checkFn = check,
                    .storeFn = store,
                },
            };
            try self.bits.appendNTimes(trueVal, size);
            return self;
        }

        pub fn deinit(self: *Self) void {
            self.bits.deinit();
        }

        fn check(field: *PrimeField, idx: usize) bool {
            const self = @fieldParentPtr(Self, "field", field);
            return self.bits.items[idx] == trueVal;
        }

        fn store(field: *PrimeField, idx: usize, val: bool) void {
            const self = @fieldParentPtr(Self, "field", field);
            self.bits.items[idx] = if (val) trueVal else falseVal;
        }
    };
}

/// Prime number sieve.
pub const Sieve = struct {
    sieveSize: usize,
    field: *PrimeField,

    const Self = @This();

    pub fn init(field: *PrimeField, size: usize) Self {
        return .{
            .sieveSize = size,
            .field = field,
        };
    }

    pub fn run(self: *Self) void {
        var factor: usize = 3;
        const q = @floatToInt(usize, @sqrt(@intToFloat(f64, self.sieveSize)));

        while (factor <= q) : (factor += 2) {
            var num: usize = factor;
            factorSet: while (num < self.sieveSize) : (num += 2) {
                if (self.field.check(num)) {
                    factor = num;
                    break :factorSet;
                }
            }

            num = factor * factor;
            while (num < self.sieveSize) : (num += factor * 2) {
                self.field.store(num, false);
            }
        }
    }

    pub fn primeCount(self: *Self) usize {
        var count: usize = 1;
        var idx: usize = 3;

        while (idx < self.sieveSize) : (idx += 2) {
            count += if (self.field.check(idx)) @as(usize, 1) else @as(usize, 0);
        }

        return count;
    }
};

const expectEqual = std.testing.expectEqual;
const test_allocator = std.testing.allocator;

test "count primes up to 10" {
    const size = 10;
    var field = try ArrayListField(bool, true, false).init(test_allocator, size);
    defer field.deinit();

    var sieve = Sieve.init(&field.field, size);
    sieve.run();

    expectEqual(@as(usize, 4), sieve.primeCount());
}

test "count primes up to 100" {
    const size = 100;
    var field = try ArrayListField(bool, true, false).init(test_allocator, size);
    defer field.deinit();

    var sieve = Sieve.init(&field.field, size);
    sieve.run();

    expectEqual(@as(usize, 25), sieve.primeCount());
}

test "count primes up to 1000" {
    const size = 1_000;
    var field = try ArrayListField(bool, true, false).init(test_allocator, size);
    defer field.deinit();

    var sieve = Sieve.init(&field.field, size);
    sieve.run();

    expectEqual(@as(usize, 168), sieve.primeCount());
}

test "count primes up to 10000" {
    const size = 10_000;
    var field = try ArrayListField(bool, true, false).init(test_allocator, size);
    defer field.deinit();

    var sieve = Sieve.init(&field.field, size);
    sieve.run();

    expectEqual(@as(usize, 1_229), sieve.primeCount());
}

test "count primes up to 100000" {
    const size = 100_000;
    var field = try ArrayListField(bool, true, false).init(test_allocator, size);
    defer field.deinit();

    var sieve = Sieve.init(&field.field, size);
    sieve.run();

    expectEqual(@as(usize, 9_592), sieve.primeCount());
}

test "count primes up to 1000000" {
    const size = 1_000_000;
    var field = try ArrayListField(bool, true, false).init(test_allocator, size);
    defer field.deinit();

    var sieve = Sieve.init(&field.field, size);
    sieve.run();

    expectEqual(@as(usize, 78_498), sieve.primeCount());
}

test "count primes up to 10000000" {
    const size = 10_000_000;
    var field = try ArrayListField(bool, true, false).init(test_allocator, size);
    defer field.deinit();

    var sieve = Sieve.init(&field.field, size);
    sieve.run();

    expectEqual(@as(usize, 664_579), sieve.primeCount());
}

test "count primes up to 100000000" {
    const size = 100_000_000;
    var field = try ArrayListField(bool, true, false).init(test_allocator, size);
    defer field.deinit();

    var sieve = Sieve.init(&field.field, size);
    sieve.run();

    expectEqual(@as(usize, 5_761_455), sieve.primeCount());
}

test "count primes up to 1000000000" {
    const size = 1_000_000_000;
    var field = try ArrayListField(bool, true, false).init(test_allocator, size);
    defer field.deinit();

    var sieve = Sieve.init(&field.field, size);
    sieve.run();

    expectEqual(@as(usize, 50_847_534), sieve.primeCount());
}

test "count primes up to 10000000000" {
    const size = 10_000_000_000;
    var field = try ArrayListField(bool, true, false).init(test_allocator, size);
    defer field.deinit();

    var sieve = Sieve.init(&field.field, size);
    sieve.run();

    expectEqual(@as(usize, 455_052_511), sieve.primeCount());
}
