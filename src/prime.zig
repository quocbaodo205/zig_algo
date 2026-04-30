const std = @import("std");

/// Compute natural log of x (comptime)
fn comptime_ln(x: comptime_float) comptime_float {
    return @log(x);
}

/// PrimeDS is a generic struct that holds primes up to max_n (comptime)
pub fn PrimeDS(comptime max_n: comptime_int) type {
    // Approximate number of primes up to max_n: ~max_n / ln(max_n), multiply by 2 to be safe
    const approx_primes = if (max_n < 2) 0 else blk: {
        const x = @as(comptime_float, @floatFromInt(max_n));
        const ln_x = comptime_ln(x);
        const est = x / ln_x;
        break :blk @as(comptime_int, @intFromFloat(@ceil(est * 2)));
    };

    return struct {
        const Self = @This();

        pub const Factor = struct {
            prime: usize,
            exponent: usize,
        };

        primes: [approx_primes]usize,
        prime_count: usize,
        is_prime: [max_n + 1]bool,
        factors: [MAX_FACTORS]Factor,
        factor_count: usize,

        // Maximum number of distinct prime factors (for numbers up to 2^64, it's at most 15)
        const MAX_FACTORS = 20;

        /// Initialize PrimeDS using linear sieve (Euler's sieve)
        pub fn init() Self {
            var self = Self{
                .primes = if (approx_primes == 0) [_]usize{} else undefined,
                .prime_count = 0,
                .is_prime = [_]bool{true} ** (max_n + 1),
                .factors = undefined,
                .factor_count = 0,
            };

            if (max_n >= 0) self.is_prime[0] = false;
            if (max_n >= 1) self.is_prime[1] = false;

            if (max_n >= 2) {
                var i: usize = 2;
                while (i <= max_n) : (i += 1) {
                    if (self.is_prime[i]) {
                        self.primes[self.prime_count] = i;
                        self.prime_count += 1;
                    }
                    var j: usize = 0;
                    while (j < self.prime_count) : (j += 1) {
                        const p = self.primes[j];
                        const product = i * p;
                        if (product > max_n) break;
                        self.is_prime[product] = false;
                        if (i % p == 0) break;
                    }
                }
            }

            return self;
        }

        /// Get the primes slice
        pub fn primesSlice(self: *const Self) []const usize {
            return self.primes[0..self.prime_count];
        }

        /// Return the closest prime <= x, or null if none exists
        pub fn closestPrimeLower(self: *const Self, x: usize) ?usize {
            if (x < 2) return null;
            const primes_slice = self.primesSlice();
            if (primes_slice.len == 0) return null;
            if (primes_slice[0] > x) return null;
            if (primes_slice[primes_slice.len - 1] <= x) return primes_slice[primes_slice.len - 1];
            
            var low: usize = 0;
            var high: usize = primes_slice.len - 1;
            var result: ?usize = null;
            while (low <= high) {
                const mid = low + (high - low) / 2;
                const p = primes_slice[mid];
                if (p == x) {
                    return p;
                } else if (p < x) {
                    result = p;
                    low = mid + 1;
                } else {
                    high = mid - 1;
                }
            }
            return result;
        }

        /// Return the closest prime >= x, or null if none exists
        pub fn closestPrimeUpper(self: *const Self, x: usize) ?usize {
            if (x > max_n) return null;
            const primes_slice = self.primesSlice();
            if (primes_slice.len == 0) return null;
            if (primes_slice[primes_slice.len - 1] < x) return null;
            if (primes_slice[0] >= x) return primes_slice[0];
            
            var low: usize = 0;
            var high: usize = primes_slice.len - 1;
            var result: ?usize = null;
            while (low <= high) {
                const mid = low + (high - low) / 2;
                const p = primes_slice[mid];
                if (p == x) {
                    return p;
                } else if (p > x) {
                    result = p;
                    high = mid - 1;
                } else {
                    low = mid + 1;
                }
            }
            return result;
        }

        /// Factorize a number x into its prime factors, returns a slice of Factor
        /// x must be <= max_n
        pub fn factorize(self: *Self, x: usize) []const Factor {
            self.factor_count = 0;
            var n = x;

            for (self.primesSlice()) |p| {
                if (p * p > n) break;
                if (n % p == 0) {
                    var exp: usize = 0;
                    while (n % p == 0) {
                        exp += 1;
                        n /= p;
                    }
                    self.factors[self.factor_count] = Factor{ .prime = p, .exponent = exp };
                    self.factor_count += 1;
                }
            }

            if (n > 1) {
                self.factors[self.factor_count] = Factor{ .prime = n, .exponent = 1 };
                self.factor_count += 1;
            }

            return self.factors[0..self.factor_count];
        }
    };
}

test "PrimeDS(10)" {
    const DS = PrimeDS(10);
    const ds = DS.init();

    try std.testing.expectEqualSlices(usize, &[_]usize{ 2, 3, 5, 7 }, ds.primesSlice());
    try std.testing.expect(!ds.is_prime[0]);
    try std.testing.expect(!ds.is_prime[1]);
    try std.testing.expect(ds.is_prime[2]);
    try std.testing.expect(ds.is_prime[3]);
    try std.testing.expect(!ds.is_prime[4]);
    try std.testing.expect(ds.is_prime[5]);
    try std.testing.expect(!ds.is_prime[6]);
    try std.testing.expect(ds.is_prime[7]);
    try std.testing.expect(!ds.is_prime[8]);
    try std.testing.expect(!ds.is_prime[9]);
    try std.testing.expect(!ds.is_prime[10]);
}

test "PrimeDS(1)" {
    const DS = PrimeDS(1);
    const ds = DS.init();

    try std.testing.expect(ds.primesSlice().len == 0);
}

test "PrimeDS(2)" {
    const DS = PrimeDS(2);
    const ds = DS.init();

    try std.testing.expectEqualSlices(usize, &[_]usize{2}, ds.primesSlice());
    try std.testing.expect(ds.is_prime[2]);
}

test "closestPrimeLower and closestPrimeUpper" {
    const DS = PrimeDS(20);
    const ds = DS.init();

    // Test closestPrimeLower
    try std.testing.expect(ds.closestPrimeLower(10) == 7);
    try std.testing.expect(ds.closestPrimeLower(7) == 7);
    try std.testing.expect(ds.closestPrimeLower(8) == 7);
    try std.testing.expect(ds.closestPrimeLower(2) == 2);
    try std.testing.expect(ds.closestPrimeLower(1) == null);
    try std.testing.expect(ds.closestPrimeLower(25) == 19); // x > max_n, use max_n

    // Test closestPrimeUpper
    try std.testing.expect(ds.closestPrimeUpper(10) == 11);
    try std.testing.expect(ds.closestPrimeUpper(11) == 11);
    try std.testing.expect(ds.closestPrimeUpper(12) == 13);
    try std.testing.expect(ds.closestPrimeUpper(19) == 19);
    try std.testing.expect(ds.closestPrimeUpper(20) == null);
    try std.testing.expect(ds.closestPrimeUpper(1) == 2); // x < 2, start at 2
}

test "factorize" {
    const DS = PrimeDS(100);
    var ds = DS.init(); // need mutable because factorize takes *Self

    // Test factorize 12 = 2^2 * 3^1
    const factors12 = ds.factorize(12);
    try std.testing.expect(factors12.len == 2);
    try std.testing.expect(factors12[0].prime == 2);
    try std.testing.expect(factors12[0].exponent == 2);
    try std.testing.expect(factors12[1].prime == 3);
    try std.testing.expect(factors12[1].exponent == 1);

    // Test factorize 17 (prime)
    const factors17 = ds.factorize(17);
    try std.testing.expect(factors17.len == 1);
    try std.testing.expect(factors17[0].prime == 17);
    try std.testing.expect(factors17[0].exponent == 1);

    // Test factorize 100 = 2^2 * 5^2
    const factors100 = ds.factorize(100);
    try std.testing.expect(factors100.len == 2);
    try std.testing.expect(factors100[0].prime == 2);
    try std.testing.expect(factors100[0].exponent == 2);
    try std.testing.expect(factors100[1].prime == 5);
    try std.testing.expect(factors100[1].exponent == 2);
}
