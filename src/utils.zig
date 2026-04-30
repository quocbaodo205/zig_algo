const std = @import("std");

/// Compute (base^exponent) mod modu using binary exponentiation (comptime)
pub fn pow_mod_comptime(base: comptime_int, exponent: comptime_int, modu: comptime_int) comptime_int {
    var b = base % modu;
    var res: comptime_int = 1;
    var e = exponent;
    while (e > 0) {
        if (e & 1 == 1) res = res * b % modu;
        b = b * b % modu;
        e >>= 1;
    }
    return res;
}

/// Compute (base^exponent) mod modu using binary exponentiation (runtime)
pub fn pow_mod(base: usize, exponent: usize, modu: usize) usize {
    var b = base % modu;
    var res: usize = 1;
    var e = exponent;
    while (e > 0) {
        if (e & 1 == 1) res = (res * b) % modu;
        b = (b * b) % modu;
        e >>= 1;
    }
    return res;
}

/// Compute (base^exponent) mod modu using binary exponentiation (runtime, uses u128 for intermediate multiplications)
pub fn pow_mod_big(base: u64, exponent: u64, modu: u64) u64 {
    var b = base % modu;
    var res: u64 = 1;
    var e = exponent;
    while (e > 0) {
        if (e & 1 == 1) {
            res = @intCast((@as(u128, res) * @as(u128, b)) % @as(u128, modu));
        }
        b = @intCast((@as(u128, b) * @as(u128, b)) % @as(u128, modu));
        e >>= 1;
    }
    return res;
}

/// Compute modular inverse using extended Euclidean algorithm (comptime)
pub fn mod_inverse_comptime(a: comptime_int, m: comptime_int) comptime_int {
    var m_val = m;
    var a_val = a;
    var y: comptime_int = 0;
    var x: comptime_int = 1;
    while (a_val > 1) {
        const q = a_val / m_val;
        const t = m_val;
        m_val = a_val % m_val;
        a_val = t;
        const ty = y;
        y = x - q * y;
        x = ty;
    }
    if (x < 0) x += m;
    return x;
}

/// Compute modular inverse using Fermat's Little Theorem (runtime)
/// Only valid when modu is prime.
/// inv(x) = x^(modu-2) mod modu
pub fn mod_inverse(x: usize, modu: usize) usize {
    return pow_mod(x, modu - 2, modu);
}

/// Compute greatest common divisor of two u64 numbers using Euclidean algorithm
pub fn gcd(a: u64, b: u64) u64 {
    var x = a;
    var y = b;
    while (y != 0) {
        const temp = y;
        y = x % y;
        x = temp;
    }
    return x;
}

/// Compute absolute value of an i64 and return as u64
pub fn abs_i64(x: i64) u64 {
    if (x == std.math.minInt(i64)) {
        return 9223372036854775808;
    }
    return if (x >= 0) @intCast(x) else @intCast(-x);
}

/// Compute absolute value of any integer type, returns unsigned type of same bit width for signed integers
pub fn abs(comptime T: type, x: T) T {
    return switch (@typeInfo(T)) {
        .int => |int_info| switch (int_info.signedness) {
            .unsigned => x,
            .signed => if (x == std.math.minInt(T))
                @as(T, 1) << (int_info.bits - 1)
            else if (x >= 0) @intCast(x) else @intCast(-x),
        },
        else => @compileError("abs only accepts integer types"),
    };
}

/// Find the largest element < x in a sorted slice using binary search
fn findClosestLower(slice: []const usize, x: usize) ?usize {
    if (slice.len == 0) return null;
    if (slice[0] >= x) return null;
    if (slice[slice.len - 1] < x) return slice[slice.len - 1];

    var low: usize = 0;
    var high: usize = slice.len - 1;
    var result: ?usize = null;
    while (low <= high) {
        const mid = low + (high - low) / 2;
        const val = slice[mid];
        if (val < x) {
            result = val;
            low = mid + 1;
        } else {
            high = mid - 1;
        }
    }
    return result;
}

/// Find the smallest element > x in a sorted slice using binary search
fn findClosestUpper(slice: []const usize, x: usize) ?usize {
    if (slice.len == 0) return null;
    if (slice[slice.len - 1] <= x) return null;
    if (slice[0] > x) return slice[0];

    var low: usize = 0;
    var high: usize = slice.len - 1;
    var result: ?usize = null;
    while (low <= high) {
        const mid = low + (high - low) / 2;
        const val = slice[mid];
        if (val > x) {
            result = val;
            high = mid - 1;
        } else {
            low = mid + 1;
        }
    }
    return result;
}

test "pow mod tests" {
    // Test comptime pow_mod
    try std.testing.expect(pow_mod_comptime(2, 10, 998244353) == 1024);
    try std.testing.expect(pow_mod_comptime(3, 5, 7) == 5); // 243 mod 7 = 5

    // Test runtime pow_mod
    try std.testing.expect(pow_mod(2, 10, 998244353) == 1024);
    try std.testing.expect(pow_mod(3, 5, 7) == 5); // 243 mod 7 = 5

    // Test pow_mod_big
    try std.testing.expect(pow_mod_big(2, 10, 998244353) == 1024);
    try std.testing.expect(pow_mod_big(3, 5, 7) == 5); // 243 mod 7 = 5
    const big_prime: u64 = 36028797018963977;
    try std.testing.expect(pow_mod_big(2, 100, big_prime) == pow_mod_comptime(2, 100, big_prime));

    // Test mod_inverse
    const p = 998244353;
    try std.testing.expect((2 * mod_inverse(2, p)) % p == 1);
    try std.testing.expect((5 * mod_inverse(5, p)) % p == 1);

    // Test mod_inverse_comptime
    try std.testing.expect(mod_inverse_comptime(2, p) == mod_inverse(2, p));
    try std.testing.expect(mod_inverse_comptime(5, p) == mod_inverse(5, p));
}

test "gcd tests" {
    try std.testing.expect(gcd(0, 0) == 0);
    try std.testing.expect(gcd(5, 0) == 5);
    try std.testing.expect(gcd(0, 7) == 7);
    try std.testing.expect(gcd(48, 18) == 6);
    try std.testing.expect(gcd(100, 25) == 25);
    try std.testing.expect(gcd(7, 13) == 1); // coprime
}

test "abs_i64 tests" {
    try std.testing.expect(abs_i64(0) == 0);
    try std.testing.expect(abs_i64(5) == 5);
    try std.testing.expect(abs_i64(-5) == 5);
    try std.testing.expect(abs_i64(12345) == 12345);
    try std.testing.expect(abs_i64(-98765) == 98765);
    try std.testing.expect(abs_i64(std.math.minInt(i64)) == 9223372036854775808); // min i64
}

test "abs generic tests" {
    // Test unsigned integers
    try std.testing.expect(abs(u8, 0) == 0);
    try std.testing.expect(abs(u16, 123) == 123);
    try std.testing.expect(abs(u32, 4567) == 4567);
    try std.testing.expect(abs(u64, 89012) == 89012);
    try std.testing.expect(abs(usize, 345678) == 345678);

    // Test signed integers
    try std.testing.expect(abs(i8, 0) == 0);
    try std.testing.expect(abs(i8, 5) == 5);
    try std.testing.expect(abs(i8, -5) == 5);
    try std.testing.expect(abs(i8, std.math.minInt(i8)) == 128);

    try std.testing.expect(abs(i16, 123) == 123);
    try std.testing.expect(abs(i16, -123) == 123);
    try std.testing.expect(abs(i16, std.math.minInt(i16)) == 32768);

    try std.testing.expect(abs(i32, 4567) == 4567);
    try std.testing.expect(abs(i32, -4567) == 4567);
    try std.testing.expect(abs(i32, std.math.minInt(i32)) == 2147483648);

    try std.testing.expect(abs(i64, 89012) == 89012);
    try std.testing.expect(abs(i64, -89012) == 89012);
    try std.testing.expect(abs(i64, std.math.minInt(i64)) == 9223372036854775808);

    try std.testing.expect(abs(isize, 345678) == 345678);
    try std.testing.expect(abs(isize, -345678) == 345678);
}
